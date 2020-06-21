use ir::{Op, OpNode, VarName, RValueElem};
use mc::ast::{UnOpNode, CanHaveId};
use mas::ast::{InstNode, JumpTarget};

use std::collections::{BTreeSet, BTreeMap};

pub struct Inliner;

fn renumber_label(label: usize, label_map: &mut BTreeMap<usize, usize>, label_id: &mut usize) -> usize {
    *label_map.entry(label).or_insert_with(|| { let temp = *label_id; *label_id += 1; temp })
}

fn func_name(func: &[Op]) -> &VarName {
    if let OpNode::Func { ref name, .. } = func[0].val {
        name
    } else {
        unreachable!()
    }
}

fn is_extern(func: &[Op]) -> bool {
    match func[0].val {
        OpNode::Func { abi: None, .. } => false,
        OpNode::Func { .. } => true,
        _ => unreachable!(),
    }
}

fn func_name_str(func: &[Op]) -> String {
    format!("{}", func_name(func).base_name())
}

fn func_refs(func: &[Op]) -> (BTreeMap<VarName, usize>, BTreeSet<VarName>,
                              BTreeSet<String>) {
    // Returns all functions that are called (with call counts), and all functions that
    // are referenced by the given function, and all functions that are referenced within
    // asm.

    let mut calls = BTreeMap::new();
    let mut refs = BTreeSet::new();
    let mut asm_refs = BTreeSet::new();

    for op in func.iter() {
        match op.val {
            OpNode::Call { func: RValueElem::Variable(v), .. } => { *calls.entry(v.name).or_insert(0) += 1; },
            OpNode::UnOp { operand: RValueElem::Variable(v), .. } => { refs.insert(v.name); }
            OpNode::BinOp { ref lhs, ref rhs, .. } => {
                if let RValueElem::Variable(v) = *lhs {
                    refs.insert(v.name);
                }
                if let RValueElem::Variable(v) = *rhs {
                    refs.insert(v.name);
                }
            }
            OpNode::AsmOp { ref insts, .. } => {
                for packet in insts.iter() {
                    for inst in packet.iter() {
                        if let InstNode::BranchImmInst(_, _, JumpTarget::JumpLabel(ref s)) = *inst {
                            asm_refs.insert(s.clone());
                        }
                    }
                }
            }
            _ => {},
        }
    }

    (calls, refs, asm_refs)
}

fn find_transitive_calls(calls: BTreeMap<VarName, BTreeSet<VarName>>
) -> BTreeMap<VarName, BTreeSet<VarName>> {
    // Given a map from callers to callees, generate a map from functions to all
    // functions reachable transitively from them.
    // That is, if
    //   calls = { A: { B }, B: { C } },
    // we'll return
    //   { A: { B, C }, B: { C } }.
    let mut result = BTreeMap::new();
    result.extend(calls.clone());
    let mut changed = true;

    while changed {
        changed = false;

        for func_calls in result.values_mut() {
            let orig_len = func_calls.len();
            let mut new_calls: BTreeSet<VarName> = BTreeSet::new();
            for called_func in func_calls.iter() {
                new_calls.extend(calls.get(called_func).unwrap_or(&BTreeSet::new()));
            }
            func_calls.extend(new_calls);

            if func_calls.len() != orig_len {
                changed = true;
            }
        }
    }

    result
}


fn inline_func(parent: &mut Vec<Op>, child: &[Op], label_id: &mut usize,
               call_counts: &mut BTreeMap<VarName, usize>) {
    // Perform the actual inlining. Inline "child" anywhere it's called in "parent".
    // call_counts will be updated during this process.
    let (child_name, child_args) = if let OpNode::Func { ref name, ref args, .. } = child[0].val {
        (name, args)
    } else {
        unreachable!()
    };

    let mut result = vec!();

    for op in parent.iter() {
        match op.val {
            OpNode::Call { target: ref v_opt, func: RValueElem::Variable(ref funcname), args: ref vars }
                if funcname.name == *child_name => {
                    // We found a callsite!

                    // We can't use the same label names in the inlined function: if we inline
                    // a function multiple times we'll have collisions in label names.
                    // This map tracks what new names we've assigned to labels.
                    let mut label_rename_map = BTreeMap::new();
                    for (caller_var, callee_var) in vars.iter().zip(child_args.iter()) {
                        result.push(
                            OpNode::UnOp {
                                target: *callee_var,
                                op: UnOpNode::Identity,
                                operand: RValueElem::Variable(*caller_var)
                            }.with_id(child[0].id)
                        );
                    }

                    let end_label = *label_id;
                    *label_id += 1;

                    for child_op in child.iter().skip(1) {
                        match child_op.val {
                            OpNode::Return { retval: Some(ref rve) } => {
                                if let Some(v) = *v_opt {
                                    result.push(OpNode::UnOp {
                                        target: v,
                                        op: UnOpNode::Identity,
                                        operand: rve.clone()
                                    }.with_id(child_op.id));
                                }
                                result.push(OpNode::Goto { label_idx: end_label, vars: BTreeSet::new() }
                                            .with_id(child_op.id));
                            },
                            OpNode::Return { retval: None } => {
                                result.push(OpNode::Goto { label_idx: end_label, vars: BTreeSet::new() }
                                            .with_id(child_op.id));
                            }
                            OpNode::Label { label_idx: label, ref vars } =>
                                result.push(
                                    OpNode::Label {
                                        label_idx: renumber_label(label, &mut label_rename_map, label_id),
                                        vars: vars.clone()
                                    }.with_id(child_op.id)
                                ),
                            OpNode::Goto { label_idx: label, ref vars } =>
                                result.push(
                                    OpNode::Goto {
                                        label_idx: renumber_label(label, &mut label_rename_map, label_id),
                                        vars: vars.clone()
                                    }.with_id(child_op.id)
                                ),
                            OpNode::CondGoto { negated, cond: ref rve, label_idx, ref vars } =>
                                result.push(
                                    OpNode::CondGoto {
                                        negated: negated,
                                        cond: rve.clone(),
                                        label_idx: renumber_label(label_idx, &mut label_rename_map, label_id),
                                        vars: vars.clone()
                                    }.with_id(child_op.id)
                                ),
                            OpNode::Call { func: RValueElem::Variable(ref v), .. } => {
                                *call_counts.entry(v.name).or_insert(0) += 1;
                                result.push(child_op.clone());
                            },
                            _ => result.push(child_op.clone()),
                        }
                    }

                    result.push(OpNode::Label { label_idx: end_label, vars: BTreeSet::new() }
                                .with_id(op.id));
                },
            _ => result.push(op.clone()) // TODO: eliminate clone
        }
    }

    *parent = result;
}

fn remove_calls(func: &[Op], call_counts: &mut BTreeMap<VarName, usize>) {
    // Remove all calls in "func" from the call counts.

    for op in func {
        if let OpNode::Call { func: RValueElem::Variable(ref v), .. } = op.val {
            *call_counts.entry(v.name).or_insert(0) -= 1;
        }
    }
}

impl Inliner {
    pub fn inline(funcs: &mut Vec<Vec<Op>>, mut next_label: usize, verbose: bool) {
        if verbose {
            println!("Starting inline");
        }
        let mut call_counts: BTreeMap<VarName, usize> = BTreeMap::new();
        let mut calls: BTreeMap<VarName, BTreeSet<VarName>> = BTreeMap::new();
        let mut refs: BTreeSet<VarName> = BTreeSet::new();
        // TODO: this following is a bit of a hack.
        let mut asm_refs: BTreeSet<String> = BTreeSet::new();

        for oplist in funcs.iter() {
            let this_funcname = func_name(oplist);
            let (thisfunc_calls, thisfunc_refs, thisfunc_asm_refs) = func_refs(oplist);

            for (f, count) in thisfunc_calls {
                *call_counts.entry(f).or_insert(0) += count;
                calls.entry(*this_funcname).or_insert_with(BTreeSet::new).insert(f);
            }

            refs.extend(thisfunc_refs.into_iter());

            for func in thisfunc_asm_refs {
                asm_refs.insert(func);
            }
        }

        if verbose {
            print!("calls: {:?}\n", calls);
            print!("call counts: {:?}\n", call_counts);
        }
        let transitive_calls = find_transitive_calls(calls);
        if verbose {
            print!("transitive calls: {:?}\n", transitive_calls);
            print!("refs: {:?}\n", refs);
            print!("asm refs: {:?}\n", asm_refs);
        }

        let mut funcs_to_inline = vec!();
        for (idx, oplist) in funcs.iter().enumerate() {
            let name = func_name(oplist);

            if transitive_calls.get(name).map(|x| x.contains(name)).unwrap_or(false) {
                // Function can call itself; can't inline.
                continue;
            }

            if is_extern(oplist) {
                // Extern functions can't be inlined.
                continue;
            }

            if let VarName::MangledVariable(_) = *name {
                // This is a special function.
                continue;
            }

            if func_name_str(oplist) == "__main" {
                continue;
            }

            funcs_to_inline.push(idx);
        }

        for func_idx_to_inline in funcs_to_inline {
            let child_func_name = *func_name(&funcs[func_idx_to_inline]);
            let child_func_name_str = func_name_str(&funcs[func_idx_to_inline]);
            let count = call_counts.get(&child_func_name).cloned().unwrap_or(0);
            let len = funcs[func_idx_to_inline].len();

            // This heuristic ensures a few things:
            // - If a function has a single call, and it's inlinable, it will always be inlined
            // - If a function is short enough and it's inlinable, it will always be inlined
            // - Otherwise, there's a tradeoff between function length and call count.
            if len >= 8 && count >= 1 && (len - 8) * (count - 1) > 32 {
                if verbose {
                    print!("Skipping inlining {} with count={}, len={}\n", child_func_name, count, len);
                }
                continue;
            }

            if verbose {
                print!("Inlining {}\n", child_func_name);
            }

            for parent_idx in 0..funcs.len() {
                if func_idx_to_inline == parent_idx { continue; }
                // A dance to appease the borrow checker.
                let mut this_func = vec!();
                ::std::mem::swap(&mut funcs[func_idx_to_inline], &mut this_func);
                inline_func(&mut funcs[parent_idx], &this_func, &mut next_label, &mut call_counts);
                ::std::mem::swap(&mut funcs[func_idx_to_inline], &mut this_func);
            }

            if !refs.contains(&child_func_name) && !asm_refs.contains(&child_func_name_str) {
                // We've inlined it, and there are no references to it.
                // It's safe to remove.
                remove_calls(&funcs[func_idx_to_inline], &mut call_counts);
                funcs[func_idx_to_inline].clear();
            }
        }

        // Remove any functions that were eliminated entirely.
        let new_funcs = funcs.drain(..).filter(|x| !x.is_empty()).collect();
        *funcs = new_funcs;
    }
}
