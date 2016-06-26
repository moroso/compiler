use ir::{Op, OpNode, VarName, RValueElem};
use mc::ast::{UnOpNode, WithIdT, CanHaveId};
use mas::ast::{InstNode, JumpTarget};

use std::collections::{BTreeSet, BTreeMap};

pub struct Inliner;

fn renumber_label(label: usize, label_map: &mut BTreeMap<usize, usize>, label_id: &mut usize) -> usize {
    *label_map.entry(label).or_insert_with(|| { let temp = *label_id; *label_id += 1; temp })
}

fn func_name(func: &Vec<Op>) -> &VarName {
    if let OpNode::Func(ref name, _, _) = func[0].val {
        name
    } else {
        unreachable!()
    }
}

fn is_extern(func: &Vec<Op>) -> bool {
    match func[0].val {
        OpNode::Func(_, _, None) => false,
        OpNode::Func(_, _, _) => true,
        _ => unreachable!(),
    }
}

fn func_name_str(func: &Vec<Op>) -> String {
    format!("{}", func_name(func).base_name())
}

fn func_refs(func: &Vec<Op>) -> (BTreeMap<VarName, usize>, BTreeSet<VarName>,
                                 BTreeSet<String>) {
    // Returns all functions that are called (with call counts), and all functions that
    // are referenced by the given function, and all functions that are referenced within
    // asm.

    let mut calls = BTreeMap::new();
    let mut refs = BTreeSet::new();
    let mut asm_refs = BTreeSet::new();

    for op in func.iter() {
        match op.val {
            OpNode::Call(_, RValueElem::Variable(v), _) => { *calls.entry(v.name).or_insert(0) += 1; },
            OpNode::UnOp(_, _, RValueElem::Variable(v)) => { refs.insert(v.name); }
            OpNode::BinOp(_, _, ref lhs, ref rhs, _) => {
                if let RValueElem::Variable(v) = *lhs {
                    refs.insert(v.name);
                }
                if let RValueElem::Variable(v) = *rhs {
                    refs.insert(v.name);
                }
            }
            OpNode::AsmOp(ref insts) => {
                for packet in insts.iter() {
                    for inst in packet.iter() {
                        match *inst {
                            InstNode::BranchImmInst(_, _, JumpTarget::JumpLabel(ref s)) => {
                                asm_refs.insert(s.clone());
                            }
                            _ => {},
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
                new_calls.extend(calls.get(called_func).clone().unwrap_or(&BTreeSet::new()));
            }
            func_calls.extend(new_calls);

            if func_calls.len() != orig_len {
                changed = true;
            }
        }
    }

    result
}


fn inline_func(parent: &mut Vec<Op>, child: &Vec<Op>, label_id: &mut usize,
               call_counts: &mut BTreeMap<VarName, usize>) {
    // Perform the actual inlining. Inline "child" anywhere it's called in "parent".
    // call_counts will be updated during this process.
    let (child_name, child_args) = if let OpNode::Func(ref name, ref args, _) = child[0].val {
        (name, args)
    } else {
        unreachable!()
    };

    let mut result = vec!();

    for op in parent.into_iter() {
        match op.val {
            OpNode::Call(ref v_opt, RValueElem::Variable(ref funcname), ref vars)
                if funcname.name == *child_name => {
                    // We found a callsite!

                    // We can't use the same label names in the inlined function: if we inline
                    // a function multiple times we'll have collisions in label names.
                    // This map tracks what new names we've assigned to labels.
                    let mut label_rename_map = BTreeMap::new();
                    for (caller_var, callee_var) in vars.iter().zip(child_args.iter()) {
                        result.push(
                            OpNode::UnOp(callee_var.clone(),
                                         UnOpNode::Identity,
                                         RValueElem::Variable(caller_var.clone()))
                                .with_id(child[0].id)
                        );
                    }

                    let end_label = *label_id;
                    *label_id += 1;

                    for child_op in child.iter().skip(1) {
                        match child_op.val {
                            OpNode::Return(Some(ref rve)) => {
                                match *v_opt {
                                    Some(ref v) =>
                                        result.push(OpNode::UnOp(v.clone(),
                                                                 UnOpNode::Identity,
                                                                 rve.clone())
                                                    .with_id(child_op.id)),
                                    None => {},
                                }
                                result.push(OpNode::Goto(end_label, BTreeSet::new())
                                            .with_id(child_op.id));
                            },
                            OpNode::Return(None) => {
                                result.push(OpNode::Goto(end_label, BTreeSet::new())
                                            .with_id(child_op.id));
                            }
                            OpNode::Label(label, ref vars) =>
                                result.push(
                                    OpNode::Label(
                                        renumber_label(label, &mut label_rename_map, label_id),
                                        vars.clone()
                                    ).with_id(child_op.id)
                                ),
                            OpNode::Goto(label, ref vars) =>
                                result.push(
                                    OpNode::Goto(
                                        renumber_label(label, &mut label_rename_map, label_id),
                                        vars.clone()
                                    ).with_id(child_op.id)
                                ),
                            OpNode::CondGoto(invert, ref rve, label, ref vars) =>
                                result.push(
                                    OpNode::CondGoto(
                                        invert,
                                        rve.clone(),
                                        renumber_label(label, &mut label_rename_map, label_id),
                                        vars.clone()
                                    ).with_id(child_op.id)
                                ),
                            OpNode::Call(_, RValueElem::Variable(ref v), _) => {
                                *call_counts.entry(v.name.clone()).or_insert(0) += 1;
                                result.push(child_op.clone());
                            },
                            _ => result.push(child_op.clone()),
                        }
                    }

                    result.push(OpNode::Label(end_label, BTreeSet::new())
                                .with_id(op.id));
                },
            _ => result.push(op.clone()) // TODO: eliminate clone
        }
    }

    *parent = result;
}

fn remove_calls(func: &Vec<Op>, call_counts: &mut BTreeMap<VarName, usize>) {
    // Remove all calls in "func" from the call counts.

    for op in func.iter() {
        match op.val {
            OpNode::Call(_, RValueElem::Variable(ref v), _) => {
                *call_counts.entry(v.name.clone()).or_insert(0) -= 1;
            },
            _ => {}
        }
    }
}

impl Inliner {
    pub fn inline(funcs: &mut Vec<(Vec<Op>)>, mut next_label: usize, verbose: bool) {
        if verbose {
            print!("Starting inline\n");
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
                calls.entry(*this_funcname).or_insert(BTreeSet::new()).insert(f);
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

            if func_name_str(oplist) == "__main".to_string() {
                continue;
            }

            funcs_to_inline.push(idx);
        }

        for func_idx_to_inline in funcs_to_inline {
            let child_func_name = func_name(&funcs[func_idx_to_inline]).clone();
            let child_func_name_str = func_name_str(&funcs[func_idx_to_inline]);
            let count = call_counts.get(&child_func_name).map(|x| *x).unwrap_or(0);
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
        let new_funcs = funcs.drain(..).filter(|x| x.len() > 0).collect();
        *funcs = new_funcs;
    }
}
