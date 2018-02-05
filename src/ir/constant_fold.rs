// Constant folding.
use util::IntKind;
use ir::util::subst;
use std::collections::{BTreeSet, BTreeMap};
use mc::ast::*;
use ir::*;
use values::{eval_binop, eval_unop};

pub struct ConstantFolder;

fn set_signedness(l: &LitNode, signed: bool) -> LitNode {
    match *l {
        NumLit(v, kind) =>
            match kind {
                IntKind::SignedInt(w) if !signed => NumLit(v, IntKind::UnsignedInt(w)),
                IntKind::UnsignedInt(w) if signed => NumLit(v, IntKind::SignedInt(w)),
                _ => l.clone(),
            },
        _ => l.clone()
    }
}

fn fold(op: &BinOpNode, e1: &RValueElem, e2: &RValueElem, signed: bool) ->
    Option<LitNode>
{
    let lit1 = match *e1 {
        Constant(ref l) => {
            set_signedness(l, signed)
        },
        _ => return None,
    };
    let lit2 = match *e2 {
        Constant(ref l) => {
            set_signedness(l, signed)
        }
        _ => return None,
    };

    Some(eval_binop(*op, lit1, lit2))
}

fn fold_unary(op: &UnOpNode, e: &RValueElem) -> Option<LitNode> {
    let lit = match *e {
        Constant(ref l) => {
            match *l {
                NumLit(..) |
                BoolLit(..) => l.clone(),
                StringLit(..) => return None,
                _ => panic!(),
            }
        }
        _ => return None,
    };

    eval_unop(*op, lit)
}

fn constant_fold_once<T>(ops: &mut Vec<Op>, vars_to_avoid: &BTreeSet<Var>,
                         globals: &BTreeMap<VarName, T>,
                         verbose: bool) -> bool {
    // Variables to replace with constants.
    let mut changes = vec!();
    let mut immediate_changes = vec!();

    for (pos, op) in ops.iter().enumerate() {
        match op.val {
            OpNode::BinOp { target: ref v, ref op, lhs: ref v1, rhs: ref v2, signed } => {
                match fold(op, v1, v2, signed) {
                    Some(c) => {
                        changes.push((*v,
                                      Constant(c.clone())));
                        immediate_changes.push((pos,
                                                OpNode::UnOp {
                                                    target: *v,
                                                    op: Identity,
                                                    operand: Constant(c)
                                                }));
                    },
                    None => {
                        // TODO: convert multiplications and divisions by
                        // powers of two into shifts.

                        // We can't directly fold, but if we're
                        // applying the operator to an identity
                        // element, we can optimize out the operation.
                        let ident = match *op {
                            TimesOp => Some(1),
                            PlusOp => Some(0),
                            _ => None,
                        };
                        // Some operators have a right identity that is not
                        // a left identity.
                        let rhs_ident = match *op {
                            DivideOp => Some(1),
                            MinusOp => Some(0),
                            _ => None,
                        };
                        // Note: we won't match in *both* of these, because
                        // if that could have happened we instead would have
                        // folded above and never landed here at all.
                        match *v1 {
                            Constant(NumLit(x, _)) if Some(x) == ident => {
                                immediate_changes.push(
                                    (pos,
                                     OpNode::UnOp {
                                         target: *v,
                                         op: Identity,
                                         operand: v2.clone()}
                                    ));
                            },
                            _ => {}
                        }
                        match *v2 {
                            Constant(NumLit(x, _)) if (Some(x) == ident ||
                                                       Some(x) == rhs_ident
                                                       ) => {
                                immediate_changes.push(
                                    (pos,
                                     OpNode::UnOp {
                                         target: *v,
                                         op: Identity,
                                         operand: v1.clone()}
                                    ));
                            }
                            _ => {}
                        }
                    }
                }
            },
            OpNode::UnOp { target: ref v, ref op, operand: ref rv } => {
                if let Some(c) = fold_unary(op, rv) {
                    changes.push((*v,
                                  Constant(c.clone())));
                    if *op != Identity {
                        immediate_changes.push((pos,
                                                OpNode::UnOp {
                                                    target: *v,
                                                    op: Identity,
                                                    operand: Constant(c)}
                        ));
                    }
                }
            }
            // TODO: unary ops, and anything else.
            _ => {},
        }
    }

    let mut changed = false;

    // These are changes we can do unconditionally.
    for (a, b) in immediate_changes {
        ops[a].val = b;
        changed = true;
    }

    for (a, b) in changes {
        if !vars_to_avoid.contains(&a) && !globals.get(&a.name).is_some() {
            if verbose {
                print!("Applying {}->{}\n", a, b);
            }
            subst(ops, &a, &b);
            if verbose {
                for op in ops.iter() {
                    print!("{}", op);
                }
            }
            changed = true;
        }
    }

    changed
}


impl ConstantFolder {

    pub fn fold<T>(ops: &mut Vec<Op>, globals: &BTreeMap<VarName, T>,
                   verbose: bool) {
        // There are certain variables we are prohibited from substituting.
        // Those include any that appear in labels/gotos, as well as any
        // that is dereferenced as part of the left hand side of an assignment.
        let mut vars_to_avoid = BTreeSet::<Var>::new();
        for op in ops.iter() {
            match op.val {
                OpNode::Label { ref vars, .. } |
                OpNode::Goto { ref vars, .. } |
                OpNode::CondGoto { ref vars, .. } => {
                    for var in vars.iter() {
                        vars_to_avoid.insert(*var);
                    }
                },
                // We can't fold anything we take the address of.
                OpNode::UnOp {op: AddrOf, operand: ref rv, .. } => {
                    if let Variable(v) = *rv {
                        vars_to_avoid.insert(v);
                    }
                },
                OpNode::Store { addr: v1, value: v2, .. } |
                OpNode::Load { target: v1, addr: v2, .. } => {
                    vars_to_avoid.insert(v1);
                    vars_to_avoid.insert(v2);
                },
                OpNode::Call { args: ref vars, .. } => {
                    for var in vars.iter() {
                        vars_to_avoid.insert(*var);
                    }
                },
                OpNode::Func { args: ref vars, ref abi, .. } => {
                    if abi.is_some() { return; }
                    for var in vars.iter() {
                        vars_to_avoid.insert(*var);
                    }
                },
                _ => {},
            }
        }
        if verbose {
            print!("avoid: {:?}\n", vars_to_avoid);
        }
        while constant_fold_once(ops, &vars_to_avoid, globals, verbose) {}
    }
}
