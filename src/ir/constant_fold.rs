// Constant folding.
use util::{IntKind, Width};
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
    let mut lit1 = match *e1 {
        Constant(ref l) => {
            set_signedness(l, signed)
        },
        _ => return None,
    };
    let mut lit2 = match *e2 {
        Constant(ref l) => {
            set_signedness(l, signed)
        }
        _ => return None,
    };
    match lit1 {
        NumLit(_, ref mut kind1) => {
            match lit2 {
                NumLit(_, ref mut kind2) => {
                    if kind1.is_generic() && kind2.is_generic() {
                        let kind = if signed {
                            IntKind::SignedInt(Width::Width32)
                        } else {
                            IntKind::UnsignedInt(Width::Width32)
                        };
                        *kind1 = kind;
                        *kind2 = kind;
                    }
                    if kind1.is_generic() {
                        *kind1 = *kind2;
                    } else if kind2.is_generic() {
                        *kind2 = *kind1;
                    }
                },
                _ => {}
            }
        },
        _ => {}
    }

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
            OpNode::BinOp(ref v, ref op, ref v1, ref v2, signed) => {
                match fold(op, v1, v2, signed) {
                    Some(c) => {
                        changes.push((v.clone(),
                                      Constant(c.clone())));
                        immediate_changes.push((pos,
                                                OpNode::UnOp(v.clone(),
                                                     Identity,
                                                     Constant(c))));
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
                                     OpNode::UnOp(v.clone(),
                                          Identity,
                                          v2.clone())));
                            },
                            _ => {}
                        }
                        match *v2 {
                            Constant(NumLit(x, _)) if (Some(x) == ident ||
                                                       Some(x) == rhs_ident
                                                       ) => {
                                immediate_changes.push(
                                    (pos,
                                     OpNode::UnOp(v.clone(),
                                          Identity,
                                          v1.clone())));
                            }
                            _ => {}
                        }
                    }
                }
            },
            OpNode::UnOp(ref v, ref op, ref rv) => {
                match fold_unary(op, rv) {
                    Some(c) => {
                        changes.push((v.clone(),
                                      Constant(c.clone())));
                        if *op != Identity {
                            immediate_changes.push((pos,
                                                    OpNode::UnOp(v.clone(),
                                                         Identity,
                                                         Constant(c))));
                        }
                    },
                    _ => {}
                }
            }
            // TODO: unary ops, and anything else.
            _ => {},
        }
    }

    let mut changed = false;

    // These are changes we can do unconditionally.
    for (a, b) in immediate_changes.into_iter() {
        ops.get_mut(a).unwrap().val = b;
        changed = true;
    }

    for (a, b) in changes.into_iter() {
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
                OpNode::Label(_, ref vars) |
                OpNode::Goto(_, ref vars) |
                OpNode::CondGoto(_, _, _, ref vars) => {
                    for var in vars.iter() {
                        vars_to_avoid.insert(var.clone());
                    }
                },
                // We can't fold anything we take the address of.
                OpNode::UnOp(_, AddrOf, ref rv) => {
                    match *rv {
                        Variable(ref v) => { vars_to_avoid.insert(v.clone()); },
                        _ => {},
                    }
                },
                OpNode::Store(ref v1, ref v2, _) |
                OpNode::Load(ref v1, ref v2, _) => {
                    vars_to_avoid.insert(v1.clone());
                    vars_to_avoid.insert(v2.clone());
                },
                OpNode::Call(_, _, ref vars) => {
                    for var in vars.iter() {
                        vars_to_avoid.insert(var.clone());
                    }
                },
                OpNode::Func(_, ref vars, ref abi) => {
                    if abi.is_some() { return; }
                    for var in vars.iter() {
                        vars_to_avoid.insert(var.clone());
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
