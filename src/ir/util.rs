use std::collections::BTreeSet;
use std::iter::Map;

use mc::ast::*;

use ir::*;
use values::*;

fn sub_vars(vars: &BTreeSet<Var>, orig_var: &Var,
            new_rvelem: &RValueElem) -> BTreeSet<Var> {
    // Substitute any variables appearing in the Goto or call.
    let mut new_vars = BTreeSet::new();
    for var in vars.iter() {
        if var == orig_var {
            match *new_rvelem {
                Variable(ref rv_var) => {
                    new_vars.insert(rv_var.clone());
                },
                // If we're trying to substitute in a constant, something is
                // wrong: we should *never* be putting a constant in to
                // a Goto.
                _ => panic!("Invalid substitution"),
            }
        } else {
            new_vars.insert(var.clone());
        }
    }
    new_vars
}

// TODO: a separate function to sub in variables.
pub fn subst(ops: &mut Vec<Op>,
             orig_var: &Var,
             new_rvelem: &RValueElem) {
    let wrapped_var = Variable(orig_var.clone());

    for op in ops.iter_mut() {
        let temp = match op.val {
            OpNode::UnOp(ref v, ref op, ref rv) =>
                if v == orig_var {
                    OpNode::Nop
                } else {
                    OpNode::UnOp(*v,
                         *op,
                         if *rv == wrapped_var {
                             new_rvelem
                         } else {
                             rv
                         }.clone())

                },
            OpNode::BinOp(ref v, ref op, ref rv1, ref rv2, signed) => {
                if v == orig_var {
                    OpNode::Nop
                } else {
                    OpNode::BinOp(*v,
                          *op,
                          if *rv1 == wrapped_var {
                              new_rvelem
                          } else {
                              rv1
                          }.clone(),
                          if *rv2 == wrapped_var {
                              new_rvelem
                          } else {
                              rv2
                          }.clone(),
                          signed)
                }
            },
            OpNode::Goto(ref u, ref vars) => {
                OpNode::Goto(u.clone(), sub_vars(vars, orig_var, new_rvelem))
            }
            OpNode::CondGoto(ref negated, ref rve, ref u, ref vars) => {
                let new_vars = sub_vars(vars, orig_var, new_rvelem);
                if wrapped_var == *rve {
                    match *new_rvelem {
                        Constant(ref c) => {
                            // TODO: emit a warning that condition is always false/true.
                            match *c {
                                BoolLit(b) =>
                                    if b == *negated {
                                        OpNode::Nop
                                    } else {
                                        OpNode::Goto(*u, new_vars)
                                    },
                                _ => panic!("CondGoto is conditioned on non-boolean constant."),
                            }
                        },
                        _ => OpNode::CondGoto(
                            *negated,
                            new_rvelem.clone(),
                            *u,
                            new_vars)
                    }
                } else {
                    OpNode::CondGoto(
                        *negated,
                        (*rve).clone(),
                        *u,
                        new_vars
                    )
                }
            },
            OpNode::Return(ref rve_opt) => {
                match *rve_opt {
                    Some(ref rve) => {
                        match *rve {
                            Variable(ref v) => {
                                if v == orig_var {
                                    OpNode::Return(Some(new_rvelem.clone()))
                                } else {
                                    OpNode::Return(Some(Variable(*v)))
                                }
                            },
                            Constant(ref c) => OpNode::Return(Some(Constant(c.clone()))),
                        }
                    },
                    None => {
                        OpNode::Return(None)
                    }
                }
            },
            OpNode::Call(ref lv, ref x, ref params) => {
                let new_x = if *x == wrapped_var {
                    new_rvelem.clone()
                } else {
                    x.clone()
                };
                let new_vars = params.iter().map(
                    |v|
                    if v == orig_var {
                        match *new_rvelem {
                            Variable(ref v2) => v2.clone(),
                            _ => panic!(),
                        }
                    } else {
                        v.clone()
                    }).collect();

                OpNode::Call(lv.clone(), new_x.clone(), new_vars)
            },
            OpNode::Load(ref lv, ref rv, ref width) => {
                let new_rv = if rv == orig_var {
                    match *new_rvelem {
                        Variable(ref v) =>
                            v.clone(),
                        _ => panic!(),
                    }
                } else {
                    rv.clone()
                };
                OpNode::Load(*lv, new_rv, *width)
            },
            OpNode::Store(ref lv, ref rv, ref width) => {
                let new_rv = if rv == orig_var {
                    match *new_rvelem {
                        Variable(ref v) =>
                            v.clone(),
                        _ => panic!(),
                    }
                } else {
                    rv.clone()
                };
                let new_lv = if lv == orig_var {
                    match *new_rvelem {
                        Variable(ref v) =>
                            v.clone(),
                        _ => panic!(),
                    }
                } else {
                    lv.clone()
                };
                OpNode::Store(new_lv, new_rv, *width)
            }
            ref x => x.clone()
        };
        op.val = temp;
    }
}
