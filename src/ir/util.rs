use std::collections::{BTreeSet, BTreeMap};

use mc::ast::*;

use ir::*;

use mc::session::Session;

pub fn add_to_globals(name: &Option<VarName>,
                      global_map: &mut BTreeMap<VarName, StaticIRItem>,
                      session: &mut Session) {
    if let Some(n) = *name {
        global_map.insert(n,
                          StaticIRItem {
                              name: n,
                              label: Some(session.interner.intern(
                                  format!("{}", n.base_name()))),
                              size: 0,
                              offset: None,
                              is_extern: false,
                              is_ref: false,
                              is_func: true,
                              expr: None,
                          });
    }
}

fn sub_vars(vars: &BTreeSet<Var>, orig_var: &Var,
            new_rvelem: &RValueElem) -> BTreeSet<Var> {
    // Substitute any variables appearing in the Goto or call.
    let mut new_vars = BTreeSet::new();
    for var in vars.iter() {
        if var == orig_var {
            match *new_rvelem {
                Variable(rv_var) => {
                    new_vars.insert(rv_var);
                },
                // If we're trying to substitute in a constant, something is
                // wrong: we should *never* be putting a constant in to
                // a Goto.
                _ => panic!("Invalid substitution"),
            }
        } else {
            new_vars.insert(*var);
        }
    }
    new_vars
}

// TODO: a separate function to sub in variables.
pub fn subst(ops: &mut Vec<Op>,
             orig_var: &Var,
             new_rvelem: &RValueElem) {
    let wrapped_var = Variable(*orig_var);

    for op in ops.iter_mut() {
        let temp = match op.val {
            OpNode::UnOp { target: ref v, ref op, operand: ref rv } =>
                if v == orig_var {
                    OpNode::Nop {}
                } else {
                    OpNode::UnOp {
                        target: *v,
                        op: *op,
                        operand: if *rv == wrapped_var {
                            new_rvelem
                        } else {
                            rv
                        }.clone()
                    }
                },
            OpNode::BinOp { target: ref v, ref op, lhs: ref rv1, rhs: ref rv2, signed } => {
                if v == orig_var {
                    OpNode::Nop {}
                } else {
                    OpNode::BinOp {
                        target: *v,
                        op: *op,
                        lhs: if *rv1 == wrapped_var {
                            new_rvelem
                        } else {
                            rv1
                        }.clone(),
                        rhs: if *rv2 == wrapped_var {
                            new_rvelem
                        } else {
                            rv2
                        }.clone(),
                        signed: signed
                    }
                }
            },
            OpNode::Goto { label_idx: ref u, ref vars } => {
                OpNode::Goto { label_idx: *u, vars: sub_vars(vars, orig_var, new_rvelem) }
            }
            OpNode::CondGoto { ref negated, cond: ref rve, label_idx: ref u, ref vars } => {
                let new_vars = sub_vars(vars, orig_var, new_rvelem);
                if wrapped_var == *rve {
                    match *new_rvelem {
                        Constant(ref c) => {
                            // TODO: emit a warning that condition is always false/true.
                            match *c {
                                BoolLit(b) =>
                                    if b == *negated {
                                        OpNode::Nop {}
                                    } else {
                                        OpNode::Goto { label_idx: *u, vars: new_vars }
                                    },
                                _ => panic!("CondGoto is conditioned on non-boolean constant."),
                            }
                        },
                        _ => OpNode::CondGoto {
                            negated: *negated,
                            cond: new_rvelem.clone(),
                            label_idx: *u,
                            vars: new_vars,
                        }
                    }
                } else {
                    OpNode::CondGoto {
                        negated: *negated,
                        cond: (*rve).clone(),
                        label_idx: *u,
                        vars: new_vars,
                    }
                }
            },
            OpNode::Return { retval: ref rve_opt } => {
                match *rve_opt {
                    Some(ref rve) => {
                        match *rve {
                            Variable(ref v) => {
                                if v == orig_var {
                                    OpNode::Return { retval: Some(new_rvelem.clone()) }
                                } else {
                                    OpNode::Return { retval: Some(Variable(*v)) }
                                }
                            },
                            Constant(ref c) => OpNode::Return { retval: Some(Constant(c.clone())) },
                        }
                    },
                    None => {
                        OpNode::Return { retval: None }
                    }
                }
            },
            OpNode::Call { target: lv, func: ref x, args: ref params } => {
                let new_x = if *x == wrapped_var {
                    new_rvelem.clone()
                } else {
                    x.clone()
                };
                let new_vars = params.iter().map(
                    |v|
                    if v == orig_var {
                        match *new_rvelem {
                            Variable(v2) => v2,
                            _ => panic!(),
                        }
                    } else {
                        *v
                    }).collect();

                OpNode::Call { target: lv, func: new_x, args: new_vars }
            },
            OpNode::Load { target: lv, addr: rv, width } => {
                let new_rv = if rv == *orig_var {
                    match *new_rvelem {
                        Variable(v) => v,
                        _ => panic!(),
                    }
                } else {
                    rv
                };
                OpNode::Load { target: lv, addr: new_rv, width: width }
            },
            OpNode::Store { addr: lv, value: rv, width } => {
                let new_rv = if rv == *orig_var {
                    match *new_rvelem {
                        Variable(v) => v,
                        _ => panic!(),
                    }
                } else {
                    rv
                };
                let new_lv = if lv == *orig_var {
                    match *new_rvelem {
                        Variable(v) => v,
                        _ => panic!(),
                    }
                } else {
                    lv
                };
                OpNode::Store { addr: new_lv, value: new_rv, width: width }
            }
            ref x => x.clone()
        };
        op.val = temp;
    }
}
