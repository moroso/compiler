use std::collections::TreeSet;
use std::iter::Map;

use mc::ast::*;

use ir::*;
use values::*;

fn substituted_rvalue(rv: &RValue,
                      wrapped_var: RValueElem,
                      new: &RValueElem) -> RValue {
    // TODO: rewrite this in a way that doesn't require as much cloning.
    match *rv {
        BinOpRValue(ref op, ref r1, ref r2) =>
            BinOpRValue(
                *op,
                if wrapped_var == *r1 { new.clone() }
                else { r1.clone() },
                if wrapped_var == *r2 { new.clone() }
                else { r2.clone() },
                ),
        UnOpRValue(ref op, ref r1) =>
            UnOpRValue(
                *op,
                if wrapped_var == *r1 { new.clone() }
                else { r1.clone() }
                ),
        DirectRValue(ref r) =>
            DirectRValue(if wrapped_var == *r
                         { new.clone() }
                         else { (*r).clone() }),
        CallRValue(ref r, ref args) => {
            CallRValue(
                if wrapped_var == *r {
                    new.clone()
                } else {
                    (*r).clone()
                },
                args.iter().map(|arg|
                         if wrapped_var == *arg {
                             new.clone()
                         } else {
                             (*arg).clone()
                         }
                ).collect()
            )
        },
        AllocaRValue(ref size) => AllocaRValue(size.clone()),
    }
}

fn sub_vars(vars: &TreeSet<Var>, orig_var: &Var,
            new_rvelem: &RValueElem) -> TreeSet<Var> {
    // Substitute any variables appearing in the Goto.
    let mut new_vars = TreeSet::new();
    for var in vars.iter() {
        if var == orig_var {
            match *new_rvelem {
                Variable(ref rv_var) => {
                    new_vars.insert(rv_var.clone());
                },
                // If we're trying to substitute in a constant, something is
                // wrong: we should *never* be putting a constant in to
                // a Goto.
                _ => fail!("Invalid substitution"),
            }
        } else {
            new_vars.insert(var.clone());
        }
    }
    new_vars
}

// TODO: we do a lot of cloning/replacing. Surely we can improve that?
pub fn subst(ops: &mut Vec<Op>,
             orig_var: &Var,
             new_rvelem: &RValueElem) {
    let wrapped_var = Variable(orig_var.clone());

    let is_constant = match *new_rvelem {
        Constant(..) => true,
        _ => false,
    };

    // TODO: this is pretty terrible.
    let var_opt = match *new_rvelem {
        Variable(ref v) => Some(v.clone()),
        _ => None,
    };

    for op in ops.mut_iter() {
        let temp = match *op {
            Assign(ref x, ref rv) =>
                match *x {
                    // This case behaves differenly if we're substituting a
                    // variable or a constant. We'll probably at some point
                    // want a separate function for variable substitutions.
                    VarLValue(ref v) if v == orig_var => {
                        if is_constant ||
                            *rv == DirectRValue(new_rvelem.clone()) {
                            Nop
                        } else {
                            Assign(match var_opt {
                                Some(ref v) => VarLValue(v.clone()),
                                _ => x.clone()
                            },
                                   substituted_rvalue(rv,
                                                  wrapped_var.clone(),
                                                  new_rvelem))
                        }
                    },
                    _ =>
                        Assign(x.clone(),
                               substituted_rvalue(rv,
                                                  wrapped_var.clone(),
                                                  new_rvelem)),
                },
            Goto(ref u, ref vars) => {
                Goto(u.clone(), sub_vars(vars, orig_var, new_rvelem))
            }
            CondGoto(ref rve, ref u, ref vars) => {
                let new_vars = sub_vars(vars, orig_var, new_rvelem);
                if wrapped_var == *rve {
                    // TODO: give a warning that conditional is always
                    // true or always false.
                    if *new_rvelem == Constant(BoolLit(true)) {
                        Goto(u.clone(), new_vars)
                    } else {
                        Nop
                    }
                } else {
                    CondGoto(
                        (*rve).clone(),
                        u.clone(),
                        new_vars
                    )
                }
            },
            Return(ref rve) => {
                match *rve {
                    Variable(ref v) => {
                        if v == orig_var {
                            Return(new_rvelem.clone())
                        } else {
                            Return(Variable(v.clone()))
                        }
                    },
                    Constant(ref c) => Return(Constant(c.clone())),
                }
            },
            Func(ref x, ref vars) => {
                let new_vars = vars.iter().map(
                    |var|
                    if var == orig_var {
                        match *new_rvelem {
                            Variable(ref rv_var) if var.name == rv_var.name => {
                                rv_var.clone()
                            },
                            _ => fail!("Invalid substitution"),
                        }
                    } else {
                        var.clone()
                    }).collect();

                Func(x.clone(), new_vars)
            }
            ref x => x.clone()
        };
        *op = temp;
    }
}
