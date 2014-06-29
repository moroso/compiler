use std::collections::TreeSet;
use std::iter::Map;

use mc::ast::*;

use ir::*;
use values::*;

fn sub_vars(vars: &TreeSet<Var>, orig_var: &Var,
            new_rvelem: &RValueElem) -> TreeSet<Var> {
    // Substitute any variables appearing in the Goto or call.
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

    for op in ops.mut_iter() {
        let temp = match *op {
            UnOp(ref v, ref op, ref rv) =>
                if v == orig_var {
                    Nop
                } else {
                    UnOp(v.clone(),
                         op.clone(),
                         if *rv == wrapped_var.clone() {
                             new_rvelem
                         } else {
                             rv
                         }.clone())

                },
            BinOp(ref v, ref op, ref rv1, ref rv2) =>
                if v == orig_var {
                    Nop
                } else {
                    BinOp(v.clone(),
                          op.clone(),
                          if *rv1 == wrapped_var.clone() {
                              new_rvelem
                          } else {
                              rv1
                          }.clone(),
                          if *rv2 == wrapped_var.clone() {
                              new_rvelem
                          } else {
                              rv2
                          }.clone())
                },
            Goto(ref u, ref vars) => {
                Goto(u.clone(), sub_vars(vars, orig_var, new_rvelem))
            }
            CondGoto(ref negated, ref rve, ref u, ref vars) => {
                let new_vars = sub_vars(vars, orig_var, new_rvelem);
                if wrapped_var == *rve {
                    // TODO: give a warning that conditional is always
                    // true or always false.
                    if (*new_rvelem == Constant(BoolLit(true))) != *negated {
                        Goto(u.clone(), new_vars)
                    } else {
                        Nop
                    }
                } else {
                    CondGoto(
                        negated.clone(),
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
            Call(ref lv, ref x, ref params) => {
                let new_vars = params.iter().map(
                    |param|
                    match *param {
                        Variable(ref v) if v == orig_var => {
                            new_rvelem.clone()
                        },
                        _ => param.clone()
                    }).collect();

                Call(lv.clone(), x.clone(), new_vars)
            },
            ref x => x.clone()
        };
        *op = temp;
    }
}
