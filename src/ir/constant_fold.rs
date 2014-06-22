// Constant folding.

use ir::util::subst;

use std::collections::TreeSet;

use mc::ast::*;

use ir::*;
use values::*;

pub struct ConstantFolder;

fn fold(op: &BinOpNode, e1: &RValueElem, e2: &RValueElem) ->
    Option<LitNode>
{
    let lit1 = match *e1 {
        Constant(ref l) => l.clone(),
        _ => return None,
    };
    let lit2 = match *e2 {
        Constant(ref l) => l.clone(),
        _ => return None,
    };

    match *op {
        PlusOp => Some(lit1+lit2),
        TimesOp => Some(lit1*lit2),
        DivideOp => Some(lit1/lit2),
        OrElseOp => Some(generic_op(&lit1, &lit2, |_,_| fail!(),
                                    |x, y| x||y)),
        LessOp => Some(relation_op(&lit1, &lit2, |x, y| x < y)),
        LessEqOp => Some(relation_op(&lit1, &lit2, |x, y| x <= y)),
        GreaterOp => Some(relation_op(&lit1, &lit2, |x, y| x > y)),
        GreaterEqOp => Some(relation_op(&lit1, &lit2, |x, y| x >= y)),
        // TODO: the rest of the ops.
        _ => None,
    }
}

fn constant_fold_once(ops: &mut Vec<Op>, vars_to_avoid: &TreeSet<Var>) -> bool {
    // Variables to replace with constants.
    let mut changes = vec!();
    // Variables to replace with other variables.
    let mut var_changes = vec!();

    for op in ops.iter() {
        match *op {
            Assign(ref lv, ref rv) =>
                match *lv {
                    VarLValue(ref v) =>
                        match *rv {
                            DirectRValue(ref r) => {
                                // Note: instead of matching on r, we could
                                // just do
                                // changes.push(v.clone(), r.clone()).
                                // This would be just as efficient and just
                                // as correct, but also more likely to
                                // replace a named variable with a temp
                                // variable. So we do it this way to make the
                                // IR a bit more readable.
                                match *r {
                                    Constant(ref c) =>
                                        changes.push((v.clone(),
                                                      Constant(c.clone()))),
                                    Variable(ref v2) =>
                                        var_changes.push(
                                            (v2.clone(), v.clone())),
                                }
                            },
                            BinOpRValue(ref op, ref v1, ref v2) => {
                                match fold(op, v1, v2) {
                                    Some(ref c) => {
                                        changes.push((v.clone(),
                                                      Constant(c.clone())));
                                    },
                                    _ => {}
                                }
                            },
                            UnOpRValue(..) => {
                                // TODO: implement this.
                            },
                            CallRValue(..) => {
                                // TODO: implement this, maybe.
                            },
                            AllocaRValue(..) => {},
                        },
                    _ => {}
                },
            _ => {},
        }
    }

    let mut changed = false;
    let mut new_vars_to_avoid = TreeSet::new();

    for (a, b) in var_changes.move_iter() {
        let (a, b) = if vars_to_avoid.contains(&a)
            || new_vars_to_avoid.contains(&b) {
            (b, a)
        } else {
            (a, b)
        };

        // Either we're prohibited from changing this variable, or we've
        // already done a substitution of b. We'll punt on this; if we can
        // still do a substitution with it, we'll still do it in a later
        // iteration.
        if vars_to_avoid.contains(&a) || new_vars_to_avoid.contains(&b) {
            continue;
        }

        print!("Applying(var) {}->{}\n", a, b);
        subst(ops, &a, &Variable(b));
        print!("{}\n", ops);
        changed = true;
        new_vars_to_avoid.insert(a);
    }

    for (a, b) in changes.move_iter() {
        if !vars_to_avoid.contains(&a){
            print!("Applying {}->{}\n", a, b);
            subst(ops, &a, &b);
            print!("{}\n", ops);
            changed = true;
        }
    }

    changed
}


impl ConstantFolder {

    pub fn fold(ops: &mut Vec<Op>) {
        // There are certain variables we are prohibited from substituting.
        // Those include any that appear in labels/gotos, as well as any
        // that is dereferenced as part of the left hand side of an assignment.
        let mut vars_to_avoid = TreeSet::<Var>::new();
        for op in ops.iter() {
            match *op {
                Label(_, ref vars) |
                Goto(_, ref vars) |
                CondGoto(_, _, ref vars) => {
                    for var in vars.iter() {
                        vars_to_avoid.insert(var.clone());
                    }
                },
                Assign(ref lhs, _) => {
                    match *lhs {
                        PtrLValue(ref var) => {
                            vars_to_avoid.insert(var.clone());
                        },
                        _ => {},
                    }
                },
                Func(_, ref vars) => {
                    for var in vars.iter() {
                        vars_to_avoid.insert(var.clone());
                    }
                }
                _ => {},
            }
        }
        print!("avoid: {}\n", vars_to_avoid);
        while constant_fold_once(ops, &vars_to_avoid) {}
    }
}
