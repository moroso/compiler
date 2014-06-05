// Constant folding.

use collections::TreeSet;
use ir::*;
use ir::util::subst;
use ast::*;
use values::*;

pub struct ConstantFolder {
    pub ops: Box<Vec<Op>>,
}

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

impl ConstantFolder {
    pub fn new(ops: Box<Vec<Op>>) -> ConstantFolder {
        ConstantFolder {
            ops: ops,
        }
    }

    pub fn constant_fold_once(&mut self, vars_to_avoid: &TreeSet<Var>) -> bool {
        let mut changes = vec!();

        for op in self.ops.iter() {
            match *op {
                Assign(ref lv, ref rv) =>
                    match *lv {
                        VarLValue(ref v) =>
                            match *rv {
                                DirectRValue(ref r) => {
                                    match *r {
                                        Constant(ref c) => {
                                            changes.push((v.clone(),
                                                          c.clone()));
                                        },
                                        _ => {},
                                    }
                                },
                                BinOpRValue(ref op, ref v1, ref v2) => {
                                    match fold(op, v1, v2) {
                                        Some(ref c) => {
                                            changes.push((v.clone(),
                                                          c.clone()));
                                        },
                                        _ => {}
                                    }
                                },
                                UnOpRValue(..) => {
                                    // TODO: implement this.
                                }
                            },
                        _ => {}
                    },
                _ => {},
            }
        }

        let mut changed = false;
        for (a, b) in changes.move_iter() {
            if !vars_to_avoid.contains(&a) {
                subst(self.ops, &a, &Constant(b));
                changed = true;
            }
        }
        changed
    }

    pub fn constant_fold(&mut self) {
        // There are certain variables we are prohibited from substituting.
        // Those include any that appear in labels/gotos, as well as any
        // that is dereferenced as part of the left hand side of an assignment.
        let mut vars_to_avoid = TreeSet::<Var>::new();
        for op in self.ops.iter() {
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
                _ => {},
            }
        }
        print!("avoid: {}\n", vars_to_avoid);
        while self.constant_fold_once(&vars_to_avoid) {}
    }
}
