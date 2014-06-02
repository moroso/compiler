// Constant folding.

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

    pub fn constant_fold_once(&mut self) -> bool {
        let mut changed = false;
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
                                            changed = true;
                                            changes.push((v.clone(),
                                                          c.clone()));
                                        },
                                        _ => {},
                                    }
                                },
                                BinOpRValue(ref op, ref v1, ref v2) => {
                                    match fold(op, v1, v2) {
                                        Some(ref c) => {
                                            changed = true;
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

        for (a, b) in changes.move_iter() {
            subst(self.ops, &a, &Constant(b));
        }
        changed
    }

    pub fn constant_fold(&mut self) {
        while self.constant_fold_once() {}
    }
}
