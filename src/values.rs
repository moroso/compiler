use mc::ast::*;
use util::{IntKind, Width};

fn num_op_helper(kind1: &IntKind, rhs: &LitNode, f: |u64| -> u64) -> LitNode {
    match *rhs {
        NumLit(n2, kind2) => {
            assert_eq!(*kind1, kind2);
            NumLit(f(n2), kind2)
        },
        _ => fail!("Incompatible types.")
    }
}

fn bool_op_helper(rhs: &LitNode, f: |bool| -> bool) -> LitNode {
    match *rhs {
        BoolLit(b2) => {
            BoolLit(f(b2))
        },
        _ => fail!("Incompatible types.")
    }
}

pub fn generic_op(lhs: &LitNode, rhs: &LitNode,
              intfunc: |u64, u64| -> u64,
              boolfunc: |bool, bool| -> bool) -> LitNode {
    match *lhs {
        NumLit(n1, kind1) => num_op_helper(&kind1, rhs, |x| intfunc(n1, x)),
        BoolLit(b) => bool_op_helper(rhs, |x| boolfunc(b, x)),
        _ => fail!("Unimplemented.")
    }
}

/// An operator that takes ints and returns a bool.
pub fn relation_op(lhs: &LitNode, rhs: &LitNode,
                   f: |u64, u64| -> bool) -> LitNode {
    match *lhs {
        NumLit(n1, kind1) => match *rhs {
            NumLit(n2, kind2) if kind1 == kind2 => {
                BoolLit(f(n1, n2))
            },
            _ => fail!(),
        },
        _ => fail!(),
    }
}

pub fn eval_binop(op: BinOpNode,
                  lit1: LitNode, lit2: LitNode) -> Option<LitNode> {
    match op {
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

pub fn eval_unop(op: UnOpNode, lit: LitNode) -> Option<LitNode> {
    match op {
        Identity => Some(lit),
        _ => unimplemented!(),
    }
}



impl Add<LitNode, LitNode> for LitNode {
    fn add(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x+y, |_,_| fail!())
    }
}

impl Mul<LitNode, LitNode> for LitNode {
    fn mul(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x*y, |_,_| fail!())
    }
}

impl Sub<LitNode, LitNode> for LitNode {
    fn sub(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x-y, |_,_| fail!())
    }
}

impl Div<LitNode, LitNode> for LitNode {
    fn div(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x/y, |_,_| fail!())
    }
}

impl BitAnd<LitNode, LitNode> for LitNode {
    fn bitand(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x&y, |_,_| fail!())
    }
}

impl BitOr<LitNode, LitNode> for LitNode {
    fn bitor(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x|y, |_,_| fail!())
    }
}

impl BitXor<LitNode, LitNode> for LitNode {
    fn bitxor(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x^y, |_,_| fail!())
    }
}

impl Rem<LitNode, LitNode> for LitNode {
    fn rem(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x%y, |_,_| fail!())
    }
}
