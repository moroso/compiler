use mc::ast::*;
use util::{IntKind, Width, GenericInt};

fn num_op_helper(kind1: &IntKind, rhs: &LitNode,
                 u: |u64| -> u64,
                 s: |u64| -> u64) -> LitNode {
    match *rhs {
        NumLit(n2, kind2) => {
            let is_signed = if kind1.is_generic() {
                kind2.is_signed()
            } else {
                kind1.is_signed()
            };

            let f = if is_signed { s } else { u };

            if *kind1 == GenericInt {
                NumLit(f(n2), kind2)
            } else if kind2 == GenericInt {
                NumLit(f(n2), *kind1)
            } else {
                assert_eq!(*kind1, kind2);
                NumLit(f(n2), kind2)
            }
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
                  uintfunc: |u64, u64| -> u64,
                  intfunc: |i64, i64| -> i64,
                  boolfunc: |bool, bool| -> bool) -> LitNode {
    match *lhs {
        NumLit(n1, kind1) => num_op_helper(
            &kind1, rhs,
            |x| uintfunc(n1, x),
            |x| intfunc(n1 as i64, x as i64) as u64),
        BoolLit(b) => bool_op_helper(rhs, |x| boolfunc(b, x)),
        _ => fail!("Unimplemented: {} {}", lhs, rhs)
    }
}

pub fn generic_unop(l: &LitNode,
                    intfunc: |u64| -> u64,
                    boolfunc: |bool| -> bool) -> LitNode {
    match *l {
        NumLit(n1, kind1) => NumLit(intfunc(n1), kind1),
        BoolLit(b) => BoolLit(boolfunc(b)),
        _ => fail!("Unimplemented.")
    }
}

/// An operator that takes ints and returns a bool.
pub fn relation_op(lhs: &LitNode, rhs: &LitNode,
                   u: |u64, u64| -> bool,
                   s: |i64, i64| -> bool) -> LitNode {
    match *lhs {
        NumLit(n1, kind1) => match *rhs {
            NumLit(n2, kind2) if kind1 == kind2 => {
                let signed = kind1.is_signed();
                let f = if signed {
                    |x: u64, y: u64| s(x as i64, y as i64)
                } else {
                    u
                };
                BoolLit(f(n1, n2))
            },
            _ => fail!(),
        },
        _ => fail!(),
    }
}

pub fn eval_binop(op: BinOpNode,
                  lit1: LitNode, lit2: LitNode) -> LitNode {
    match op {
        PlusOp => lit1+lit2,
        MinusOp => lit1-lit2,
        TimesOp => lit1*lit2,
        DivideOp => lit1/lit2,
        ModOp => lit1%lit2,
        BitAndOp => lit1&lit2,
        BitOrOp => lit1|lit2,
        BitXorOp => lit1^lit2,
        LeftShiftOp => lit1<<lit2,
        RightShiftOp => lit1>>lit2,
        OrElseOp => generic_op(&lit1, &lit2, |_,_| fail!(), |_,_| fail!(),
                                    |x, y| x||y),
        AndAlsoOp => generic_op(&lit1, &lit2, |_,_| fail!(), |_,_| fail!(),
                                     |x, y| x&&y),
        LessOp => relation_op(&lit1, &lit2, |x, y| x < y, |x, y| x < y),
        LessEqOp => relation_op(&lit1, &lit2, |x, y| x <= y, |x, y| x <= y),
        GreaterOp => relation_op(&lit1, &lit2, |x, y| x > y, |x, y| x > y),
        GreaterEqOp => relation_op(&lit1, &lit2, |x, y| x >= y, |x, y| x >= y),
        EqualsOp => BoolLit(lit1 == lit2),
        NotEqualsOp => BoolLit(lit1 != lit2),
    }
}

pub fn eval_unop(op: UnOpNode, lit: LitNode) -> Option<LitNode> {
    match op {
        Identity => Some(lit),
        Negate => Some(generic_unop(&lit, |x| -(x as i64) as u64, |_| fail!())),
        BitNot => Some(generic_unop(&lit, |x| !x, |_| fail!())),
        LogNot => Some(generic_unop(&lit, |_| fail!(), |x| !x)),
        Deref | AddrOf => None,
        SxbOp => Some(generic_unop(&lit, |x| (x as i8) as u64, |_| fail!())),
        SxhOp => Some(generic_unop(&lit, |x| (x as i16) as u64, |_| fail!())),
    }
}


impl Add<LitNode, LitNode> for LitNode {
    fn add(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x+y, |x, y| x+y, |_,_| fail!())
    }
}

impl Mul<LitNode, LitNode> for LitNode {
    fn mul(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x*y, |x, y| x*y, |_,_| fail!())
    }
}

impl Sub<LitNode, LitNode> for LitNode {
    fn sub(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x-y, |x, y| x-y, |_,_| fail!())
    }
}

impl Div<LitNode, LitNode> for LitNode {
    fn div(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x/y, |x, y| x/y, |_,_| fail!())
    }
}

impl BitAnd<LitNode, LitNode> for LitNode {
    fn bitand(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x&y, |x, y| x&y, |_,_| fail!())
    }
}

impl BitOr<LitNode, LitNode> for LitNode {
    fn bitor(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs, |x, y| x|y, |x, y| x|y, |_,_| fail!())
    }
}

impl BitXor<LitNode, LitNode> for LitNode {
    fn bitxor(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs,
                   |x, y| x^y,
                   |x, y| x^y,
                   |_,_| fail!())
    }
}

impl Rem<LitNode, LitNode> for LitNode {
    fn rem(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs,
                   |x, y| x%y,
                   |x, y| x%y,
                   |_,_| fail!())
    }
}

impl Shl<LitNode, LitNode> for LitNode {
    fn shl(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs,
                   |x, y| x << y as uint,
                   |x, y| x << y as uint,
                   |_,_| fail!())
    }
}
impl Shr<LitNode, LitNode> for LitNode {
    fn shr(&self, rhs: &LitNode) -> LitNode {
        generic_op(self, rhs,
                   |x, y| x >> y as uint,
                   |x, y| x >> y as uint,
                   |_,_| fail!())
    }
}
