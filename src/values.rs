use mc::ast::*;
use util::{IntKind, Width};
use std::ops::{Add, Sub, Shl, Shr, BitXor, BitOr, BitAnd, Div, Rem, Mul};

fn num_op_helper<U, S>(kind1: &IntKind, rhs: &LitNode,
                       u: U,
                       s: S) -> LitNode
    where U: Fn(u64) -> u64, S: Fn(u64) -> u64 {
    match *rhs {
        NumLit(n2, kind2) => {
            let is_signed = if kind1.is_generic() {
                kind2.is_signed()
            } else {
                kind1.is_signed()
            };

            let f: &Fn(u64) -> u64 = if is_signed { &s } else { &u };

            if *kind1 == IntKind::GenericInt {
                NumLit(f(n2), kind2)
            } else if kind2 == IntKind::GenericInt {
                NumLit(f(n2), *kind1)
            } else {
                assert_eq!(*kind1, kind2);
                NumLit(f(n2), kind2)
            }
        },
        _ => panic!("Incompatible types.")
    }
}

fn bool_op_helper<F>(rhs: &LitNode, f: F) -> LitNode
    where F: Fn(bool) -> bool {
    match *rhs {
        BoolLit(b2) => {
            BoolLit(f(b2))
        },
        _ => panic!("Incompatible types.")
    }
}

pub fn generic_op<S, T, U>(lhs: &LitNode, rhs: &LitNode,
                           uintfunc: S,
                           intfunc: T,
                           boolfunc: U) -> LitNode
    where S: Fn(u64, u64) -> u64, T: Fn(i64, i64) -> i64,
          U: Fn(bool, bool) -> bool {
    match *lhs {
        NumLit(n1, kind1) => num_op_helper(
            &kind1, rhs,
            |x| uintfunc(n1, x),
            |x| intfunc(n1 as i64, x as i64) as u64),
        BoolLit(b) => bool_op_helper(rhs, |x| boolfunc(b, x)),
        _ => panic!("Unimplemented: {} {}", lhs, rhs)
    }
}

pub fn generic_unop<S, T>(l: &LitNode,
                          intfunc: S,
                          boolfunc: T) -> LitNode
    where S: Fn(u64) -> u64, T: Fn(bool) -> bool{
    match *l {
        NumLit(n1, kind1) => NumLit(intfunc(n1), kind1),
        BoolLit(b) => BoolLit(boolfunc(b)),
        _ => panic!("Unimplemented.")
    }
}

/// An operator that takes ints and returns a bool.
pub fn relation_op<U, S>(lhs: &LitNode, rhs: &LitNode,
                         u: U,
                         s: S) -> LitNode
    where U: Fn(u64, u64) -> bool, S: Fn(i64, i64) -> bool {
    match *lhs {
        NumLit(n1, kind1) => match *rhs {
            NumLit(n2, kind2) if kind1 == kind2
                || kind1.is_generic()
                || kind2.is_generic() => {
                let signed = if kind1.is_generic() {
                    kind2.is_signed()
                } else {
                    kind1.is_signed()
                };
                let f: Box<Fn(u64, u64) -> bool> = if signed {
                    Box::new(|x: u64, y: u64| s(x as i64, y as i64))
                } else {
                    Box::new(u)
                };
                BoolLit(f(n1, n2))
            },
            _ => panic!("Can't fold rhs {}", rhs),
        },
        _ => panic!("Can't fold lhs {}", lhs),
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
        OrElseOp => generic_op(&lit1, &lit2, |_,_| panic!(), |_,_| panic!(),
                                    |x, y| x||y),
        AndAlsoOp => generic_op(&lit1, &lit2, |_,_| panic!(), |_,_| panic!(),
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
        Negate => Some(generic_unop(&lit, |x| -(x as i64) as u64, |_| panic!())),
        BitNot => Some(generic_unop(&lit, |x| !x, |_| panic!())),
        LogNot => Some(generic_unop(&lit, |_| panic!(), |x| !x)),
        Deref | AddrOf => None,
        SxbOp => Some(generic_unop(&lit, |x| (x as i8) as u64, |_| panic!())),
        SxhOp => Some(generic_unop(&lit, |x| (x as i16) as u64, |_| panic!())),
    }
}


impl Add<LitNode> for LitNode {
    type Output = LitNode;
    fn add(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs, |x, y| x+y, |x, y| x+y, |_,_| panic!())
    }
}

impl Mul<LitNode> for LitNode {
    type Output = LitNode;
    fn mul(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs, |x, y| x*y, |x, y| x*y, |_,_| panic!())
    }
}

impl Sub<LitNode> for LitNode {
    type Output = LitNode;
    fn sub(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs, |x, y| x-y, |x, y| x-y, |_,_| panic!())
    }
}

impl Div<LitNode> for LitNode {
    type Output = LitNode;
    fn div(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs, |x, y| x/y, |x, y| x/y, |_,_| panic!())
    }
}

impl BitAnd<LitNode> for LitNode {
    type Output = LitNode;
    fn bitand(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs, |x, y| x&y, |x, y| x&y, |_,_| panic!())
    }
}

impl BitOr<LitNode> for LitNode {
    type Output = LitNode;
    fn bitor(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs, |x, y| x|y, |x, y| x|y, |_,_| panic!())
    }
}

impl BitXor<LitNode> for LitNode {
    type Output = LitNode;
    fn bitxor(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs,
                   |x, y| x^y,
                   |x, y| x^y,
                   |_,_| panic!())
    }
}

impl Rem<LitNode> for LitNode {
    type Output = LitNode;
    fn rem(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs,
                   |x, y| x%y,
                   |x, y| x%y,
                   |_,_| panic!())
    }
}

impl Shl<LitNode> for LitNode {
    type Output = LitNode;
    fn shl(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs,
                   |x, y| x << y as usize,
                   |x, y| x << y as usize,
                   |_,_| panic!())
    }
}
impl Shr<LitNode> for LitNode {
    type Output = LitNode;
    fn shr(self, rhs: LitNode) -> LitNode {
        generic_op(&self, &rhs,
                   |x, y| x >> y as usize,
                   |x, y| x >> y as usize,
                   |_,_| panic!())
    }
}
