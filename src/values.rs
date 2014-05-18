use std::fmt::{Formatter, Result, Show};

#[deriving(Eq, Clone)]
pub enum Width {
    AnyWidth,
    Width32,
    Width16,
    Width8,
}

impl Show for Width {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", match *self {
            AnyWidth => "",
            Width32 => "32",
            Width16 => "16",
            Width8  => "8",
        })
    }
}

#[deriving(Eq, Clone)]
pub enum LitNode {
    NumLit(u64, IntKind),
    StringLit(StringValue),
    BoolLit(bool),
}

impl Show for LitNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            NumLit(i, nt)     => write!(f, "{}{}", i, nt),
            StringLit(ref s)  => write!(f, "\"{}\"", s),
            BoolLit(b)        => write!(f, "BoolLit:{}", b),
        }
    }
}

#[deriving(Eq, Clone)]
pub enum IntKind {
    GenericInt,
    SignedInt(Width),
    UnsignedInt(Width),
}

impl Show for IntKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            GenericInt     => write!(f, ""),
            SignedInt(w)   => write!(f, "i{}", w),
            UnsignedInt(w) => write!(f, "u{}", w),
        }
    }
}

// Change this when we decide to intern strings
pub type StringValue = ~str;

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