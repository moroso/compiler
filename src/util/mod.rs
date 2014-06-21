use std::collections::TreeMap;
use std::fmt::{Show, Formatter};

use std::fmt;

pub mod lexer;

// This represents an interned string/name/identifier. The mapping from strings
// to Names and Names to strings is in the Interner (session.rs).
#[deriving(Eq, Ord, PartialOrd, PartialEq, Clone)]
pub struct Name(pub uint);

impl Show for Name {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use mc::session::interner;

        match interner.get() {
            None => {
                let Name(n) = *self;
                write!(f, "<name #{}>", n)
            }
            Some(real_interner) => {
                write!(f, "{}", real_interner.name_to_str(self))
            }
        }
    }
}

#[deriving(Eq, Ord, PartialOrd, PartialEq, Clone)]
pub enum Width {
    AnyWidth,
    Width32,
    Width16,
    Width8,
}

impl Show for Width {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            AnyWidth => "",
            Width32 => "32",
            Width16 => "16",
            Width8  => "8",
        })
    }
}

#[deriving(Eq, Clone, PartialEq)]
pub enum IntKind {
    GenericInt,
    SignedInt(Width),
    UnsignedInt(Width),
}

impl Show for IntKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            GenericInt     => write!(f, ""),
            SignedInt(w)   => write!(f, "i{}", w),
            UnsignedInt(w) => write!(f, "u{}", w),
        }
    }
}
