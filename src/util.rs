use std::collections::TreeMap;
use std::fmt::{Show, Formatter};

use std::fmt;

// This represents an interned string/name/identifier. The mapping from strings
// to Names and Names to strings is in the Interner (session.rs).
#[deriving(Eq, Ord, PartialOrd, PartialEq, Clone, Show)]
pub struct Name(pub uint);

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
