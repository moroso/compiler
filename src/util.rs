use collections::TreeMap;
use std::fmt::{Show, Formatter};

use std::fmt;

#[deriving(TotalEq, TotalOrd, PartialOrd, PartialEq, Clone, Show)]
pub struct Name(pub uint);

#[deriving(TotalEq, TotalOrd, PartialOrd, PartialEq, Clone)]
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

#[deriving(TotalEq, Clone, PartialEq)]
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