use std::fmt::{self, Show, Formatter};

#[derive(Copy, Eq, Ord, PartialOrd, PartialEq, Clone)]
pub enum Width {
    AnyWidth,
    Width32,
    Width16,
    Width8,
}

impl Show for Width {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Width::AnyWidth => "",
            Width::Width32 => "32",
            Width::Width16 => "16",
            Width::Width8  => "8",
        })
    }
}

#[derive(Copy, Eq, Clone, PartialEq)]
pub enum IntKind {
    GenericInt,
    SignedInt(Width),
    UnsignedInt(Width),
}

impl IntKind {
    pub fn is_signed(&self) -> bool {
        match *self {
            IntKind::GenericInt |
            IntKind::SignedInt(..) => true,
            IntKind::UnsignedInt(..) => false,
        }
    }

    pub fn is_generic(&self) -> bool {
        match *self {
            IntKind::GenericInt => true,
            _ => false,
        }
    }

    pub fn num_to_string(&self, n: u64) -> String {
        match *self {
            IntKind::GenericInt                   => format!("{}", n as i32),
            IntKind::SignedInt(Width::AnyWidth)   => format!("{}", n as i32),
            IntKind::SignedInt(Width::Width8)     => format!("{}", n as i8),
            IntKind::SignedInt(Width::Width16)    => format!("{}", n as i16),
            IntKind::SignedInt(Width::Width32)    => format!("{}", n as i32),
            IntKind::UnsignedInt(Width::AnyWidth) => format!("{}", n as u32),
            IntKind::UnsignedInt(Width::Width8)   => format!("{}", n as u8),
            IntKind::UnsignedInt(Width::Width16)  => format!("{}", n as u16),
            IntKind::UnsignedInt(Width::Width32)  => format!("{}", n as u32),
        }
    }
}

impl Show for IntKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            IntKind::GenericInt     => write!(f, ""),
            IntKind::SignedInt(w)   => write!(f, "i{}", w),
            IntKind::UnsignedInt(w) => write!(f, "u{}", w),
        }
    }
}
