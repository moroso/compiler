use std::fmt;

#[deriving(Clone, Eq, TotalEq, TotalOrd)]
pub struct SourcePos {
    pub row: uint,
    pub col: uint,
}

impl Ord for SourcePos {
    #[inline]
    fn lt(&self, other: &SourcePos) -> bool {
        self.row < other.row || (self.row == other.row && self.col < other.col)
    }

    #[inline]
    fn le(&self, other: &SourcePos) -> bool {
        self.row < other.row || (self.row == other.row && self.col <= other.col)
    }

    #[inline]
    fn gt(&self, other: &SourcePos) -> bool {
        self.row >= other.row || (self.row == other.row && self.col > other.col)
    }

    #[inline]
    fn ge(&self, other: &SourcePos) -> bool {
        self.row >= other.row || (self.row == other.row && self.col >= other.col)
    }
}

impl fmt::Show for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line {}, column {}", self.row+1, self.col+1)
    }
}

impl SourcePos {
    pub fn new() -> SourcePos {
        SourcePos { row: 0, col: 0 }
    }
}

#[deriving(Clone, Eq, Ord, TotalEq, TotalOrd)]
pub struct Span {
    // Private, so that we must use mk_span (which enforces invariants)
    // to make a Span.
    begin: SourcePos,
    end:   SourcePos,
}

impl fmt::Show for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.begin, self.end)
    }
}

impl Span {
    pub fn to(self, other: Span) -> Span {
        assert!(self <= other, format!("self={}, other={}", self, other));
        Span {
            begin: self.begin,
            end: other.end,
        }
    }

    pub fn get_begin(&self) -> SourcePos {
        self.begin
    }

    pub fn get_end(&self) -> SourcePos {
        self.end
    }
}

pub fn mk_sp(begin: SourcePos, len: uint) -> Span {
    let mut end = begin.clone();
    end.col += len;
    Span { begin: begin, end: end }
}
