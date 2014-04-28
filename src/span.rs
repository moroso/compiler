use std::fmt;

pub struct Spanned<T> {
    pub sp: Span,
    pub val: T,
}

impl<T: Eq> Eq for Spanned<T> {
    fn eq(&self, other: &Spanned<T>) -> bool {
        self.val.eq(&other.val)
    }
}

impl<T: fmt::Show> fmt::Show for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.val.fmt(f)
    }
}

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
        write!(f.buf, "{}:{}", self.row, self.col)
    }
}

impl SourcePos {
    pub fn new() -> SourcePos {
        SourcePos { row: 0, col: 0 }
    }
}

#[deriving(Clone, Eq, Ord, TotalEq, TotalOrd)]
pub struct Span {
    begin: SourcePos,
    pub end:   SourcePos,
}

impl fmt::Show for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f.buf, "{}: {}", self.begin, self.end)
    }
}

impl Span {
    pub fn to(self, other: Span) -> Span {
        assert!(self <= other);
        Span {
            begin: self.begin,
            end: other.end,
        }
    }
}

pub fn mk_sp(begin: SourcePos, len: uint) -> Span {
    let mut end = begin.clone();
    end.col += len;
    Span { begin: begin, end: end }
}
