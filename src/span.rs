use std::fmt;
use std::fmt::{Formatter, Display};
use std::cmp::{Eq, Ord, Ordering};

#[derive(Clone, PartialEq, Eq, Ord, Debug, Copy)]
pub struct SourcePos {
    pub row: usize,
    pub col: usize,
}

impl PartialOrd for SourcePos {
    #[inline]
    fn partial_cmp(&self, other: &SourcePos) -> Option<Ordering> {
        (self.row, self.col).partial_cmp(&(other.row, other.col))
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line {}, column {}", self.row+1, self.col+1)
    }
}

impl SourcePos {
    pub fn new() -> SourcePos {
        SourcePos { row: 0, col: 0 }
    }
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug, Copy)]
pub struct Span {
    // Private, so that we must use mk_span (which enforces invariants)
    // to make a Span.
    begin: SourcePos,
    end:   SourcePos,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} - {}", self.begin, self.end)
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

pub fn mk_sp(begin: SourcePos, len: usize) -> Span {
    let mut end = begin.clone();
    end.col += len;
    Span { begin: begin, end: end }
}
