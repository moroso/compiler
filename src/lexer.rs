use std::borrow::IntoCow;
use std::fmt::{self, Formatter};
use std::io::{Buffer, BufferedReader};

use regex::Regex;

use span::{Span, SourcePos, mk_sp};

/// A token together with a Span, to keep track of where in the source file it was.
#[derive(Copy, Eq, PartialEq)]
pub struct SourceToken<T> {
    pub tok: T,
    pub sp: Span,
}

impl<T: fmt::Show> fmt::Show for SourceToken<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "SourceToken {{ tok: {:?}, sp: {:?} }}", self.tok, self.sp)
    }
}

pub struct Language<T> {
    pub eof: T,
    pub ws: T,
    pub begin_comment: T,
    pub end_comment: T,
    pub rules: Vec<fn(&str) -> Option<(usize, T)>>,
    pub comment_rules: Vec<fn(&str) -> Option<(usize, T)>>,
}

pub struct Lexer<B, T> {
    lines: BufferLines<B>,
    line: Option<String>,
    pos: SourcePos,
    name: String,
    // Ordinary rules.
    rules: Vec<fn(&str) -> Option<(usize, T)>>,
    // Rules specifically for when we're within a comment. We need this
    // for handling multi-line comments.
    comment_rules: Vec<fn(&str) -> Option<(usize, T)>>,
    comment_nest: usize,
    // We set this to Some(Eof) and take it when we hit EOF
    eof: Option<T>,
    ws: T,
    begincomment: T,
    endcomment: T,
}

struct BufferLines<B> {
    lineno: usize,
    buffer: BufferedReader<B>,
}

impl<B: Reader> BufferLines<B> {
    fn new(buffer: B) -> BufferLines<B> {
        BufferLines {
            lineno: 0,
            buffer: BufferedReader::new(buffer),
        }
    }
}

impl<B: Reader> Iterator for BufferLines<B> {
    type Item = (usize, String);
    fn next(&mut self) -> Option<(usize, String)> {
        self.buffer.read_line().ok().map(|l| {
            let n = self.lineno;
            self.lineno += 1;
            (n, l)
        })
    }
}

impl<B: Reader, T> Lexer<B, T> {
    pub fn get_name(&self) -> String {
        self.name.clone()
    }

    pub fn new<'s, S: IntoCow<'s, String, str>>(lang: Language<T>, name: S, buffer: B) -> Lexer<B, T> {
        Lexer {
            pos:  SourcePos::new(),
            line: Some(String::new()),
            lines: BufferLines::new(buffer),
            name: name.into_cow().into_owned(),
            rules: lang.rules,
            comment_rules: lang.comment_rules,
            comment_nest: 0,
            eof: Some(lang.eof),
            ws: lang.ws,
            begincomment: lang.begin_comment,
            endcomment: lang.end_comment,
        }
    }
}

// The meat of the lexer (read this as a stateful flat-map)
impl<B: Reader, T: Eq> Iterator for Lexer<B, T> {
    type Item = SourceToken<T>;
    fn next(&mut self) -> Option<SourceToken<T>> {
        loop {
            match self.line.as_ref().map(|l| l.as_slice()) {
                Some(line) => {
                    while self.pos.col < line.len() {
                        // We apply each rule. Of the ones that match, we take
                        // the longest match.
                        let mut longest = 0us;
                        let mut best = None;
                        let rules = if self.comment_nest > 0 {
                            &self.comment_rules
                        } else {
                            &self.rules
                        };

                        for rule in rules.iter() {
                            let m = rule(line.slice_from(self.pos.col));
                            match m {
                                Some((len, tok)) => {
                                    if len > longest {
                                        // We have a match that's longer than our
                                        // previous one. Remember it.
                                        best = Some((mk_sp(self.pos, len), tok));
                                        longest = len;
                                    }
                                },
                                _ => {},
                            }
                        }

                        // Advance our position within the line.
                        self.pos.col += longest;

                        match best {
                            None => panic!("Unexpected input at {:?} ({:?} line {:?} col {:?})",
                                          line.slice_from(self.pos.col),
                                          self.name,
                                          self.pos.row + 1,
                                          self.pos.col,
                                          ),
                            Some((_, ref x)) if x == &self.ws
                                => {} // Skip whitespace.
                            Some((_, ref x)) if x == &self.begincomment
                                => { self.comment_nest += 1; }
                            Some((_, ref x)) if x == &self.endcomment
                                => { self.comment_nest -= 1; }
                            Some((sp, tok)) => {
                                return Some(SourceToken {
                                    tok: tok,
                                    sp: sp,
                                })
                            }
                        }
                    }
                }
                None => {
                    if self.comment_nest > 0 {
                        panic!("Unterminated multiline comment found in input stream");
                    }

                    return self.eof.take().map(|eof| SourceToken {
                        tok: eof,
                        sp: mk_sp(self.pos, 0),
                    });
                }
            }

            // Fetch a new line, now that we're done with the previous one.
            self.line = self.lines.next().map(|(row, line)| {
                self.pos = SourcePos { row: row, col: 0 };
                line
            });
        }
    }
}

// A RuleMatcher accepts a string slice and tests the encapsulated rule on it.  If there is a match
// it can optionally hand back a string slice corresponding to that match. (The "optionally" part
// is determined by whether or not the result of this RuleMatcher requires the match as an
// argument; the type magic is handled by SimpleRuleArg).
pub trait RuleMatcher<'a, A>: Sized {
    fn check(&self, s: &'a str) -> Option<(usize, A)>;
}

// Simple string-prefix match
impl<'a, T: SimpleRuleArg<'a>> RuleMatcher<'a, T> for &'a str {
    fn check(&self, s: &'a str) -> Option<(usize, T)> {
        match s.starts_with(*self) {
            true => {
                let len = self.len();
                let t = s.slice(0, len);
                Some((len, SimpleRuleArg::make_from(t)))
            },
            _ => None
        }
    }
}

// Regex match
impl<'a, T: SimpleRuleArg<'a>> RuleMatcher<'a, T> for Regex {
    fn check(&self, s: &'a str) -> Option<(usize, T)> {
        match self.find(s) {
            Some((_, end)) => {
                let t = s.slice(0, end);
                Some((t.len(), SimpleRuleArg::make_from(t)))
            },
            _ => None
        }
    }
}

// Arbitrary function
impl<'a, T, F: Fn(&'a str) -> Option<(usize, T)>> RuleMatcher<'a, T> for F {
    fn check(&self, s: &'a str) -> Option<(usize, T)> {
        self(s)
    }
}

// Utility trait to optionally grab the match as an argument
// (useful to avoid unnecessary string copies when we will just throw the result away anyway)
trait SimpleRuleArg<'a> {
    fn make_from(&'a str) -> Self;
}

impl<'a> SimpleRuleArg<'a> for () {
    fn make_from(_: &'a str) -> () { }
}

impl<'a> SimpleRuleArg<'a> for (&'a str,) {
    fn make_from(t: &'a str) -> (&'a str,) { (t,) }
}

impl<'a> SimpleRuleArg<'a> for (String,) {
    fn make_from(t: &'a str) -> (String,) { (t.into_cow().into_owned(),) }
}
