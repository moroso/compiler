/* The lexer.
 * This takes an interator of streams, and gives us an iterator of tokens.
 * Each token has an associated "span", which tells us where in the source
 * file the token was.
 */

use span::{Span, SourcePos, mk_sp};

use regex::Regex;
use std::{io, option, iter};
use std::slice::CloneableVector;

/// A token together with a Span, to keep track of where in the source file
/// it was.
#[deriving(Show, Eq, PartialEq)]
pub struct SourceToken<T> {
    pub tok: T,
    pub sp: Span,
}

/// A single rule for the lexer. This includes a `matcher`, which matches
/// a string in the input, and a `maker`, which generates the token from
/// the matched string.
pub struct LexerRule<T, U> {
    pub matcher: T,
    pub maker: U,
}

// Trait to make lexer_rules! forget the type parameters of LexerRule
pub trait LexerRuleT<T> {
    fn run(&self, s: &str) -> Option<(uint, T)>;
}

// The idea behind this is to allow very flexible lexer rules in the lexer_rules!
// macro.  A LexerRule has a rule with which to check for a match, and a factory
// to produce a Token if the rule's match succeeds (some tokens like Number, String,
// and IdentTok carry additional context about what matched).  This way, we can specify
// a raw string as a rule to do a simple string-prefix match, a Regex to check
// for a regex match, or optionally more complicated rules to e.g. use capture
// groups from a Regex and construct a token from those.
impl<A, T: RuleMatcher<A>, U: TokenMaker<A, V>, V> LexerRuleT<V> for LexerRule<T, U> {
    fn run(&self, s: &str) -> Option<(uint, V)> {
        match self.matcher.find(s) {
            Some((len, args)) => Some((len, self.maker.mk_tok(args))),
            _ => None
        }
    }
}

pub struct Language<T> {
    pub eof: T,
    pub ws: T,
    pub begin_comment: T,
    pub end_comment: T,
    pub rules: Vec<Box<LexerRuleT<T>>>,
    pub comment_rules: Vec<Box<LexerRuleT<T>>>,
}

pub struct Lexer<B, T> {
    lines: BufferLines<B>,
    line: Option<String>,
    pos: SourcePos,
    name: String,
    // Ordinary rules.
    rules: Vec<Box<LexerRuleT<T>>>,
    // Rules specifically for when we're within a comment. We need this
    // for handling multi-line comments.
    comment_rules: Vec<Box<LexerRuleT<T>>>,
    comment_nest: uint,
    // We set this to Some(Eof) and take it when we hit EOF
    eof: Option<T>,
    ws: T,
    begincomment: T,
    endcomment: T,
}

impl<B: Buffer, T> Lexer<B, T> {
    pub fn get_name(&self) -> String {
        self.name.clone()
    }

    pub fn new<S: StrAllocating>(lang: Language<T>, name: S, buffer: B) -> Lexer<B, T> {
        Lexer {
            pos:  SourcePos::new(),
            line: Some(String::new()),
            lines: BufferLines::new(buffer),
            name: name.into_string(),
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
impl<B: Buffer, T: Eq> Iterator<SourceToken<T>> for Lexer<B, T> {
    fn next(&mut self) -> Option<SourceToken<T>> {
        loop {
            match self.line {
                Some(ref line) => {
                    let line = line.as_slice();
                    while self.pos.col < line.len() {
                        // We apply each rule. Of the ones that match, we take
                        // the longest match.
                        let mut longest = 0u;
                        let mut best = None;
                        let rules = if self.comment_nest > 0 {
                            &self.comment_rules
                        } else {
                            &self.rules
                        };

                        for rule in rules.iter() {
                            let m = rule.run(line.slice_from(self.pos.col));
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
                            None => fail!("Unexpected input"),
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
                        fail!("Unterminated multiline comment found in input stream");
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

// A TokenMaker accepts an argument tuple and hands back a token.
pub trait TokenMaker<T, U> {
    fn mk_tok(&self, args: T) -> U;
}

// A RuleMatcher accepts a string slice and tests the encapsulated rule on it.
// If there is a match it can optionally hand back a string slice corresponding
// to that match. (The "optionally" part is determined by whether or not the
// TokenMaker for this RuleMatcher requires the match as an argument, like IdentTok;
// the type magic is handled by MaybeArg).
pub trait RuleMatcher<T> {
    fn find<'a>(&self, s: &'a str) -> Option<(uint, T)>;
}

// Simple string-prefix match
impl<'a, T: MaybeArg> RuleMatcher<T> for &'a str {
    fn find<'a>(&self, s: &'a str) -> Option<(uint, T)> {
        match s.starts_with(*self) {
            true => Some((self.len(), MaybeArg::maybe_arg(*self))),
            _ => None
        }
    }
}

// Regex match
impl<T: MaybeArg> RuleMatcher<T> for Regex {
    fn find<'a>(&self, s: &'a str) -> Option<(uint, T)> {
        match self.find(s) {
            Some((_, end)) => {
                let t = s.slice(0, end);
                Some((t.len(), MaybeArg::maybe_arg(t)))
            },
            _ => None
        }
    }
}

// Utility trait to optionally grab the match as an argument
// (useful to avoid unnecessary string copies when we will just throw the result away anyway)
trait MaybeArg {
    fn maybe_arg<T: StrAllocating>(arg: T) -> Self;
}

impl MaybeArg for () {
    fn maybe_arg<T: StrAllocating>(_: T) { }
}

impl MaybeArg for String {
    fn maybe_arg<T: StrAllocating>(s: T) -> String { s.into_string() }
}

struct BufferLines<B> {
    lineno: uint,
    buffer: B,
}

impl<B: Buffer> BufferLines<B> {
    fn new(buffer: B) -> BufferLines<B> {
        BufferLines {
            lineno: 0,
            buffer: buffer,
        }
    }
}

impl<B: Buffer> Iterator<(uint, String)> for BufferLines<B> {
    fn next(&mut self) -> Option<(uint, String)> {
        use std::num::from_str_radix;

        self.buffer.read_line().ok().map(|l| {
            // *Super* hacky handling of the line num directives produced by cpp
            {
                let s = l.as_slice();
                if s.char_at(0) == '#' {
                    let s2: &str = s.split_str(" ").nth(1).unwrap_or("");

                    let i = from_str_radix::<uint>(s2, 10).unwrap_or(1);
                    self.lineno = i - 1;
                    return (0, String::from_str(""))
                }
            }

            let n = self.lineno;
            self.lineno += 1;
            (n, l)
        })
    }
}
