/* The lexer.
 * This takes an interator of streams, and gives us an iterator of tokens.
 * Each token has an associated "span", which tells us where in the source
 * file the token was.
 */

use ast;
use span::{Span, SourcePos, mk_sp};
use util::{IntKind, GenericInt, SignedInt, UnsignedInt};
use util::{Width, AnyWidth, Width8, Width16, Width32};

use regex::Regex;
use std::{io, option, iter};
use std::slice::CloneableVector;


#[deriving(Eq, Clone, Show)]
pub enum Token {
    // Whitespace
    WS,

    // Reserved words
    Let,
    As,
    If,
    Else,
    Fn,
    Return,
    True,
    False,
    IntTypeTok(IntKind),
    Bool,
    While,
    For,
    Struct,
    Enum,
    Match,

    // Symbols
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Tilde,
    Ampersand,
    Pipe,
    Caret,
    AmpAmp,
    PipePipe,
    Plus,
    Dash,
    Star,
    Percent,
    ForwardSlash,
    Lsh,
    Rsh,
    Colon,
    ColonColon,
    Semicolon,
    Eq,
    EqEq,
    BangEq,
    Bang,
    Arrow,
    DoubleArrow,
    Comma,
    QuestionMark,
    Period,
    Underscore,

    // Literals
    IdentTok(String),
    NumberTok(u64, IntKind),
    StringTok(String),

    // Special
    Eof,
    // The following two serve as flags for the lexer (to indicate when we
    // enter and exit a multi-line comment). They should never be part of
    // the token stream it generates.
    BeginComment,
    EndComment,
}

/// A token together with a Span, to keep track of where in the source file
/// it was.
#[deriving(Show, Eq)]
pub struct SourceToken {
    pub tok: Token,
    pub sp: Span,
}

/// A single rule for the lexer. This includes a `matcher`, which matches
/// a string in the input, and a `maker`, which generates the token from
/// the matched string.
struct LexerRule<T, U> {
    matcher: T,
    maker: U,
}

// Trait to make lexer_rules! forget the type parameters of LexerRule
trait LexerRuleT {
    fn run(&self, s: &str) -> Option<(uint, Token)>;
}

// The idea behind this is to allow very flexible lexer rules in the lexer_rules!
// macro.  A LexerRule has a rule with which to check for a match, and a factory
// to produce a Token if the rule's match succeeds (some tokens like Number, String,
// and IdentTok carry additional context about what matched).  This way, we can specify
// a raw string as a rule to do a simple string-prefix match, a Regex to check
// for a regex match, or optionally more complicated rules to e.g. use capture
// groups from a Regex and construct a token from those.
impl<A, T: RuleMatcher<A>, U: TokenMaker<A>> LexerRuleT for LexerRule<T, U> {
    fn run(&self, s: &str) -> Option<(uint, Token)> {
        match self.matcher.find(s) {
            Some((len, args)) => Some((len, self.maker.mk_tok(args))),
            _ => None
        }
    }
}

pub struct Lexer<T> {
    lines: BufferLines<T>,
    line: Option<String>,
    pos: SourcePos,
    name: String,
    // Ordinary rules.
    rules: Vec<Box<LexerRuleT>>,
    // Rules specifically for when we're within a comment. We need this
    // for handling multi-line comments.
    comment_rules: Vec<Box<LexerRuleT>>,
    comment_nest: uint,
    // We set this to Some(Eof) and take it when we hit EOF
    eof: Option<Token>,
}

// Convenience for tests
pub fn lexer_from_str(s: &str) -> Lexer<io::BufferedReader<io::MemReader>> {
    use std::str::StrSlice;
    let bytes = Vec::from_slice(s.as_bytes());
    let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
    Lexer::new("test", buffer)
}

impl<T: Buffer> Lexer<T> {
    pub fn new<S: StrAllocating>(name: S, buffer: T) -> Lexer<T> {
        macro_rules! matcher { ( $e:expr ) => ( regex!(concat!("^(?:", $e, ")"))) }
        macro_rules! lexer_rules {
            ( $( $c:expr => $m:expr ),*) => (
                vec!( $( box LexerRule { matcher: $m, maker: $c } as Box<LexerRuleT> ),* )
            )
        }

        // Rule to match U32, U16, U8, I32, I16, I8
        struct IntTypeRule;
        impl RuleMatcher<IntKind> for IntTypeRule {
            fn find(&self, s: &str) -> Option<(uint, IntKind)> {
                use std::num::from_str_radix;

                let matcher = matcher!(r"[uUiI](32|16|8)");
                match matcher.captures(s) {
                    Some(groups) => {
                        let ctor = match s.char_at(0) {
                            'u' | 'U' => UnsignedInt,
                            'i' | 'I' => SignedInt,
                            _ => fail!(),
                        };

                        let w = match from_str_radix(groups.at(1), 10) {
                            Some(32) => Width32,
                            Some(16) => Width16,
                            Some(8)  => Width8,
                            _ => fail!(),
                        };

                        Some((groups.at(0).len(), ctor(w)))
                    },
                    _ => None
                }
            }
        }

        // Rule to match a numeric literal and parse it into a number
        struct NumberRule;
        impl RuleMatcher<(u64, IntKind)> for NumberRule {
            fn find(&self, s: &str) -> Option<(uint, (u64, IntKind))> {
                use std::num::from_str_radix;

                let matcher = matcher!(r"((?:0[xX]([:xdigit:]+))|(?:\d+))(?:([uUiI])(32|16|8)?)?");
                match matcher.captures(s) {
                    Some(groups) => {
                        let (num_str, radix) = match groups.at(2) {
                            ""  => (groups.at(1), 10),
                            hex => (hex, 16),
                        };

                        let s = groups.at(3);
                        let kind = if s.len() > 0 {
                            let ctor = match s.char_at(0) {
                                'u' | 'U' => UnsignedInt,
                                'i' | 'I' => SignedInt,
                                _ => fail!(),
                            };

                            let w = match from_str_radix(groups.at(4), 10) {
                                None     => AnyWidth,
                                Some(32) => Width32,
                                Some(16) => Width16,
                                Some(8)  => Width8,
                                _ => fail!(),
                            };

                            ctor(w)
                        } else {
                            GenericInt
                        };

                        Some((groups.at(0).len(), (from_str_radix(num_str, radix).take_unwrap(), kind)))
                    },
                    _ => None
                }
            }
        }

        // Rule to match a string literal and strip off the surrounding quotes
        struct StringRule;
        impl RuleMatcher<String> for StringRule {
            fn find(&self, s: &str) -> Option<(uint, String)> {
                let matcher = matcher!(r#""((?:\\"|[^"])*)""#);
                match matcher.captures(s) {
                    Some(groups) => {
                        let t = groups.at(0);
                        Some((t.len(), String::from_str(groups.at(1))))
                    },
                    _ => None
                }
            }
        }

        // Note: rules are in decreasing order of priority if there's a
        // conflict. In particular, reserved words must go before IdentTok.
        let rules = lexer_rules! {
            // Whitespace, including C-style comments
            WS         => matcher!(r"//.*|\s"),

            // The start of a multi-line comment.
            // BeginComment does not appear in the token stream.
            // Instead, it sets a counter indicating we are in a multiline comment.
            BeginComment => matcher!(r"/\*"),

            // Reserved words
            Let          => "let",
            As           => "as",
            If           => "if",
            Else         => "else",
            Fn           => "fn",
            Return       => "return",
            True         => "true",
            False        => "false",
            While        => "while",
            For          => "for",
            Struct       => "struct",
            Enum         => "enum",
            Match        => "match",

            // Basic types; TODO: add more.
            IntTypeTok   => IntTypeRule,
            Bool         => "bool",

            // Symbols
            LParen       => "(",
            RParen       => ")",
            LBrace       => "{",
            RBrace       => "}",
            LBracket     => "[",
            RBracket     => "]",
            Less         => "<",
            Greater      => ">",
            LessEq       => "<=",
            GreaterEq    => ">=",
            Tilde        => "~",
            Ampersand    => "&",
            Pipe         => "|",
            Caret        => "^",
            AmpAmp       => "&&",
            PipePipe     => "||",
            Plus         => "+",
            Dash         => "-",
            Star         => "*",
            Percent      => "%",
            ForwardSlash => "/",
            Lsh          => "<<",
            Rsh          => ">>",
            Colon        => ":",
            ColonColon   => "::",
            Semicolon    => ";",
            Eq           => "=",
            EqEq         => "==",
            BangEq       => "!=",
            Bang         => "!",
            Arrow        => "->",
            DoubleArrow  => "=>",
            Comma        => ",",
            QuestionMark => "?",
            Period       => ".",
            Underscore   => "_",

            // Literals
            IdentTok     => matcher!(r"[a-zA-Z_]\w*"),
            NumberTok    => NumberRule,
            StringTok    => StringRule
        };

        // A special set of rules, just for when we're within a multi-line
        // comment.
        let comment_rules = lexer_rules! {
            // The start of a multi-line comment.
            // This is to properly handle nested multiline comments.
            // We increase the multiline-comment-nesting-counter with this.
            BeginComment => matcher!(r"/\*"),

            // The end of a multi-line comment.
            // We decrease the multiline-comment-nesting-counter with this.
            EndComment => matcher!(r"\*/"),

            // If we're within a comment, any string not
            // containing "/*" or "*/" is considered whitespace.
            WS => matcher!(r"(?:[^*/]|/[^*]|\*[^/])*")
        };

        Lexer {
            pos:  SourcePos::new(),
            line: Some(String::new()),
            lines: BufferLines::new(buffer),
            name: name.into_strbuf(),
            rules: rules,
            comment_rules: comment_rules,
            comment_nest: 0,
            eof: Some(Eof),
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}

// The meat of the lexer (read this as a stateful flat-map)
impl<T: Buffer> Iterator<SourceToken> for Lexer<T> {
    fn next(&mut self) -> Option<SourceToken> {
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
                            Some((_, WS)) => {} // Skip whitespace.
                            Some((_, BeginComment)) => { self.comment_nest += 1; }
                            Some((_, EndComment)) => { self.comment_nest -= 1; }
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
            self.line = self.lines.next();
            self.pos = SourcePos { row: self.pos.row + 1, col: 0 };
        }
    }
}

// A TokenMaker accepts an argument tuple and hands back a token.
trait TokenMaker<T>{
    fn mk_tok(&self, args: T) -> Token;
}

// A raw Token never needs any more arguments, so accept unit and hand back itself.
impl TokenMaker<()> for Token {
    fn mk_tok(&self, _: ()) -> Token { self.clone() }
}

// A Token constructor (NumberTok, StringTok, IdentTok, etc.) requires arguments for the
// constructor to make the token, so accept a tuple of those arguments and hand back
// the constructed token.
impl<T> TokenMaker<T> for fn(T) -> Token {
    fn mk_tok(&self, arg: T) -> Token{ (*self)(arg) }
}

impl<A, B> TokenMaker<(A, B)> for fn(A, B) -> Token {
    fn mk_tok(&self, (a, b): (A, B)) -> Token{ (*self)(a, b) }
}

// A RuleMatcher accepts a string slice and tests the encapsulated rule on it.
// If there is a match it can optionally hand back a string slice corresponding
// to that match. (The "optionally" part is determined by whether or not the
// TokenMaker for this RuleMatcher requires the match as an argument, like IdentTok;
// the type magic is handled by MaybeArg).
trait RuleMatcher<T> {
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
    fn maybe_arg<T: StrAllocating>(s: T) -> String { s.into_strbuf() }
}

struct BufferLines<T> {
    buffer: T,
}

impl<T: Buffer> BufferLines<T> {
    fn new(buffer: T) -> BufferLines<T> {
        BufferLines {
            buffer: buffer,
        }
    }
}

impl<T: Buffer> Iterator<String> for BufferLines<T> {
    fn next(&mut self) -> Option<String> {
        self.buffer.read_line().ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::vec::Vec;
    use ast;
    use util::GenericInt;

    fn compare(actual: &[SourceToken], expected: &[Token]) {
        for (actual_st, expected_tok)
            in actual.iter().zip(expected.iter()) {
            assert!(actual_st.tok == *expected_tok,
                    format!("Failure:\n  found {:?}, expected {:?}\n",
                            actual_st.tok, *expected_tok));
        }
    }

    #[test]
    fn test() {
        let lexer1 = lexer_from_str(r#"f(x - /* I am a comment */ 0x3f5B)+1 "Hello\" World")"#);
        let tokens1: Vec<SourceToken> = FromIterator::from_iter(lexer1);

        compare(tokens1.as_slice(),
                vec! {
                    IdentTok(String::from_str("f")),
                    LParen,
                    IdentTok(String::from_str("x")),
                    Dash,
                    NumberTok(0x3f5B, GenericInt),
                    RParen,
                    Plus,
                    NumberTok(1, GenericInt),
                    StringTok(String::from_str(r#"Hello\" World"#)),
                }.as_slice());

        let lexer2 = lexer_from_str("let x: int = 5;");
        let tokens2: Vec<SourceToken> = FromIterator::from_iter(lexer2);
        compare(tokens2.as_slice(),
                vec! {
                    Let,
                    IdentTok(String::from_str("x")),
                    Colon,
                    IdentTok(String::from_str("int")),
                    Eq,
                    NumberTok(5, GenericInt),
                }.as_slice());
    }
}
