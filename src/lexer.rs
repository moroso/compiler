/* The lexer.
 * This takes an interator of streams, and gives us an iterator of tokens.
 * Each token has an associated "span", which tells us where in the source
 * file the token was.
 */

use ast;
use span::{Span, SourcePos, mk_sp};
use regex::Regex;
use std::iter;
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
    IntTypeTok(ast::IntKind),
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
    IdentTok(~str),
    NumberTok(u64, ast::IntKind),
    StringTok(~str),

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

struct LineContext {
    pos: SourcePos,
    line: ~str,
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
    iter: iter::Enumerate<T>,
    linectx: Option<LineContext>,
    // Ordinary rules.
    rules: Vec<~LexerRuleT>,
    // Rules specifically for when we're within a comment. We need this
    // for handling multi-line comments.
    comment_rules: Vec<~LexerRuleT>,
    in_comment: bool,
}

impl<T: Iterator<~str>> Lexer<T> {
    pub fn new(line_iter: T) -> Lexer<T> {
        macro_rules! matcher { ( $e:expr ) => ( regex!(concat!("^(?:", $e, ")"))) }
        macro_rules! lexer_rules {
            ( $( $c:expr => $m:expr ),*) => (
                vec!( $( ~LexerRule { matcher: $m, maker: $c } as ~LexerRuleT ),* )
            )
        }

        // Rule to match U32, U16, U8, I32, I16, I8
        struct IntTypeRule;
        impl RuleMatcher<ast::IntKind> for IntTypeRule {
            fn find(&self, s: &str) -> Option<(uint, ast::IntKind)> {
                use std::num::from_str_radix;

                let matcher = matcher!(r"[uUiI](32|16|8)");
                match matcher.captures(s) {
                    Some(groups) => {
                        let ctor = match s.char_at(0) {
                            'u' | 'U' => ast::UnsignedInt,
                            'i' | 'I' => ast::SignedInt,
                            _ => fail!(),
                        };

                        let w = match from_str_radix(groups.at(1), 10) {
                            Some(32) => ast::Width32,
                            Some(16) => ast::Width16,
                            Some(8)  => ast::Width8,
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
        impl RuleMatcher<(u64, ast::IntKind)> for NumberRule {
            fn find(&self, s: &str) -> Option<(uint, (u64, ast::IntKind))> {
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
                                'u' | 'U' => ast::UnsignedInt,
                                'i' | 'I' => ast::SignedInt,
                                _ => fail!(),
                            };

                            let w = match from_str_radix(groups.at(4), 10) {
                                None     => ast::AnyWidth,
                                Some(32) => ast::Width32,
                                Some(16) => ast::Width16,
                                Some(8)  => ast::Width8,
                                _ => fail!(),
                            };

                            ctor(w)
                        } else {
                            ast::GenericInt
                        };

                        Some((groups.at(0).len(), (from_str_radix(num_str, radix).take_unwrap(), kind)))
                    },
                    _ => None
                }
            }
        }

        // Rule to match a string literal and strip off the surrounding quotes
        struct StringRule;
        impl RuleMatcher<~str> for StringRule {
            fn find(&self, s: &str) -> Option<(uint, ~str)> {
                let matcher = matcher!(r#""((?:\\"|[^"])*)""#);
                match matcher.captures(s) {
                    Some(groups) => {
                        let t = groups.at(0);
                        Some((t.len(), groups.at(1).to_owned()))
                    },
                    _ => None
                }
            }
        }

        // Note: rules are in decreasing order of priority if there's a
        // conflict. In particular, reserved words must go before IdentTok.
        let rules = lexer_rules! {
            // Whitespace, including comments.
            // From left to right, the parts of this match:
            // - actual whitespace,
            // - // style comments
            // - /* */ style comments. This clause is a little more complicated,
            //     because we have to be sure the match doesn't contain "*/"
            //     except at the very end. This is what ([^*]|\*[^/]) takes
            //     care of.
            WS         => matcher!(r"\s|//.*|(?s)/\*([^*]|\*[^/])*\*/"),

            // A multi-line comment that's begun, but not ended.
            // This consists of a string starting with "/*" but not
            // containing "*/". The actual lexer body treats a "BeginComment"
            // token specially: it sets a flag that we're inside a multi-line
            // comment. BeginComment does not appear in the token stream.
            BeginComment => matcher!(r"/\*([^*]|\*[^/])*$"),

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
            // The end of a multi-line comment. This is any sequence of
            // characers not containing "*/" (matched by ([^*]|\*[^/])* ),
            // followed by "*/". The lexer treats the EndComment token
            // specially: it causes it to clear the flag that indicates
            // we're in a multi-line comment. EndComment does not appear
            // in the token stream.
            EndComment => matcher!(r"([^*]|\*[^/])*\*/"),

            // If we're within a comment, any string not
            // containing "*/" is considered whitespace.
            WS => matcher!(r"([^*]|\*[^/])*")
        };

        Lexer {
            iter: line_iter.enumerate(),
            linectx: None,
            rules: rules,
            comment_rules: comment_rules,
            in_comment: false,
        }
    }
}

// The meat of the lexer (read this as a stateful flat-map)
impl<T: Iterator<~str>> Iterator<SourceToken> for Lexer<T> {
    fn next(&mut self) -> Option<SourceToken> {
        loop {
            for lc in self.linectx.mut_iter() {
                while lc.pos.col < lc.line.len() {
                    // We apply each rule. Of the ones that match, we take
                    // the longest match.
                    let mut longest = 0u;
                    let mut best = None;
                    let mut rules = if self.in_comment {
                        &self.comment_rules
                    } else {
                        &self.rules
                    };

                    for rule in rules.iter() {
                        let m = rule.run(lc.line.slice_from(lc.pos.col));
                        match m {
                            Some((len, tok)) => {
                                if len > longest {
                                    // We have a match that's longer than our
                                    // previous one. Remember it.
                                    best = Some((mk_sp(lc.pos, len), tok));
                                    longest = len;
                                }
                            },
                            _ => {},
                        }
                    }

                    // Advance our position within the line.
                    lc.pos.col += longest;

                    match best {
                        None => fail!("Unexpected input"),
                        Some((_, WS)) => {} // Skip whitespace.
                        Some((_, BeginComment)) => { self.in_comment = true; }
                        Some((_, EndComment)) => { self.in_comment = false; }
                        Some((sp, tok)) => {
                            return Some(SourceToken {
                                tok: tok,
                                sp: sp,
                            })
                        }
                    }
                }
            }

            // Fetch a new line, now that we're done with the previous one.
            match self.iter.next() {
                None => return None,
                Some((row, line)) => {
                    self.linectx = Some(LineContext {
                        pos: SourcePos { row: row, col: 0 },
                        line: line,
                    });
                }
            }
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
    fn maybe_arg<'a>(arg: &'a str) -> Self;
}

impl MaybeArg for () {
    fn maybe_arg<'a>(_: &'a str) { }
}

impl MaybeArg for ~str {
    fn maybe_arg<'a>(s: &'a str) -> ~str { s.to_owned() }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::Repeat;
    use std::vec::Vec;
    use ast;

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
        let lexer1 = Lexer::new(vec!(r#"f(x - /* I am a comment */ 0x3f5B)+1 "Hello\" World")"#.to_owned()).move_iter());
        let tokens1: ~[SourceToken] = FromIterator::from_iter(lexer1);

        compare(tokens1,
                [IdentTok("f".to_owned()),
                  LParen,
                  IdentTok("x".to_owned()),
                  Dash,
                  NumberTok(0x3f5B, ast::GenericInt),
                  RParen,
                  Plus,
                  NumberTok(1, ast::GenericInt),
                  StringTok(r#"Hello\" World"#.to_owned()),
                ]);

        let lexer2 = Lexer::new(vec!("let x: int = 5;".to_owned()).move_iter());
        let tokens2: ~[SourceToken] = FromIterator::from_iter(lexer2);
        compare(tokens2,
                [Let,
                 IdentTok("x".to_owned()),
                 Colon,
                 IdentTok("int".to_owned()),
                 Eq,
                 NumberTok(5, ast::GenericInt),
                 ]);
    }
}
