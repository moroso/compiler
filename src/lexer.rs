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
    U32,
    I32,

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
    Xor,
    AmpAmp,
    PipePipe,
    Plus,
    Dash,
    Star,
    ForwardSlash,
    Lsh,
    Rsh,
    Colon,
    ColonColon,
    Semicolon,
    Eq,
    EqEq,
    Bang,
    Arrow,
    Comma,
    QuestionMark,
    Period,

    // Literals
    Ident(~str),
    Number(u64, Option<ast::IntKind>),
    String(~str),

    // Special
    Eof,
}

#[deriving(Show, Eq)]
pub struct SourceToken {
    pub tok: Token,
    pub sp: Span,
}

struct LineContext {
    pos: SourcePos,
    line: ~str,
}

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
// and Ident carry additional context about what matched).  This way, we can specify
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
    rules: Vec<~LexerRuleT>,
}

impl<T: Iterator<~str>> Lexer<T> {
    pub fn new(line_iter: T) -> Lexer<T> {
        macro_rules! matcher { ( $e:expr ) => ( regex!(concat!("^(?:", $e, ")"))) }
        macro_rules! lexer_rules {
            ( $( $c:expr => $m:expr ),*) => (
                vec!( $( ~LexerRule { matcher: $m, maker: $c } as ~LexerRuleT ),* )
            )
        }

        // Rule to match a numeric literal and parse it into a number
        struct NumberRule;
        impl RuleMatcher<(u64, Option<ast::IntKind>)> for NumberRule {
            fn find(&self, s: &str) -> Option<(uint, (u64, Option<ast::IntKind>))> {
                use std::num::from_str_radix;

                let matcher = matcher!(r"((?:0[xX]([:xdigit:]+))|(?:\d+))(?:([uUiI])(32|16|8))?");
                match matcher.captures(s) {
                    Some(groups) => {
                        let t = groups.at(1);
                        let (num_str, radix) = match groups.at(2) {
                            ""  => (t, 10),
                            hex => (hex, 16),
                        };

                        let s = groups.at(3);
                        let kind = if s.len() > 0 {
                            let s = match s.char_at(0) {
                                'u' | 'U' => ast::Unsigned,
                                'i' | 'I' => ast::Signed,
                                _ => fail!(),
                            };

                            let w = match from_str_radix(groups.at(4), 10) {
                                Some(32) => ast::Width32,
                                Some(16) => ast::Width16,
                                Some(8)  => ast::Width8,
                                _ => fail!(),
                            };

                            Some(ast::IntKind { signedness: s, width: w })
                        } else {
                            None
                        };

                        Some((t.len(), (from_str_radix(num_str, radix).take_unwrap(), kind)))
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
        // conflict. In particular, reserved words must go before Ident.
        let rules = lexer_rules! {
            // Whitespace
            WS         => matcher!(r"\s|//.*|(?s)/\*.*\*/"),

            // Reserved words
            Let        => "let",
            As         => "as",
            If         => "if",
            Else       => "else",
            Fn         => "fn",
            Return     => "return",
            True       => "true",
            False      => "false",

            // Basic types; TODO: add more.
            I32        => matcher!(r"[iI]32"),
            U32        => matcher!(r"[uU]32"),

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
            Xor          => "^",
            AmpAmp       => "&&",
            PipePipe     => "||",
            Plus         => "+",
            Dash         => "-",
            Star         => "*",
            ForwardSlash => "/",
            Lsh          => "<<",
            Rsh          => ">>",
            Colon        => ":",
            ColonColon   => "::",
            Semicolon    => ";",
            Eq           => "=",
            EqEq         => "==",
            Bang         => "!",
            Arrow        => "->",
            Comma        => ",",
            QuestionMark => "?",
            Period       => ".",

            // Literals
            Ident      => matcher!(r"[a-zA-Z_]\w*"),
            Number     => NumberRule,
            String     => StringRule
        };

        Lexer {
            iter: line_iter.enumerate(),
            linectx: None,
            rules: rules
        }
    }
}

// The meat of the lexer (read this as a stateful flat-map)
impl<T: Iterator<~str>> Iterator<SourceToken> for Lexer<T> {
    fn next(&mut self) -> Option<SourceToken> {
        loop {
            for lc in self.linectx.mut_iter() {
                while lc.pos.col < lc.line.len() {
                    let mut longest = 0u;
                    let mut best = None;
                    for rule in self.rules.iter() {
                        let m = rule.run(lc.line.slice_from(lc.pos.col));
                        match m {
                            Some((len, tok)) => {
                                if len > longest {
                                    best = Some((mk_sp(lc.pos, len), tok));
                                    longest = len;
                                }
                            },
                            _ => {},
                        }
                    }

                    lc.pos.col += longest;

                    match best {
                        None => fail!("Unexpected input"),
                        Some((_, WS)) => {}
                        Some((sp, tok)) => {
                            return Some(SourceToken {
                                tok: tok,
                                sp: sp,
                            })
                        }
                    }
                }
            }

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

// A Token constructor (Number, String, Ident, etc.) requires arguments for the
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
// TokenMaker for this RuleMatcher requires the match as an argument, like Ident;
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
        let lexer1 = Lexer::new(vec!(~r#"f(x - /* I am a comment */ 0x3f5B)+1 "Hello\" World")"#).move_iter());
        let tokens1: ~[SourceToken] = FromIterator::from_iter(lexer1);

        compare(tokens1,
                [Ident(~"f"),
                  LParen,
                  Ident(~"x"),
                  Dash,
                  Number(0x3f5B, None),
                  RParen,
                  Plus,
                  Number(1, None),
                  String(~r#"Hello\" World"#),
                ]);

        let lexer2 = Lexer::new(vec!(~"let x: int = 5;").move_iter());
        let tokens2: ~[SourceToken] = FromIterator::from_iter(lexer2);
        compare(tokens2,
                [Let,
                 Ident(~"x"),
                 Colon,
                 Ident(~"int"),
                 Eq,
                 Number(5, None),
                 ]);
    }
}
