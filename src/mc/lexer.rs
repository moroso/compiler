use super::ast;

use util::{IntKind, GenericInt, SignedInt, UnsignedInt};
use util::{Width, AnyWidth, Width8, Width16, Width32};
use std::{io, option, iter};

pub use util::lexer::{Language, Lexer, LexerRule, LexerRuleT};
pub use util::lexer::{RuleMatcher, SourceToken, TokenMaker};

#[deriving(Eq, PartialEq, Clone, Show)]
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
    Mod,
    Null,
    Break,
    Continue,
    Static,
    Extern,
    Use,

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
    PlusEq,
    MinusEq,
    TimesEq,
    SlashEq,
    PipeEq,
    CaretEq,
    AmpEq,
    LshEq,
    RshEq,
    PercentEq,

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

// Convenience for tests
pub fn lexer_from_str(s: &str) -> Lexer<io::BufferedReader<io::MemReader>, Token> {
    use std::str::StrSlice;
    let bytes = Vec::from_slice(s.as_bytes());
    let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
    new_mb_lexer("test", buffer)
}

macro_rules! matcher { ( $e:expr ) => ( regex!(concat!("^(?:", $e, ")"))) }
macro_rules! lexer_rules {
    ( $( $c:expr => $m:expr ),*) => (
        vec! ( $( box LexerRule { matcher: $m, maker: $c } as Box<LexerRuleT<Token>> ),* )
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

pub fn new_mb_lexer<S: StrAllocating, B: Buffer>(name: S, buffer: B) -> Lexer<B, Token> {
    let lang = Language {
        eof: Eof,
        ws: WS,
        begin_comment: BeginComment,
        end_comment: EndComment,

        // Note: rules are in decreasing order of priority if there's a
        // conflict. In particular, reserved words must go before IdentTok.
        rules: lexer_rules! {
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
            Mod          => "mod",
            Null         => "null",
            Break        => "break",
            Continue     => "continue",
            Static       => "static",
            Extern       => "extern",
            Use          => "use",

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
            PlusEq       => "+=",
            MinusEq      => "-=",
            TimesEq      => "*=",
            SlashEq      => "/=",
            PipeEq       => "|=",
            CaretEq      => "^=",
            AmpEq        => "&=",
            LshEq        => "<<=",
            RshEq        => ">>=",
            PercentEq    => "%=",

            // Literals
            IdentTok     => matcher!(r"[a-zA-Z_]\w*"),
            NumberTok    => NumberRule,
            StringTok    => StringRule
        },

        // A special set of rules, just for when we're within a multi-line
        // comment.
        comment_rules: lexer_rules! {
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
        },
    };

    Lexer::new(lang, name, buffer)
}

// A raw Token never needs any more arguments, so accept unit and hand back itself.
impl TokenMaker<(), Token> for Token {
    fn mk_tok(&self, _: ()) -> Token { self.clone() }
}

// A Token constructor (NumberTok, StringTok, IdentTok, etc.) requires arguments for the
// constructor to make the token, so accept a tuple of those arguments and hand back
// the constructed token.
impl<T> TokenMaker<T, Token> for fn(T) -> Token {
    fn mk_tok(&self, arg: T) -> Token{ (*self)(arg) }
}

impl<A, B> TokenMaker<(A, B), Token> for fn(A, B) -> Token {
    fn mk_tok(&self, (a, b): (A, B)) -> Token{ (*self)(a, b) }
}

#[cfg(test)]
mod tests {
    use util::GenericInt;

    use ST = super::SourceToken;
    use super::super::ast;

    use std::vec::Vec;

    use super::*;

    type SourceToken = ST<Token>;

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