use super::ast;

use util::{IntKind, Width};
use std::{old_io, option, iter};

pub use util::lexer::{Language, Lexer, LexerRule, LexerRuleT};
pub use util::lexer::{RuleMatcher, SourceToken, TokenMaker};
pub use util::lexer::BufReader;

use std::fmt;
use std::fmt::{Formatter, Display, Debug};
use std::str::StrExt;

#[derive(Eq, PartialEq, Clone, Debug)]
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
    Do,
    For,
    Struct,
    Enum,
    Type,
    Match,
    Mod,
    Null,
    Break,
    Continue,
    Static,
    Extern,
    Use,
    Macro,
    Const,
    Sizeof,

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
    Dollar,
    DotDotDot,

    // Literals
    IdentTok(String),
    IdentBangTok(String),
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            Token::Let                    => String::from_str("let"),
            Token::As                     => String::from_str("as"),
            Token::If                     => String::from_str("if"),
            Token::Else                   => String::from_str("else"),
            Token::Fn                     => String::from_str("fn"),
            Token::Return                 => String::from_str("return"),
            Token::True                   => String::from_str("true"),
            Token::False                  => String::from_str("false"),
            Token::While                  => String::from_str("while"),
            Token::Do                     => String::from_str("do"),
            Token::For                    => String::from_str("for"),
            Token::Struct                 => String::from_str("struct"),
            Token::Enum                   => String::from_str("enum"),
            Token::Type                   => String::from_str("type"),
            Token::Match                  => String::from_str("match"),
            Token::Mod                    => String::from_str("mod"),
            Token::Null                   => String::from_str("null"),
            Token::Break                  => String::from_str("break"),
            Token::Continue               => String::from_str("continue"),
            Token::Static                 => String::from_str("static"),
            Token::Extern                 => String::from_str("extern"),
            Token::Use                    => String::from_str("use"),
            Token::Macro                  => String::from_str("macro"),
            Token::Const                  => String::from_str("const"),
            Token::Sizeof                 => String::from_str("sizeof"),

            Token::IntTypeTok(ik)         => format!("{}", ik),
            Token::Bool                   => String::from_str("bool"),

            Token::LParen                 => String::from_str("("),
            Token::RParen                 => String::from_str(")"),
            Token::LBrace                 => String::from_str("{"),
            Token::RBrace                 => String::from_str("}"),
            Token::LBracket               => String::from_str("["),
            Token::RBracket               => String::from_str("]"),
            Token::Less                   => String::from_str("<"),
            Token::Greater                => String::from_str(">"),
            Token::LessEq                 => String::from_str("<="),
            Token::GreaterEq              => String::from_str(">="),
            Token::Tilde                  => String::from_str("~"),
            Token::Ampersand              => String::from_str("&"),
            Token::Pipe                   => String::from_str("|"),
            Token::Caret                  => String::from_str("^"),
            Token::AmpAmp                 => String::from_str("&&"),
            Token::PipePipe               => String::from_str("||"),
            Token::Plus                   => String::from_str("+"),
            Token::Dash                   => String::from_str("-"),
            Token::Star                   => String::from_str("*"),
            Token::Percent                => String::from_str("%"),
            Token::ForwardSlash           => String::from_str("/"),
            Token::Lsh                    => String::from_str("<<"),
            Token::Rsh                    => String::from_str(">>"),
            Token::Colon                  => String::from_str(":"),
            Token::ColonColon             => String::from_str("::"),
            Token::Semicolon              => String::from_str(";"),
            Token::Eq                     => String::from_str("="),
            Token::EqEq                   => String::from_str("=="),
            Token::BangEq                 => String::from_str("!="),
            Token::Bang                   => String::from_str("!"),
            Token::Arrow                  => String::from_str("->"),
            Token::DoubleArrow            => String::from_str("=>"),
            Token::Comma                  => String::from_str(","),
            Token::QuestionMark           => String::from_str("?"),
            Token::Period                 => String::from_str("."),
            Token::Underscore             => String::from_str("_"),
            Token::PlusEq                 => String::from_str("+="),
            Token::MinusEq                => String::from_str("-="),
            Token::TimesEq                => String::from_str("*="),
            Token::SlashEq                => String::from_str("/="),
            Token::PipeEq                 => String::from_str("|="),
            Token::CaretEq                => String::from_str("^="),
            Token::AmpEq                  => String::from_str("&="),
            Token::LshEq                  => String::from_str("<<="),
            Token::RshEq                  => String::from_str(">>="),
            Token::PercentEq              => String::from_str("%="),
            Token::Dollar                 => String::from_str("$"),
            Token::DotDotDot              => String::from_str("..."),

            Token::IdentTok(ref id)       => format!("{}", id),
            Token::IdentBangTok(ref id)   => format!("{}!", id),
            Token::NumberTok(n, ik)       => format!("{}{}", n, ik),
            Token::StringTok(ref s)       => format!("\"{}\"", s.escape_default()),

            Token::WS                     => String::from_str(" "),
            Token::Eof                    => String::from_str("<EOF>"),
            Token::BeginComment           => String::from_str("/*"),
            Token::EndComment             => String::from_str("*/"),
        };

        write!(f, "{}", s)
    }
}

// Convenience for tests
pub fn lexer_from_str(s: &str) -> Lexer<old_io::BufferedReader<old_io::MemReader>, Token> {
    let bytes = s.as_bytes().to_vec();
    let buffer = old_io::BufferedReader::new(old_io::MemReader::new(bytes));
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
                let ctor: fn(Width) -> IntKind = match s.char_at(0) {
                    'u' | 'U' => IntKind::UnsignedInt,
                    'i' | 'I' => IntKind::SignedInt,
                    _ => panic!(),
                };

                let w = match from_str_radix::<u8>(groups.at(1).unwrap(), 10) {
                    Some(32) => Width::Width32,
                    Some(16) => Width::Width16,
                    Some(8)  => Width::Width8,
                    _ => panic!(),
                };

                Some((groups.at(0).unwrap().len(), ctor(w)))
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
                let (num_str, radix) = match groups.at(2).unwrap() {
                    ""  => (groups.at(1).unwrap(), 10),
                    hex => (hex, 16),
                };

                let s = groups.at(3).unwrap();
                let kind = if s.len() > 0 {
                    let ctor: fn(Width) -> IntKind = match s.char_at(0) {
                        'u' | 'U' => IntKind::UnsignedInt,
                        'i' | 'I' => IntKind::SignedInt,
                        _ => panic!(),
                    };

                    let w = match from_str_radix::<u8>(groups.at(4).unwrap(), 10) {
                        None     => Width::AnyWidth,
                        Some(32) => Width::Width32,
                        Some(16) => Width::Width16,
                        Some(8)  => Width::Width8,
                        _ => panic!(),
                    };

                    ctor(w)
                } else {
                    IntKind::GenericInt
                };

                Some((groups.at(0).unwrap().len(), (from_str_radix(num_str, radix).unwrap(), kind)))
            },
            _ => None
        }
    }
}

// Rule to match a name followed by a Bang and strip off the trailing Bang
struct IdentBangRule;
impl RuleMatcher<String> for IdentBangRule {
    fn find(&self, s: &str) -> Option<(uint, String)> {
        let matcher = matcher!(r"([a-zA-Z_]\w*)!");
        match matcher.captures(s) {
           Some(groups) => {
                let t = groups.at(0).unwrap();
                Some((t.len(), String::from_str(groups.at(1).unwrap())))
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
                use rust_syntax::parse::str_lit;
                let t = groups.at(0).unwrap();
                Some((t.len(), str_lit(groups.at(1).unwrap())))
           },
            _ => None
        }
    }
}

struct CharRule;
impl RuleMatcher<(u64, IntKind)> for CharRule {
    fn find(&self, s: &str) -> Option<(uint, (u64, IntKind))> {
        let matcher = matcher!(r#"'((?:\\["nrt'\\]|\\[xX][0-9a-fA-F]+|[^']))'"#);
        match matcher.captures(s) {
           Some(groups) => {
               use rust_syntax::parse::char_lit;
               let t = groups.at(0).unwrap();
               let (c, _) = char_lit(groups.at(1).unwrap());
               Some((t.len(), (c as u64, IntKind::UnsignedInt(Width::Width8))))
           },
            _ => None
        }
    }
}

pub fn new_mb_lexer<'a, S: ?Sized + ToString, B: BufReader>(name: &S, buffer: B) -> Lexer<'a, B, Token> {
    let lang = Language {
        eof: Token::Eof,
        ws: Token::WS,
        begin_comment: Token::BeginComment,
        end_comment: Token::EndComment,

        // Note: rules are in decreasing order of priority if there's a
        // conflict. In particular, reserved words must go before IdentTok.
        rules: lexer_rules! {
            // Whitespace, including C-style comments
            Token::WS         => matcher!(r"//.*|\s"),

            // The start of a multi-line comment.
            // BeginComment does not appear in the token stream.
            // Instead, it sets a counter indicating we are in a multiline comment.
            Token::BeginComment => matcher!(r"/\*"),

            // Reserved words
            Token::Let          => "let",
            Token::As           => "as",
            Token::If           => "if",
            Token::Else         => "else",
            Token::Fn           => "fn",
            Token::Return       => "return",
            Token::True         => "true",
            Token::False        => "false",
            Token::While        => "while",
            Token::Do           => "do",
            Token::For          => "for",
            Token::Struct       => "struct",
            Token::Enum         => "enum",
            Token::Type         => "type",
            Token::Match        => "match",
            Token::Mod          => "mod",
            Token::Null         => "null",
            Token::Break        => "break",
            Token::Continue     => "continue",
            Token::Static       => "static",
            Token::Extern       => "extern",
            Token::Use          => "use",
            Token::Macro        => "macro",
            Token::Const        => "const",
            Token::Sizeof       => "sizeof",

            // Basic types; TODO: add more.
            Token::IntTypeTok   => IntTypeRule,
            Token::Bool         => "bool",

            // Symbols
            Token::LParen       => "(",
            Token::RParen       => ")",
            Token::LBrace       => "{",
            Token::RBrace       => "}",
            Token::LBracket     => "[",
            Token::RBracket     => "]",
            Token::Less         => "<",
            Token::Greater      => ">",
            Token::LessEq       => "<=",
            Token::GreaterEq    => ">=",
            Token::Tilde        => "~",
            Token::Ampersand    => "&",
            Token::Pipe         => "|",
            Token::Caret        => "^",
            Token::AmpAmp       => "&&",
            Token::PipePipe     => "||",
            Token::Plus         => "+",
            Token::Dash         => "-",
            Token::Star         => "*",
            Token::Percent      => "%",
            Token::ForwardSlash => "/",
            Token::Lsh          => "<<",
            Token::Rsh          => ">>",
            Token::Colon        => ":",
            Token::ColonColon   => "::",
            Token::Semicolon    => ";",
            Token::Eq           => "=",
            Token::EqEq         => "==",
            Token::BangEq       => "!=",
            Token::Bang         => "!",
            Token::Arrow        => "->",
            Token::DoubleArrow  => "=>",
            Token::Comma        => ",",
            Token::QuestionMark => "?",
            Token::Period       => ".",
            Token::Underscore   => "_",
            Token::PlusEq       => "+=",
            Token::MinusEq      => "-=",
            Token::TimesEq      => "*=",
            Token::SlashEq      => "/=",
            Token::PipeEq       => "|=",
            Token::CaretEq      => "^=",
            Token::AmpEq        => "&=",
            Token::LshEq        => "<<=",
            Token::RshEq        => ">>=",
            Token::PercentEq    => "%=",
            Token::Dollar       => "$",
            Token::DotDotDot    => "...",

            // Literals
            Token::IdentTok     => matcher!(r"[a-zA-Z_]\w*"),
            Token::IdentBangTok => IdentBangRule,
            |&: (n, ik)| Token::NumberTok(n, ik)    => NumberRule,
            |&: (n, ik)| Token::NumberTok(n, ik)    => CharRule,
            Token::StringTok    => StringRule
        },

        // A special set of rules, just for when we're within a multi-line
        // comment.
        comment_rules: lexer_rules! {
            // The start of a multi-line comment.
            // This is to properly handle nested multiline comments.
            // We increase the multiline-comment-nesting-counter with this.
            Token::BeginComment => matcher!(r"/\*"),

            // The end of a multi-line comment.
            // We decrease the multiline-comment-nesting-counter with this.
            Token::EndComment => matcher!(r"\*/"),

            // If we're within a comment, any string not
            // containing "/*" or "*/" is considered whitespace.
            Token::WS => matcher!(r"(?:.|\n)")
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
impl<T, F: Fn(T) -> Token> TokenMaker<T, Token> for F {
    fn mk_tok(&self, arg: T) -> Token { (*self)(arg) }
}

#[cfg(test)]
mod tests {
    use util::GenericInt;

    use super::SourceToken as ST;
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
                    StringTok(String::from_str(r#"Hello" World"#)),
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
