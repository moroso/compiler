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
            Token::Let                    => "let".to_string(),
            Token::As                     => "as".to_string(),
            Token::If                     => "if".to_string(),
            Token::Else                   => "else".to_string(),
            Token::Fn                     => "fn".to_string(),
            Token::Return                 => "return".to_string(),
            Token::True                   => "true".to_string(),
            Token::False                  => "false".to_string(),
            Token::While                  => "while".to_string(),
            Token::Do                     => "do".to_string(),
            Token::For                    => "for".to_string(),
            Token::Struct                 => "struct".to_string(),
            Token::Enum                   => "enum".to_string(),
            Token::Type                   => "type".to_string(),
            Token::Match                  => "match".to_string(),
            Token::Mod                    => "mod".to_string(),
            Token::Null                   => "null".to_string(),
            Token::Break                  => "break".to_string(),
            Token::Continue               => "continue".to_string(),
            Token::Static                 => "static".to_string(),
            Token::Extern                 => "extern".to_string(),
            Token::Use                    => "use".to_string(),
            Token::Macro                  => "macro".to_string(),
            Token::Const                  => "const".to_string(),
            Token::Sizeof                 => "sizeof".to_string(),

            Token::IntTypeTok(ik)         => format!("{}", ik),
            Token::Bool                   => "bool".to_string(),

            Token::LParen                 => "(".to_string(),
            Token::RParen                 => ")".to_string(),
            Token::LBrace                 => "{".to_string(),
            Token::RBrace                 => "}".to_string(),
            Token::LBracket               => "[".to_string(),
            Token::RBracket               => "]".to_string(),
            Token::Less                   => "<".to_string(),
            Token::Greater                => ">".to_string(),
            Token::LessEq                 => "<=".to_string(),
            Token::GreaterEq              => ">=".to_string(),
            Token::Tilde                  => "~".to_string(),
            Token::Ampersand              => "&".to_string(),
            Token::Pipe                   => "|".to_string(),
            Token::Caret                  => "^".to_string(),
            Token::AmpAmp                 => "&&".to_string(),
            Token::PipePipe               => "||".to_string(),
            Token::Plus                   => "+".to_string(),
            Token::Dash                   => "-".to_string(),
            Token::Star                   => "*".to_string(),
            Token::Percent                => "%".to_string(),
            Token::ForwardSlash           => "/".to_string(),
            Token::Lsh                    => "<<".to_string(),
            Token::Rsh                    => ">>".to_string(),
            Token::Colon                  => ":".to_string(),
            Token::ColonColon             => "::".to_string(),
            Token::Semicolon              => ";".to_string(),
            Token::Eq                     => "=".to_string(),
            Token::EqEq                   => "==".to_string(),
            Token::BangEq                 => "!=".to_string(),
            Token::Bang                   => "!".to_string(),
            Token::Arrow                  => "->".to_string(),
            Token::DoubleArrow            => "=>".to_string(),
            Token::Comma                  => ",".to_string(),
            Token::QuestionMark           => "?".to_string(),
            Token::Period                 => ".".to_string(),
            Token::Underscore             => "_".to_string(),
            Token::PlusEq                 => "+=".to_string(),
            Token::MinusEq                => "-=".to_string(),
            Token::TimesEq                => "*=".to_string(),
            Token::SlashEq                => "/=".to_string(),
            Token::PipeEq                 => "|=".to_string(),
            Token::CaretEq                => "^=".to_string(),
            Token::AmpEq                  => "&=".to_string(),
            Token::LshEq                  => "<<=".to_string(),
            Token::RshEq                  => ">>=".to_string(),
            Token::PercentEq              => "%=".to_string(),
            Token::Dollar                 => "$".to_string(),
            Token::DotDotDot              => "...".to_string(),

            Token::IdentTok(ref id)       => format!("{}", id),
            Token::IdentBangTok(ref id)   => format!("{}!", id),
            Token::NumberTok(n, ik)       => format!("{}{}", n, ik),
            Token::StringTok(ref s)       => format!("\"{}\"", s.escape_default()),

            Token::WS                     => " ".to_string(),
            Token::Eof                    => "<EOF>".to_string(),
            Token::BeginComment           => "/*".to_string(),
            Token::EndComment             => "*/".to_string(),
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
        vec! ( $( box LexerRule::new($m, $c) as Box<LexerRuleT<Token>> ),* )
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
                    Ok(32) => Width::Width32,
                    Ok(16) => Width::Width16,
                    Ok(8)  => Width::Width8,
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
                        Err(_) => Width::AnyWidth,
                        Ok(32) => Width::Width32,
                        Ok(16) => Width::Width16,
                        Ok(8) => Width::Width8,
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
                Some((t.len(), groups.at(1).unwrap().to_string()))
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
            |(n, ik)| Token::NumberTok(n, ik)    => NumberRule,
            |(n, ik)| Token::NumberTok(n, ik)    => CharRule,
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

        compare(&tokens1[..],
                vec! {
                    IdentTok("f".to_string()),
                    LParen,
                    IdentTok("x".to_string()),
                    Dash,
                    NumberTok(0x3f5B, GenericInt),
                    RParen,
                    Plus,
                    NumberTok(1, GenericInt),
                    StringTok(r#"Hello" World"#).to_string(),
                }.as_slice());

        let lexer2 = lexer_from_str("let x: int = 5;");
        let tokens2: Vec<SourceToken> = FromIterator::from_iter(lexer2);
        compare(&tokens2[..],
                vec! {
                    Let,
                    IdentTok("x".to_string()),
                    Colon,
                    IdentTok("int".to_string()),
                    Eq,
                    NumberTok(5, GenericInt),
                }.as_slice());
    }
}
