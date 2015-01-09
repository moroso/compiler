use std::{fmt};
use std::borrow::IntoCow;

use mclib::lexer::{Language, Lexer};
use mclib::util::{Width, IntKind};

macro_rules! matcher { ( $e:expr ) => ( regex!(concat!("^(?:", $e, ")"))) }
macro_rules! lexer_rules {
    ( $( $c:expr => $m:expr ),+,) => ( lexer_rules!($($c => $m),*) );
    ( $( $c:expr => $m:expr ),*) => (
        vec![ $( 
            {
                use mclib::lexer::RuleMatcher;

                fn rule(s: &str) -> Option<(uint, Token)> {
                    $m.check(s).map(|(len, args)| (len, fn_call($c, args)))
                }

                rule as fn(&str) -> Option<(uint, Token)>
            }
        ),* ]
    )
}

// Pretend to be a token with a zero-argument constructor
impl FnOnce() -> Token for Token {
    extern "rust-call" fn call_once(self, _:()) -> Token { self }
}

// Workaround for a rustc bug
#[inline(always)]
fn fn_call<A, F: FnOnce<A, Token>>(f: F, args: A) -> Token {
    f.call_once(args)
}

#[derive(Eq, PartialEq, Clone)]
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

impl<'a> fmt::Show for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            Token::Let                    => "let",
            Token::As                     => "as",
            Token::If                     => "if",
            Token::Else                   => "else",
            Token::Fn                     => "fn",
            Token::Return                 => "return",
            Token::True                   => "true",
            Token::False                  => "false",
            Token::While                  => "while",
            Token::Do                     => "do",
            Token::For                    => "for",
            Token::Struct                 => "struct",
            Token::Enum                   => "enum",
            Token::Type                   => "type",
            Token::Match                  => "match",
            Token::Mod                    => "mod",
            Token::Null                   => "null",
            Token::Break                  => "break",
            Token::Continue               => "continue",
            Token::Static                 => "static",
            Token::Extern                 => "extern",
            Token::Use                    => "use",
            Token::Macro                  => "macro",
            Token::Const                  => "const",
            Token::Sizeof                 => "sizeof",

            Token::IntTypeTok(ik)         => return write!(f, "{}", ik),
            Token::Bool                   => "bool",

            Token::LParen                 => "(",
            Token::RParen                 => ")",
            Token::LBrace                 => "{",
            Token::RBrace                 => "}",
            Token::LBracket               => "[",
            Token::RBracket               => "]",
            Token::Less                   => "<",
            Token::Greater                => ">",
            Token::LessEq                 => "<=",
            Token::GreaterEq              => ">=",
            Token::Tilde                  => "~",
            Token::Ampersand              => "&",
            Token::Pipe                   => "|",
            Token::Caret                  => "^",
            Token::AmpAmp                 => "&&",
            Token::PipePipe               => "||",
            Token::Plus                   => "+",
            Token::Dash                   => "-",
            Token::Star                   => "*",
            Token::Percent                => "%",
            Token::ForwardSlash           => "/",
            Token::Lsh                    => "<<",
            Token::Rsh                    => ">>",
            Token::Colon                  => ":",
            Token::ColonColon             => "::",
            Token::Semicolon              => ";",
            Token::Eq                     => "=",
            Token::EqEq                   => "==",
            Token::BangEq                 => "!=",
            Token::Bang                   => "!",
            Token::Arrow                  => "->",
            Token::DoubleArrow            => "=>",
            Token::Comma                  => ",",
            Token::QuestionMark           => "?",
            Token::Period                 => ".",
            Token::Underscore             => "_",
            Token::PlusEq                 => "+=",
            Token::MinusEq                => "-=",
            Token::TimesEq                => "*=",
            Token::SlashEq                => "/=",
            Token::PipeEq                 => "|=",
            Token::CaretEq                => "^=",
            Token::AmpEq                  => "&=",
            Token::LshEq                  => "<<=",
            Token::RshEq                  => ">>=",
            Token::PercentEq              => "%=",
            Token::Dollar                 => "$",
            Token::DotDotDot              => "...",

            Token::IdentTok(ref id)       => return write!(f, "{}", id),
            Token::IdentBangTok(ref id)   => return write!(f, "{}!", id),
            Token::NumberTok(n, ik)       => return write!(f, "{}{}", n, ik),
            Token::StringTok(ref s)       => return write!(f, "\"{}\"", s.escape_default()),

            Token::WS                     => " ",
            Token::Eof                    => "<EOF>",
            Token::BeginComment           => "/*",
            Token::EndComment             => "*/",
        };

        write!(f, "{}", s)
    }
}

pub fn new_mb_lexer<'s, S, B>(name: S, buffer: B) -> Lexer<B, Token>
        where S: IntoCow<'s, String, str>, B: Reader {
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
            Token::IntTypeTok   => |&:s| {
                use std::num::from_str_radix;

                let matcher = matcher!(r"[uUiI](32|16|8)");
                match matcher.captures(s) {
                    Some(groups) => {
                        let ctor: fn(Width) -> IntKind = match s.char_at(0) {
                            'u' | 'U' => IntKind::UnsignedInt,
                            'i' | 'I' => IntKind::SignedInt,
                            _ => panic!(),
                        };

                        let w = match from_str_radix::<u8>(groups.at(1).unwrap_or(""), 10) {
                            Some(32) => Width::Width32,
                            Some(16) => Width::Width16,
                            Some(8)  => Width::Width8,
                            _ => panic!(),
                        };

                        Some((groups.at(0).unwrap_or("").len(), (ctor(w),)))
                    },
                    _ => None
                }
            },
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
            Token::IdentBangTok => |&:s| {
                let matcher = matcher!(r"([a-zA-Z_]\w*)!");
                match matcher.captures(s) {
                   Some(groups) => {
                        let t = groups.at(0);
                        Some((t.unwrap_or("").len(), (String::from_str(groups.at(1).unwrap_or("")),)))
                   },
                    _ => None
                }
            },
            Token::NumberTok    => |&:s| {
                use std::num::from_str_radix;

                let matcher = matcher!(r"((?:0[xX]([:xdigit:]+))|(?:\d+))(?:([uUiI])(32|16|8)?)?");
                match matcher.captures(s) {
                    Some(groups) => {
                        let (num_str, radix) = match groups.at(2).unwrap_or("") {
                            ""  => (groups.at(1).unwrap_or(""), 10),
                            hex => (hex, 16),
                        };

                        let s = groups.at(3).unwrap_or("");
                        let kind = if s.len() > 0 {
                            let ctor: fn(Width) -> IntKind = match s.char_at(0) {
                                'u' | 'U' => IntKind::UnsignedInt,
                                'i' | 'I' => IntKind::SignedInt,
                                _ => panic!(),
                            };

                            let w = match from_str_radix::<u8>(groups.at(4).unwrap_or(""), 10) {
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

                        Some((groups.at(0).unwrap_or("").len(), (from_str_radix(num_str, radix).unwrap(), kind)))
                    },
                    _ => None
                }
            },
            Token::NumberTok    => |&:s| {
                let matcher = matcher!(r#"'((?:\\["nrt'\\]|\\[xX][0-9a-fA-F]+|[^']))'"#);
                match matcher.captures(s) {
                   Some(groups) => {
                       use rust_syntax::parse::char_lit;
                       let t = groups.at(0).unwrap_or("");
                       let (c, _) = char_lit(groups.at(1).unwrap_or(""));
                       Some((t.len(), (c as u64, IntKind::UnsignedInt(Width::Width8))))
                   },
                    _ => None
                }
            },
            Token::StringTok    => |&:s| {
                let matcher = matcher!(r#""((?:\\"|[^"])*)""#);
                match matcher.captures(s) {
                   Some(groups) => {
                        let t = groups.at(0).unwrap_or("");
                        Some((t.len(), (String::from_str(groups.at(1).unwrap_or("")),)))
                   },
                    _ => None
                }
            }
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
