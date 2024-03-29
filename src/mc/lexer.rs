use util::{IntKind, Width};
use std::io;

pub use util::lexer::{Language, Lexer, LexerRule, LexerRuleT};
pub use util::lexer::{RuleMatcher, SourceToken, TokenMaker};
pub use util::lexer::BufReader;

use util::Escape;

use std::fmt;

use regex::Regex;

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
    Asm,

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
            Token::Asm                    => "asm!".to_string(),

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
pub fn lexer_from_str(s: &str) -> Lexer<io::BufReader<&[u8]>, Token> {
    let bytes = s.as_bytes();
    let buffer = io::BufReader::new(bytes);
    new_mb_lexer("test", buffer)
}

// XXX: This is a workaround I don't totally understand and might not work.
fn mk_rule<A: 'static, T: RuleMatcher<A>+'static, U: TokenMaker<A, Token>+'static>(
    matcher: T, maker: U)
    -> Box<dyn LexerRuleT<Token>> {
    Box::new(LexerRule::<A, _, _>::new(matcher, maker)) as Box<dyn LexerRuleT<Token>>
}
macro_rules! matcher { ( $e:expr ) => ( Regex::new(concat!("^(?:", $e, ")")).unwrap()) }
macro_rules! lexer_rules {
    ( $( $c:expr => $m:expr ),*) => (
        vec! ( $( mk_rule($m, $c) ),* )
    )
}

// Rule to match U32, U16, U8, I32, I16, I8
struct IntTypeRule {
    re: Regex,
}

impl IntTypeRule {
    pub fn new() -> IntTypeRule {
        IntTypeRule { re: matcher!(r"[uUiI](32|16|8)") }
    }
}

impl RuleMatcher<IntKind> for IntTypeRule {
    fn find(&self, s: &str) -> Option<(usize, IntKind)> {
        match self.re.captures(s) {
            Some(groups) => {
                let ctor: fn(Width) -> IntKind = match s.chars().nth(0).unwrap() {
                    'u' | 'U' => IntKind::UnsignedInt,
                    'i' | 'I' => IntKind::SignedInt,
                    _ => panic!(),
                };
                let w = match u8::from_str_radix(&groups[1], 10) {
                    Ok(32) => Width::Width32,
                    Ok(16) => Width::Width16,
                    Ok(8)  => Width::Width8,
                    _ => panic!(),
                };

                Some((groups[0].len(), ctor(w)))
            },
            _ => None
        }
    }
}

// Rule to match a numeric literal and parse it into a number
struct NumberRule {
    re: Regex,
}

impl NumberRule {
    pub fn new() -> NumberRule {
        NumberRule { re: matcher!(r"((?:0[xX]([[:xdigit:]]+))|(?:\d+))(?:([uUiI])(32|16|8)?)?") }
    }
}

impl RuleMatcher<(u64, IntKind)> for NumberRule {
    fn find(&self, s: &str) -> Option<(usize, (u64, IntKind))> {
        match self.re.captures(s) {
            Some(groups) => {
                let (num_str, radix) = match groups.get(2) {
                    None => (&groups[1], 10),
                    Some(hex) => (hex.as_str(), 16),
                };

                let s_opt = groups.get(3).map(|s| s.as_str());
                let kind = match s_opt {
                    Some(s) => {
                        let ctor: fn(Width) -> IntKind = match s.chars().nth(0).unwrap() {
                            'u' | 'U' => IntKind::UnsignedInt,
                            'i' | 'I' => IntKind::SignedInt,
                            _ => panic!(),
                        };
                        let w = match groups.get(4).map(|g| u8::from_str_radix(&g.as_str(), 10)) {
                            Some(Err(_)) | None => Width::AnyWidth,
                            Some(Ok(32)) => Width::Width32,
                            Some(Ok(16)) => Width::Width16,
                            Some(Ok(8)) => Width::Width8,
                            _ => panic!(),
                        };

                        ctor(w)
                    },
                    None => IntKind::GenericInt
                };

                Some((groups[0].len(),
                      (u64::from_str_radix(num_str, radix).unwrap(), kind)))
            },
            _ => None
        }
    }
}

// Rule to match a name followed by a Bang and strip off the trailing Bang
struct IdentBangRule {
    re: Regex,
}

impl IdentBangRule {
    pub fn new() -> IdentBangRule {
        IdentBangRule { re: matcher!(r"([a-zA-Z_]\w*)!") }
    }
}

impl RuleMatcher<String> for IdentBangRule {
    fn find(&self, s: &str) -> Option<(usize, String)> {
        match self.re.captures(s) {
           Some(groups) => {
                let t = &groups[0];
                Some((t.len(), groups[1].to_string()))
           },
            _ => None
        }
    }
}

// Rule to match a string literal and strip off the surrounding quotes
struct StringRule {
    re: Regex,
}

fn escaped_char(c: char) -> char {
    match c {
        't' => '\t',
        'n' => '\n',
        'r' => '\r',
        '\\' => '\\',
        x => x,
    }
}

impl StringRule {
    pub fn new() -> StringRule {
        StringRule { re: matcher!(r#""((?:\\"|[^"])*)""#) }
    }
}

impl RuleMatcher<String> for StringRule {
    fn find(&self, s: &str) -> Option<(usize, String)> {
        match self.re.captures(s) {
           Some(groups) => {
               fn str_lit(s: &str) -> String {
                   let mut quoted = false;
                   let mut res = String::from("");
                   for c in s.chars() {
                       if quoted {
                           quoted = false;
                           res.push(escaped_char(c));
                       } else if c == '\\' {
                           quoted = true;
                       } else {
                           res.push(c);
                       }
                   }

                   res
               }
               let t = &groups[0];
               Some((t.len(), str_lit(&groups[1])))
           },
            _ => None
        }
    }
}

struct CharRule {
    re: Regex,
}

impl CharRule {
    pub fn new() -> CharRule {
        CharRule { re: matcher!(r#"'((?:\\["nrt'\\]|\\[xX][0-9a-fA-F]+|[^']))'"#) }
    }
}

impl RuleMatcher<(u64, IntKind)> for CharRule {
    fn find(&self, s: &str) -> Option<(usize, (u64, IntKind))> {
        match self.re.captures(s) {
           Some(groups) => {
               fn char_lit(s: &str) -> char {
                   let first = s.chars().nth(0).unwrap();
                   if first == '\\' {
                       let second = s.chars().nth(1).unwrap();
                       if second == 'x' || second == 'X' {
                           let n = u8::from_str_radix(&s[2..], 16).unwrap();
                           n as char
                       } else {
                           escaped_char(second)
                       }
                   } else {
                       first
                   }
               }
               let t = &groups[0];
               let c = char_lit(&groups[1]);
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
            Token::Asm          => "asm!",

            // Basic types; TODO: add more.
            Token::IntTypeTok   => IntTypeRule::new(),
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
            Token::IdentBangTok => IdentBangRule::new(),
            |(n, ik)| Token::NumberTok(n, ik)    => NumberRule::new(),
            |(n, ik)| Token::NumberTok(n, ik)    => CharRule::new(),
            Token::StringTok    => StringRule::new()
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
    use util::IntKind;

    use super::SourceToken as ST;
    

    use std::vec::Vec;

    use super::*;

    type SourceToken = ST<Token>;

    fn compare(actual: &[SourceToken], expected: &[Token]) {
        for (actual_st, expected_tok)
            in actual.iter().zip(expected.iter()) {
            assert!(actual_st.tok == *expected_tok,
                    "Failure:\n  found {:?}, expected {:?}\n",
                            actual_st.tok, *expected_tok);
        }
    }

    #[test]
    fn test() {
        let lexer1 = lexer_from_str(r#"f(x - /* I am a comment */ 0x3f5B)+1 "Hello\" World")"#);
        let tokens1: Vec<SourceToken> = lexer1.collect();

        compare(&tokens1[..],
                &[
                    Token::IdentTok("f".to_string()),
                    Token::LParen,
                    Token::IdentTok("x".to_string()),
                    Token::Dash,
                    Token::NumberTok(0x3f5B, IntKind::GenericInt),
                    Token::RParen,
                    Token::Plus,
                    Token::NumberTok(1, IntKind::GenericInt),
                    Token::StringTok(r#"Hello" World"#.to_string()),
                ]);

        let lexer2 = lexer_from_str("let x: int = 5;");
        let tokens2: Vec<SourceToken> = lexer2.collect();
        compare(&tokens2[..],
                &[
                    Token::Let,
                    Token::IdentTok("x".to_string()),
                    Token::Colon,
                    Token::IdentTok("int".to_string()),
                    Token::Eq,
                    Token::NumberTok(5, IntKind::GenericInt),
                ]);
    }
}
