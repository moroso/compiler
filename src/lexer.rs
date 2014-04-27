use regex::Regex;
use std::iter;
use std::slice::CloneableVector;

/// A single regex for a token.
#[deriving(Clone)]
struct LexerRule {
    matcher: Regex,
    token: Token,
}

#[deriving(Eq, Clone, Show)]
pub enum Token {
    // Whitespace
    WS,

    // Reserved words
    Let,
    As,
    If,
    Else,

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
    Ident,
    Number,
    HexNumber,
    String,
    True,
    False,

    // Basic types
    U32,
    I32,

    // Special
    Eof,
}

#[deriving(Show, Eq)]
pub struct SourceToken {
    pub tok: Token,
    pub txt: ~str
}

struct LineContext {
    pos: uint,
    line: ~str,
}

pub struct Lexer<T> {
    iter: T,
    linectx: Option<LineContext>,
    rules: Vec<LexerRule>,
}

impl<T: Iterator<~str>> Lexer<T> {
    pub fn new(line_iter: T) -> Lexer<T> {
        macro_rules! lexer_rules {
            ( $( $t:expr => $r:expr ),*) => (
                    vec!( $( LexerRule { matcher: regex!(concat!("^(?:", $r, ")")),
                                         token: $t } ),* )
            )
        }

        // Note: rules are in decreasing order of priority if there's a
        // conflict. In particular, reserved words must go before Ident.
        let rules = lexer_rules! {
            // Whitespace
            WS         => r"\s|//.*|(?s)/\*.*\*/",

            // Reserved words
            Let        => r"let",
            True       => r"true",
            False      => r"false",
            As         => r"as",
            If         => r"if",
            Else       => r"else",

            // Basic types; TODO: add more.
            I32        => r"[iI]32",
            U32        => r"[uU]32",

            // Symbols
            LParen       => r"\(",
            RParen       => r"\)",
            LBrace       => r"\{",
            RBrace       => r"\}",
            LBracket     => r"\[",
            RBracket     => r"\]",
            Less         => r"<",
            Greater      => r">",
            LessEq       => r"<=",
            GreaterEq    => r">=",
            Ampersand    => r"&",
            Pipe         => r"\|",
            Xor          => r"\^",
            AmpAmp       => r"&&",
            PipePipe     => r"\|\|",
            Plus         => r"\+",
            Dash         => r"-",
            Star         => r"\*",
            ForwardSlash => r"/",
            Lsh          => r"<<",
            Rsh          => r">>",
            Colon        => r":",
            ColonColon   => r"::",
            Semicolon    => r";",
            Eq           => r"=",
            EqEq         => r"==",
            Bang         => r"!",
            Arrow        => r"->",
            Comma        => r",",
            QuestionMark => r"\?",
            Period       => r"\.",

            // Literals
            Ident      => r"[a-zA-Z_]\w*",
            Number     => r"\d+",
            HexNumber  => r"0[xX][:xdigit:]+",
            String     => r#""(?:\\"|[^"])*""#
        };

        Lexer {
            iter: line_iter,
            linectx: None,
            rules: rules
        }
    }
}

impl<T: Iterator<~str>> Iterator<SourceToken> for Lexer<T> {
    fn next(&mut self) -> Option<SourceToken> {
        loop {
            for lc in self.linectx.mut_iter() {
                while lc.pos < lc.line.len() {
                    let mut longest = 0u;
                    let mut best = None;
                    for rule in self.rules.iter() {
                        let m = rule.matcher.find(lc.line.slice_from(lc.pos));
                        match m {
                            Some((begin, end)) if begin == 0 => {
                                let s = lc.line.slice(lc.pos, lc.pos + end);
                                if s.len() > longest {
                                    best = Some((rule.token, s));
                                    longest = s.len();
                                }
                            },
                            _ => {},
                        }
                    }

                    lc.pos += longest;

                    match best {
                        None => fail!("Unexpected input"),
                        Some((t, s)) if t != WS => {
                            return Some(SourceToken {
                                tok: t,
                                txt: s.to_owned()
                            })
                        }
                        _ => {}
                    }
                }
            }

            match self.iter.next() {
                None => return None,
                Some(line) => {
                    self.linectx = Some(LineContext {
                        pos: 0,
                        line: line,
                    });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::Repeat;
    use std::vec::Vec;

    fn compare(actual: &[SourceToken], expected: &[(Token, ~str)]) {
        for (ref actual_st, ref expected_tup)
            in actual.iter().zip(expected.iter()) {
            assert!(actual_st.tok == *expected_tup.ref0() &&
                    actual_st.txt == *expected_tup.ref1(),
                    format!("Failure:\n  Tokens:{:?}, {:?}\n  Strings:{:s}, {:s}\n",
                            actual_st.tok, *expected_tup.ref0(),
                            actual_st.txt, *expected_tup.ref1()));
        }
    }

    #[test]
    fn test() {
        let lexer1 = Lexer::new(vec!(~r#"f(x - /* I am a comment */ 0x3f5B)+1 "Hello\" World")"#).move_iter());
        let tokens1: ~[SourceToken] = FromIterator::from_iter(lexer1);

        compare(tokens1,
                [(Ident, ~"f"),
                  (LParen, ~"("),
                  (Ident, ~"x"),
                  (Dash, ~"-"),
                  (HexNumber, ~"0x3f5B"),
                  (RParen, ~")"),
                  (Plus, ~"+"),
                  (Number, ~"1"),
                  (String, ~r#""Hello\" World""#),
                ]);

        let lexer2 = Lexer::new(vec!(~"let x: int = 5;").move_iter());
        let tokens2: ~[SourceToken] = FromIterator::from_iter(lexer2);
        compare(tokens2,
                [(Let, ~"let"),
                 (Ident, ~"x"),
                 (Colon, ~":"),
                 (Ident, ~"int"),
                 (Eq, ~"="),
                 (Number, ~"5"),
                 ]);
    }
}
