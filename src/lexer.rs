use regex::Regex;
use std::vec::Vec;
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
    Semicolon,
    Eq,
    EqEq,
    Bang,
    Arrow,
    Comma,
    QuestionMark,

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

pub struct Lexer<T> {
    rules: ~[LexerRule],
    current_line: Option<~str>,
    pos: Option<uint>,
    line_iter: ~T,
}



impl<T: Iterator<~str>> Lexer<T> {
    pub fn new(line_iter: ~T) -> Lexer<T> {
        macro_rules! lexer_rules {
            ( $( $t:expr => $r:expr ),*) => (
                ~[ $( LexerRule { matcher: regex!(concat!("^(?:", $r, ")")),
                                  token: $t } ),* ]
            )
        }

        // Note: rules are in decreasing order of priority if there's a
        // conflict. In particular, reserved words must go before Ident.
        Lexer {
            current_line: None,
            pos: None,
            line_iter: line_iter,
            rules:
            lexer_rules! {
                // Whitespace
                WS         => r"\s|//.*|(?s)/\*.*\*/",

                // Reserved words
                Let        => r"let",
                True       => r"true",
                False      => r"false",
                As         => r"as",

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
                Semicolon    => r";",
                Eq           => r"=",
                EqEq         => r"==",
                Bang         => r"!",
                Arrow        => r"->",
                Comma        => r",",
                QuestionMark => r"\?",

                // Literals
                Ident      => r"[a-zA-Z_]\w*",
                Number     => r"\d+",
                HexNumber  => r"0[xX][:xdigit:]+",
                String     => r#""(?:\\"|[^"])*""#
            }
        }
    }
}

impl<T: Iterator<~str>> Iterator<(Token, ~str)> for Lexer<T> {
    fn next(&mut self) -> Option<(Token, ~str)> {
        let mut tok = None;

        while tok.is_none() {
            // Grab a new line if we need to
            let pos = match self.pos {
                Some(p) => p,
                None => {
                    self.current_line = match self.line_iter.next() {
                        None => return None,
                        line => line
                    };
                    0
                }
            };

            // Move the line out of self to avoid a copy on every iteration
            let line = self.current_line.take_unwrap();

            // Check for a match
            let mut longest = 0u;

            // Appease the borrow checker by killing 'best' early
            {
                let mut best = None;
                for rule in self.rules.iter() {
                    let m = rule.matcher.find(line.slice_from(pos));
                    match m {
                        Some((begin, end)) if begin == 0 => {
                            let s = line.slice(pos, pos + end);
                            if s.len() > longest {
                                best = Some((rule.token, s));
                                longest = s.len();
                            }
                        },
                        _ => {},
                    }
                }

                // Save the matched token, skipping whitespace
                tok = match best {
                    Some((WS, _)) => None,
                    Some((t, s))  => Some((t, s.to_owned())),
                    None          => fail!("Unexpected input")
                };
            }

            // Advance past the token
            let pos = pos + longest;
            if pos == line.len() {
                self.pos = None;
            } else {
                self.pos = Some(pos);
                self.current_line = Some(line);
            }
        }

        tok
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::Repeat;
    use std::vec::Vec;

    fn compare(actual: &[(Token, ~str)], expected: &[(Token, ~str)]) {
        for (&(actual_token, ref actual_str),
             &(expected_token, ref expected_str))
            in actual.iter().zip(expected.iter()) {
            assert!(actual_token == expected_token &&
                    actual_str == expected_str,
                    format!("Failure:\n  Tokens:{:?}, {:?}\n  Strings:{:s}, {:s}\n",
                            actual_token, expected_token,
                            *actual_str, *expected_str));
        }
    }

    #[test]
    fn test() {
        let lexer1 = Lexer::new(~vec!(~r#"f(x - /* I am a comment */ 0x3f5B)+1 "Hello\" World")"#).move_iter());

        let tokens1: ~[(Token, ~str)] = FromIterator::from_iter(lexer1);

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

        let lexer2 = Lexer::new(~vec!(~"let x: int = 5;").move_iter());
        let tokens2: ~[(Token, ~str)] = FromIterator::from_iter(lexer2);
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
