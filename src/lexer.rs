use regexp::Regexp;
use std::vec::Vec;
use std::slice::CloneableVector;

/// A single regexp for a token.
#[deriving(Clone)]
struct LexerRule {
    matcher: Regexp,
    token: Token,
}

#[deriving(Eq, Clone)]
pub enum Token {
    // Whitespace
    WS,

    // Reserved words
    Let,

    // Symbols
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Less,
    Greater,
    And,
    Or,
    Xor,
    AndAnd,
    OrOr,
    Add,
    Sub,
    Mul,
    Div,
    Lsh,
    Rsh,
    Colon,
    Semi,
    Eq,
    Bang,

    // Literals
    Ident,
    Number,
    HexNumber,
    String,
}

pub struct Lexer {
    rules: ~[LexerRule],
}

impl Lexer {
    pub fn new() -> Lexer {
        macro_rules! lexer {
            ( $( $t:expr => $r:expr ),*) => (
                Lexer { rules: ~[ $( LexerRule { matcher: regexp!(concat!("^(?:", $r, ")")), token: $t } ),* ] }
            )
        }

        // Note: rules are in decreasing order of priority if there's a
        // conflict. In particular, reserved words must go before Ident.
        lexer! {
            // Whitespace
            WS         => r"\s|//.*|(?s)/\*.*\*/",

            // Reserved words
            Let        => r"let",

            // Symbols
            LParen     => r"\(",
            RParen     => r"\)",
            LBrace     => r"\{",
            RBrace     => r"\}",
            LBracket   => r"\[",
            RBracket   => r"\]",
            Less       => r"<",
            Greater    => r">",
            And        => r"&",
            Or         => r"\|",
            Xor        => r"\^",
            AndAnd     => r"&&",
            OrOr       => r"\|\|",
            Add        => r"\+",
            Sub        => r"-",
            Mul        => r"\*",
            Div        => r"/",
            Lsh        => r"<<",
            Rsh        => r">>",
            Colon      => r":",
            Semi       => r";",
            Eq         => r"=",
            Bang       => r"!",

            // Literals
            Ident      => r"[a-zA-Z_]\w*",
            Number     => r"\d",
            HexNumber  => r"0[xX][:xdigit:]+",
            String     => r#""(?:\\"|[^"])*""#
        }
    }

    pub fn tokenize(&self, s: &str) -> (~[(Token, ~str)]) {
        let mut pos = 0u;
        let mut result = vec!();
        while pos < s.len() {
            let mut longest = 0u;
            let mut best = None;
            for rule in self.rules.iter() {
                let m = rule.matcher.find(s.slice_from(pos));
                match m {
                    Some((begin, end)) if begin == 0 => {
                        let s = s.slice(pos, pos + end);
                        if s.len() > longest {
                            best = Some((rule.token, s));
                            longest = s.len();
                        }
                    },
                    _ => {},
                }
            }
            pos += longest;
            match best.unwrap() {
                (WS, _) => {},
                (t, s) => result.push((t, s.to_owned()))
            }
        }

        result.as_slice().to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let l = Lexer::new();

        compare(l.tokenize(
            r#"f(x - /* I am a comment */ 0x3f5B)+1 "Hello\" World")"#),
                [(Ident, ~"f"),
                  (LParen, ~"("),
                  (Ident, ~"x"),
                  (Sub, ~"-"),
                  (HexNumber, ~"0x3f5B"),
                  (RParen, ~")"),
                  (Add, ~"+"),
                  (Number, ~"1"),
                  (String, ~r#""Hello\" World""#),
                ]);
        compare(l.tokenize("let x: int = 5;"),
                [(Let, ~"let"),
                 (Ident, ~"x"),
                 (Colon, ~":"),
                 (Ident, ~"int"),
                 (Eq, ~"="),
                 (Number, ~"5"),
                 ]);
    }
}
