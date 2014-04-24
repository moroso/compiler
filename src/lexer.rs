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

pub struct Lexer<T> {
    rules: ~[LexerRule],
    current_line: Option<~str>,
    pos: Option<uint>,
    line_iter: ~T,
}



impl<T: Iterator<~str>> Lexer<T> {
    pub fn new<T>(line_iter: ~T) -> Lexer<T> {
        macro_rules! lexer_rules {
            ( $( $t:expr => $r:expr ),*) => (
                ~[ $( LexerRule { matcher: regexp!(concat!("^(?:", $r, ")")),
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
    }
}

impl<T: Iterator<~str>> Iterator<(Token, ~str)> for Lexer<T> {
    fn next(&mut self) -> Option<(Token, ~str)> {
        loop {
            if self.pos.is_none() {
                self.current_line = self.line_iter.next();
                if self.current_line.is_none() { return None; }
                self.pos = Some(0);
            }

            let mut pos = self.pos.unwrap();
            let s = self.current_line.clone().unwrap();

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
            if pos == s.len() {
                self.pos = None;
                self.current_line = None;
            } else {
                self.pos = Some(pos);
            }

            match best.unwrap() {
                (WS, _) => continue,
                (t, s) => return Some((t, s.to_owned()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::Repeat;

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
        let test_iter = Repeat::new(~"f(x)+1");
        let mut l = Lexer::<Repeat<~str>>::new::<Repeat<~str>>(~test_iter);

        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());
        print!("{:?}\n", l.next());

        fail!();
        /*
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
                 ]);*/
    
    }
}
