use lexer::*;
use std::iter::Peekable;
mod lexer;

#[deriving(Eq, Show)]
pub enum AST {
    Num(int),
    Sum(~AST, ~AST),
    Difference(~AST, ~AST),
    Product(~AST, ~AST),
    Quotient(~AST, ~AST),
}

pub struct Parser<A, T> {
    tokens: ~Peekable<A, T>,
}

impl<T: Iterator<(Token, ~str)>> Parser<(Token, ~str), T> {
    pub fn new(tokens: T) -> Parser<(Token, ~str), T> {
        Parser::<(Token, ~str), T> {
            tokens: ~tokens.peekable(),
        }
    }

    /// "Peek" at the next token, returning the token, without consuming
    /// it from the stream.
    fn peek(&mut self) -> Token {
        match self.tokens.peek() {
            Some(&(token, _)) => token.clone(),
            None => Eof,
        }
    }

    /// Consume one token from the stream, erroring if it's not
    /// the `expected_token`. Returns the string corresponding to that
    /// token.
    fn expect(&mut self, expected_token: Token) -> ~str {
        let (stream_token, s) = self.tokens.next().unwrap();
        assert_eq!(expected_token, stream_token);
        s
    }

    fn parse_factor(&mut self) -> AST {
        /*
        Parse a factor.

        FACTOR ::= '(' EXPR ')'
                 | Number
        */
        match self.peek() {
            // '(' expr ')'
            LParen => {
                self.expect(LParen);
                let result = self.parse_expr();
                self.expect(RParen);
                return result;
            },
            Number => {
                let num = self.expect(Number);
                return Num(from_str(num).unwrap());
            },
            _ => { fail!("Parse error."); }
        }

    }

    fn parse_term(&mut self) -> AST {
        /*
        Parse a term.

        TERM ::= FACTOR [ '*' TERM ]
               | FACTOR [ '/' TERM ]
        */
        let matched_factor = self.parse_factor();
        match self.peek() {
            Star => {
                self.expect(Star);
                let next_term = self.parse_term();
                return Product(~matched_factor, ~next_term);
            },
            ForwardSlash => {
                self.expect(ForwardSlash);
                let next_term = self.parse_term();
                return Quotient(~matched_factor, ~next_term);
            }
            _ => {
                return matched_factor;
            }
        }
    }

    pub fn parse_expr(&mut self) -> AST {
        /*
        Parse an expression.

        EXPR ::= TERM [ '+' EXPR ]
               | TERM [ '-' EXPR ]
         */
        
        match self.peek() {
            Number => { let parsed_term = self.parse_term();
                        match self.peek() {
                            Plus => { self.expect(Plus);
                                      let next_term = self.parse_expr();
                                      return Sum(~parsed_term,
                                                 ~next_term);
                            },
                            Dash => { self.expect(Dash);
                                      let next_term = self.parse_expr();
                                      return Difference(~parsed_term,
                                                        ~next_term);
                            }
                            _ => { return parsed_term;
                            }
                        }
            }
            _ => { fail!(); }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Parser;
    use super::AST;
    use lexer::Lexer;

    #[test]
    fn test() {
        let lexer = Lexer::new(~vec!(~r#"1+3*5/2-2*3*(5+6)"#).move_iter());
        let mut parser = Parser::new(lexer);
        assert_eq!(parser.parse_expr(),
                   Sum(
                       ~Num(1),
                       ~Difference(
                           ~Product(
                               ~Num(3),
                               ~Quotient(
                                   ~Num(5),
                                   ~Num(2)
                                       )
                                   ),
                           ~Product(
                               ~Num(2),
                               ~Product(
                                   ~Num(3),
                                   ~Sum(
                                       ~Num(5),
                                       ~Num(6)
                                           )
                                       )
                                   )
                               )
                           )
                   );
    }
}
