use lexer::*;
use std::fmt::{Formatter, Result, Show};
use std::iter::Peekable;
use std::num;

#[deriving(Eq)]
pub struct NumberType {
    signedness: Signedness,
    width: u8,
}

#[deriving(Eq, Show)]
pub enum Signedness {
    Signed,
    Unsigned,
}

impl Show for NumberType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f.buf, "{}{}",
               match self.signedness {
                   Signed => 'i',
                   Unsigned => 'u',
               },
               self.width)
    }
}

// Numbers by default are 32-bit, signed.
mod defaults {
    use super::{NumberType, Signed, Unsigned};
    pub static DEFAULT_NUM_TYPE: NumberType = NumberType { signedness: Signed, width: 32 };
}

#[deriving(Eq)]
pub enum ExpressionComponent {
    Num(i64, NumberType),
    StringConstant(~str),
    TrueConstant,
    FalseConstant,
    Sum(~ExpressionComponent, ~ExpressionComponent),
    Difference(~ExpressionComponent, ~ExpressionComponent),
    Product(~ExpressionComponent, ~ExpressionComponent),
    Quotient(~ExpressionComponent, ~ExpressionComponent),
    Dereference(~ExpressionComponent),
    Reference(~ExpressionComponent),
    Identifier(~str),
    EqualsExpr(~ExpressionComponent, ~ExpressionComponent),
    LessExpr(~ExpressionComponent, ~ExpressionComponent),
    LessEquals(~ExpressionComponent, ~ExpressionComponent),
    GreaterExpr(~ExpressionComponent, ~ExpressionComponent),
    GreaterEquals(~ExpressionComponent, ~ExpressionComponent),
    LogicalAnd(~ExpressionComponent, ~ExpressionComponent),
    BitwiseAnd(~ExpressionComponent, ~ExpressionComponent),
    LogicalOr(~ExpressionComponent, ~ExpressionComponent),
    BitwiseOr(~ExpressionComponent, ~ExpressionComponent),
    Index(~ExpressionComponent, ~ExpressionComponent),
    Assignment(~ExpressionComponent, ~ExpressionComponent),
    FunctionApplication(~ExpressionComponent, Vec<~ExpressionComponent>),
    Cast(~ExpressionComponent, ~Type),
    TernaryConditional(~ExpressionComponent,
                       ~ExpressionComponent,
                       ~ExpressionComponent),
}

impl Show for ExpressionComponent {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Num(n, t) => write!(f.buf, "{}{}", n, t),
            StringConstant(ref s) => write!(f.buf, "{}", s),
            TrueConstant => write!(f.buf, "true"),
            FalseConstant => write!(f.buf, "false"),
            Sum(ref e1, ref e2) => write!(f.buf, "({}+{})", e1, e2),
            Difference(ref e1, ref e2) => write!(f.buf, "({}-{})", e1, e2),
            Product(ref e1, ref e2) => write!(f.buf, "({}*{})", e1, e2),
            Quotient(ref e1, ref e2) => write!(f.buf, "({}/{})", e1, e2),
            EqualsExpr(ref e1, ref e2) => write!(f.buf, "({}=={})", e1, e2),
            LessExpr(ref e1, ref e2) => write!(f.buf, "({}<{})", e1, e2),
            LessEquals(ref e1, ref e2) => write!(f.buf, "({}<={})", e1, e2),
            GreaterExpr(ref e1, ref e2) => write!(f.buf, "({}>{})", e1, e2),
            GreaterEquals(ref e1, ref e2) => write!(f.buf, "({}>={})", e1, e2),
            LogicalAnd(ref e1, ref e2) => write!(f.buf, "({}&&{})", e1, e2),
            LogicalOr(ref e1, ref e2) => write!(f.buf, "({}||{})", e1, e2),
            BitwiseAnd(ref e1, ref e2) => write!(f.buf, "({}&{})", e1, e2),
            BitwiseOr(ref e1, ref e2) => write!(f.buf, "({}|{})", e1, e2),
            Dereference(ref e) => write!(f.buf, "(*{})", e),
            Reference(ref e) => write!(f.buf, "(&{})", e),
            Identifier(ref s) => write!(f.buf, "{}", s),
            Index(ref e1, ref e2) => write!(f.buf, "{}[{}]", e1, e2),
            Assignment(ref e1, ref e2) => write!(f.buf, "({}={})", e1, e2),
            FunctionApplication(ref e1, ref args) =>
                write!(f.buf, "{}({})", e1, args),
            Cast(ref e, ref t) => write!(f.buf, "({} as {})", e, t),
            TernaryConditional(ref e1, ref e2, ref e3) =>
                write!(f.buf, "({} ? {} : {})", e1, e2, e3),
        }
    }
}

#[deriving(Eq)]
pub enum Type {
    // Types
    PointerTo(~Type),
    NamedType(~str),
    FunctionType(~Type, ~Type),
    ArrayType(~Type, i64),
}

impl Show for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            PointerTo(ref t) => write!(f.buf, "(*{})", t),
            NamedType(ref s) => write!(f.buf, "{}", s),
            FunctionType(ref t1, ref t2) => write!(f.buf, "({} -> {})", t1, t2),
            ArrayType(ref t1, n) => write!(f.buf, "({}[{}])", t1, n),
        }
    }
}

#[deriving(Eq, Show)]
pub enum VariableDeclarationEnum {
    // TODO: qualifiers.
    VariableDeclaration(~str, ~Type),
    TypedVariableDeclarationInit(~str, ~Type, ~ExpressionComponent),
    VariableDeclarationInit(~str, ~ExpressionComponent),
}

pub struct Parser<A, T> {
    tokens: ~Peekable<A, T>,
}

impl<T: Iterator<SourceToken>> Parser<SourceToken, T> {
    pub fn new(tokens: T) -> Parser<SourceToken, T> {
        Parser::<SourceToken, T> {
            tokens: ~tokens.peekable(),
        }
    }

    /// "Peek" at the next token, returning the token, without consuming
    /// it from the stream.
    fn peek(&mut self) -> Token {
        match self.tokens.peek() {
            Some(st) => st.tok,
            None => Eof,
        }
    }

    /// Consume one token from the stream, erroring if it's not
    /// the `expected_token`. Returns the string corresponding to that
    /// token.
    fn expect(&mut self, expected_token: Token) -> ~str {
        match self.tokens.next() {
            ref mut o@Some(ref st) if st.tok == expected_token => o.take_unwrap().txt,
            st => fail!("Expected {:?}, found {:?}", expected_token, st.map_or(Eof, |st| st.tok))
        }
    }

    fn parse_number(&mut self) -> i64 {
        match self.peek() {
            Number => {
                let num = self.expect(Number);
                return from_str(num).unwrap();
            },
            HexNumber => {
                let num = self.expect(HexNumber);
                assert_eq!(num[0], '0' as u8);
                assert!(num[1] == ('x' as u8) || num[1] == ('X' as u8));
                let x: Option<i64> = num::from_str_radix(num.slice_from(2), 16);
                let y: i64 = x.unwrap();
                return y;
            },
            _ => { fail!("Parse error."); }
        }        
    }

    fn parse_typed_literal(&mut self) -> ExpressionComponent {
        match self.peek() {
            True => { self.expect(True); TrueConstant },
            False => { self.expect(False); FalseConstant },
            String => { StringConstant(self.expect(String)) },
            Number => {
                let num = self.parse_number();
                let number_type_str = match self.peek() {
                    x if x == U32 || x == I32 => {
                        self.expect(x)
                    },
                    _ => return Num(num, defaults::DEFAULT_NUM_TYPE)
                };
                let signedness = match number_type_str[0] as char {
                    'i' | 'I' => Signed,
                    'u' | 'U' => Unsigned,
                    _ => fail!()
                };
                let width = from_str(number_type_str.slice_from(1)).unwrap();
                Num(num, NumberType { signedness: signedness,
                                      width: width })
            },
            _ => fail!()
        }
    }

    fn parse_expr_list(&mut self) -> Vec<~ExpressionComponent> {
        /*
        Parse a list of expressions, as in a function call.

        EXPR_LIST ::= [ EXPR [ ',' EXPR ...] [ ',' ] ]
        */
        let mut res = vec!();
        loop {
            match self.peek() {
                RParen => return res,
                _ => { res.push(~self.parse_expr());
                       match self.peek() {
                           RParen => return res,
                           Comma => { self.expect(Comma); },
                           _ => fail!()
                       }
                }
            }
        }
    }

    fn parse_index(&mut self) -> ExpressionComponent {
        /*
        Parse a possibly-array-indexed expression.

        INDEX ::= '(' EXPR ')'
                | Number | String | True | False
                | INDEX '[' EXPR ']'
                | INDEX '(' ARGLIST ')'
                | INDEX 'as' TYPE
                | IDENT
        */
        let first_index =
            match self.peek() {
                // '(' expr ')'
                LParen => {
                    self.expect(LParen);
                    let result = self.parse_expr();
                    self.expect(RParen);
                    result
                },
                Number | HexNumber | True | False | String => {
                    self.parse_typed_literal()
                },
                Ident => {
                    let ident_name = self.expect(Ident);
                    Identifier(ident_name)
                }
                Star => {
                    self.expect(Star);
                    Dereference(~self.parse_factor())
                },
                Ampersand => {
                    self.expect(Ampersand);
                    Reference(~self.parse_factor())
                }
                _ => { fail!("Parse error."); }
            };
        match self.peek() {
            LBracket => {
                self.expect(LBracket);
                let indexing_expr = self.parse_expr();
                self.expect(RBracket);
                Index(~first_index, ~indexing_expr)
            },
            LParen => {
                self.expect(LParen);
                let args = self.parse_expr_list();
                self.expect(RParen);
                FunctionApplication(~first_index, args)
            },
            As => {
                self.expect(As);
                let type_specifier = self.parse_type();
                Cast(~first_index, ~type_specifier)
            },
            _ => first_index
        }

    }

    fn parse_factor(&mut self) -> ExpressionComponent {
        /*
        Parse a factor.

        FACTOR ::= INDEX
                 | '*' INDEX
                 | '&' INDEX
        */
        match self.peek() {
            Star => {
                self.expect(Star);
                Dereference(~self.parse_index())
            },
            Ampersand => {
                self.expect(Ampersand);
                Reference(~self.parse_index())
            }
            _ => self.parse_index()
        }
    }

    fn parse_term(&mut self) -> ExpressionComponent {
        /*
        Parse a term.

        TERM ::= FACTOR [ '*' TERM ]
               | FACTOR [ '/' TERM ]
               | FACTOR [ '&' TERM ]
        */
        let matched_factor = self.parse_factor();
        match self.peek() {
            Star => {
                self.expect(Star);
                let next_term = self.parse_term();
                Product(~matched_factor, ~next_term)
            },
            ForwardSlash => {
                self.expect(ForwardSlash);
                let next_term = self.parse_term();
                Quotient(~matched_factor, ~next_term)
            }
            Ampersand => {
                self.expect(Ampersand);
                let next_term = self.parse_term();
                BitwiseAnd(~matched_factor, ~next_term)
            }
            _ => matched_factor
        }
    }

    pub fn parse_arith(&mut self) -> ExpressionComponent {
        /*
        Parse an arithmetic expression.

        ARITH ::= TERM [ '+' ARITH ]
                | TERM [ '-' ARITH ]
                | TERM [ '|' ARITH ]
         */
        
        let parsed_term = self.parse_term();
        match self.peek() {
            Plus => { self.expect(Plus);
                      let next_term = self.parse_arith();
                      Sum(~parsed_term, ~next_term)
            },
            Dash => { self.expect(Dash);
                      let next_term = self.parse_arith();
                      Difference(~parsed_term, ~next_term)
            }
            Pipe => { self.expect(Pipe);
                      let next_term = self.parse_arith();
                      BitwiseOr(~parsed_term, ~next_term)
            }
            _ => parsed_term
        }
    }

    pub fn parse_bool_factor(&mut self) -> ExpressionComponent {
        /*
        Parse a boolean factor.

        BOOL_FACTOR ::= ARITH [ '==' BOOL_FACTOR ]
                      | ARITH [ '<=' BOOL_FACTOR ]
                      | ARITH [ '>=' BOOL_FACTOR ]
                      | ARITH [ '<' BOOL_FACTOR ]
                      | ARITH [ '>' BOOL_FACTOR ]
        */
        let parsed_arith = self.parse_arith();
        match self.peek() {
            EqEq => { self.expect(EqEq);
                      EqualsExpr(~parsed_arith, ~self.parse_bool_factor())
            },
            LessEq => { self.expect(LessEq);
                      LessEquals(~parsed_arith, ~self.parse_bool_factor())
            },
            Less => { self.expect(Less);
                      LessExpr(~parsed_arith, ~self.parse_bool_factor())
            },
            GreaterEq => { self.expect(GreaterEq);
                      GreaterEquals(~parsed_arith, ~self.parse_bool_factor())
            },
            Greater => { self.expect(Greater);
                      GreaterExpr(~parsed_arith, ~self.parse_bool_factor())
            },
            _ => parsed_arith
        }
    }

    pub fn parse_bool_term(&mut self) -> ExpressionComponent {
        /*
        Parse a boolean term.

        BOOL_TERM ::= BOOL_FACTOR [ '&&' BOOL_TERM ]
        */
        let parsed_factor = self.parse_bool_factor();
        match self.peek() {
            AmpAmp => { self.expect(AmpAmp);
                        LogicalAnd(~parsed_factor, ~self.parse_bool_term())
            },
            _ => parsed_factor
        }
    }

    pub fn parse_bool_arith(&mut self) -> ExpressionComponent {
        /*
        Parse a boolean arithmetic expression.

        BOOL_ARITH ::= BOOL_TERM [ '||' BOOL_ARITH ]
        */
        let parsed_term = self.parse_bool_term();
        match self.peek() {
            PipePipe => { self.expect(PipePipe);
                          LogicalOr(~parsed_term, ~self.parse_bool_term())
            },
            _ => parsed_term
        }
    }

    pub fn parse_assignment(&mut self) -> ExpressionComponent {
        /*
        Parse a (possible) assignment.

        ASSIGN ::= BOOL_ARITH [ '=' ASSIGN ]
        */
        let parsed_bool_arith = self.parse_bool_arith();
        match self.peek() {
            Eq => { self.expect(Eq);
                    Assignment(~parsed_bool_arith, ~self.parse_assignment())
            },
            _ => parsed_bool_arith
        }
    }

    pub fn parse_ternary(&mut self) -> ExpressionComponent {
        /*
        Parse a (possible) ternary conditional operator.

        TERNARY ::= ASSIGNMENT [ '?' ASSIGNMENT ':' ASSIGNMENT ]
        */
        let parsed_assignment = self.parse_assignment();
        match self.peek() {
            QuestionMark => {
                self.expect(QuestionMark);
                let second_assignment = self.parse_assignment();
                self.expect(Colon);
                let third_assignment = self.parse_assignment();
                TernaryConditional(~parsed_assignment,
                                    ~second_assignment,
                                    ~third_assignment)
            },
            _ => parsed_assignment
        }
    }

    pub fn parse_expr(&mut self) -> ExpressionComponent {
        self.parse_ternary()
    }

    pub fn parse_type(&mut self) -> Type {
        let mut result;
        match self.peek() {
            Star => {
                self.expect(Star);
                let inner_type = self.parse_type();
                result = PointerTo(~inner_type);
            }
            LParen => {
                self.expect(LParen);
                let inner_type = self.parse_type();
                self.expect(RParen);
                result = inner_type;
            }
            Ident => {
                let ident_name = self.expect(Ident);
                result = NamedType(ident_name);
            }
            _ => { fail!(); }
        }

        loop {
            match self.peek() {
                Arrow => {
                    self.expect(Arrow);
                    let dest_type = self.parse_type();
                    return FunctionType(~result, ~dest_type);
                },
                LBracket => {
                    self.expect(LBracket);
                    let dimension = self.parse_number();
                    self.expect(RBracket);
                    result = ArrayType(~result, dimension);
                }
                _ => return result
            }
        }
    }

    pub fn parse_type_declaration(&mut self) -> VariableDeclarationEnum {
        self.expect(Let);
        let var_name = self.expect(Ident);
        let var_type =
            match self.peek() {
                Colon => {
                    self.expect(Colon);
                    Some(self.parse_type())
                },
                Eq => None,
                _ => fail!()
            };
        match self.peek() {
            Semicolon => {
                self.expect(Semicolon);
                VariableDeclaration(var_name, ~var_type.unwrap())
            },
            Eq => {
                self.expect(Eq);
                let var_value = self.parse_expr();
                self.expect(Semicolon);
                match var_type {
                    None => VariableDeclarationInit(var_name, ~var_value),
                    Some(var_type) =>
                        TypedVariableDeclarationInit(var_name, ~var_type,
                                                     ~var_value)
                }
            },
            _ => { fail!(); }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::defaults;
    use super::Parser;
    use super::ExpressionComponent;
    use lexer::Lexer;

    fn mknum(n: i64) -> ExpressionComponent {
        Num(n, defaults::DEFAULT_NUM_TYPE)
    }

    #[test]
    fn test() {
        let lexer = Lexer::new(vec!(~r#"1+3*5/2-2*3*(5+6)"#).move_iter());
        let mut parser = Parser::new(lexer);
        assert_eq!(parser.parse_expr(),
                   Sum(
                       ~mknum(1),
                       ~Difference(
                           ~Product(
                               ~mknum(3),
                               ~Quotient(
                                   ~mknum(5),
                                   ~mknum(2)
                                        )
                                   ),
                           ~Product(
                               ~mknum(2),
                               ~Product(
                                   ~mknum(3),
                                   ~Sum(
                                       ~mknum(5),
                                       ~mknum(6)
                                           )
                                       )
                                   )
                               )
                           )
                   );
    }
}
