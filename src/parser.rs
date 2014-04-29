use span::{SourcePos, Span, Spanned, mk_sp};
use ast::IntKind;
use lexer::*;
use lexer::SourceToken;
use std::fmt::{Formatter, Result, Show};
use std::iter::Peekable;
use std::num;
use std::vec;
use ast::*;

// Numbers by default are 32-bit, signed.
mod defaults {
    use ast::{IntKind, Signed, Width32};
    pub static DEFAULT_INT_KIND: IntKind = IntKind { signedness: Signed, width: Width32 };
}

pub struct Parser<A, T> {
    tokens: ~Peekable<A, T>,
    last_span: Span,
    next_id: u64,
}

pub fn new_from_string(s: ~str) -> 
    Parser<SourceToken,
           Lexer<vec::MoveItems<~str>>>
{
    let lexer = Lexer::new(vec!(s).move_iter());
    Parser::new(lexer)
}

macro_rules! span {
    ( $n:expr, $s:expr ) => ( Spanned { val: $n, sp: $s } );
}

macro_rules! parse_list(
    ($parser:expr until $end_token:ident) => (
        {
            let mut res = vec!();
            let sp = self.peek_span();
            loop {
                match *self.peek() {
                    $end_token => break,
                    _ => { res.push($parser);
                           match *self.peek() {
                               $end_token => break,
                               Comma => { self.expect(Comma); },
                               _ => fail!()
                           }
                    }
                }
            }
            span!(res, sp.to(self.last_span))
        }
    )
)

static EOF: Token = Eof;

macro_rules! binop_handler {
    ($tok:ident, $op:ident, $parse_rest:expr, $parsed:expr) =>
        ({
            self.expect($tok);
            let __op_sp__ = self.last_span;
            BinOpExpr(span!($op, __op_sp__), ~$parsed, ~$parse_rest)
        });
}

impl<T: Iterator<SourceToken>> Parser<SourceToken, T> {
    pub fn new(tokens: T) -> Parser<SourceToken, T> {
        Parser::<SourceToken, T> {
            tokens: ~tokens.peekable(),
            last_span: mk_sp(SourcePos::new(), 0),
            next_id: 0,
        }
    }

    /// Peek at the Span of the next token.
    fn peek_span(&mut self) -> Span {
        match self.tokens.peek() {
            Some(st) => st.sp,
            None => fail!("At EOF."),
        }
    }

    /// "Peek" at the next token, returning the token, without consuming
    /// it from the stream.
    fn peek<'a>(&'a mut self) -> &'a Token {
        match self.tokens.peek() {
            Some(st) => &st.tok,
            None => &EOF,
        }
    }

    fn eat(&mut self) -> Token {
        match self.tokens.next() {
            Some(st) => {
                self.last_span = st.sp;
                st.tok
            },
            None => Eof,
        }
    }

    /// Consume one token from the stream, erroring if it's not
    /// the `expected`. Returns the string corresponding to that
    /// token.
    fn expect(&mut self, expected: Token) {
        let tok = self.eat();
        if tok != expected{
            self.error(format!("Expected {:?}, found {:?}", expected, tok));
        }
    }

    fn new_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn parse_ident(&mut self) -> Ident {
        match self.eat() {
            IdentTok(name) => Ident {
                id: self.new_id(),
                sp: self.last_span,
                tps: None,
                name: name,
            },
            tok => self.error(format!("Expected ident, found {}", tok))
        }
    }

    fn expect_number(&mut self) -> u64 {
        match self.eat() {
            NumberTok(num, _) => num,
            tok => self.error(format!("Unexpected {} where number expected", tok))
        }
    }

    fn error<'a>(&self, message: &'a str) -> ! {
        fail!("{}", message)
    }

    fn parse_typed_literal(&mut self) -> Lit {
        let span = self.peek_span();

        let node = match self.eat() {
            True                 => BoolLit(true),
            False                => BoolLit(false),
            StringTok(s)         => StringLit(s),
            NumberTok(num, kind) => NumLit(num,
                                           kind.unwrap_or(
                                               defaults::DEFAULT_INT_KIND
                                           )
                                    ),
            tok                  => self.error(format!("Unexpected {} where literal expected", tok))
        };

        span!(node, span)
    }

    fn parse_index(&mut self) -> Expr {
        /*
        Parse a possibly-array-indexed expression.

        INDEX ::= '(' EXPR [ , EXPR ...] ')'
                | COMPOUND_EXPRESSION
                | Number | String | True | False
                | INDEX '[' EXPR ']'
                | INDEX '(' ARGLIST ')'
                | INDEX '.' IDENT
                | INDEX 'as' TYPE
                | IDENT
        */
        let start_span = self.peek_span();
        let mut node = match *self.peek() {
            // '(' expr ')'
            LParen => {
                self.expect(LParen);
                let mut inner_exprs = parse_list!(self.parse_expr()
                                                  until RParen);
                self.expect(RParen);
                if inner_exprs.val.len() == 0 {
                    UnitExpr
                } else if inner_exprs.val.len() == 1 {
                    inner_exprs.val.pop().take_unwrap().val
                } else {
                    TupleExpr(inner_exprs.val)
                }
            },
            LBrace => {
                BlockExpr(~self.parse_block_expr())
            }
            NumberTok(..) | True | False | StringTok(..) => {
                LitExpr(self.parse_typed_literal())
            },
            IdentTok(_) => {
                IdentExpr(self.parse_ident())
            }
            Star => {
                self.expect(Star);
                let op = span!(Deref, self.last_span);
                UnOpExpr(op, ~self.parse_factor())
            },
            Ampersand => {
                self.expect(Ampersand);
                let op = span!(AddrOf, self.last_span);
                UnOpExpr(op, ~self.parse_factor())
            }
            _ => { fail!("Parse error."); }
        };

        // lol this is what you get for having block expressions
        let mut current_index;
        while {
            current_index = span!(node, start_span.to(self.last_span));
            true
        } {
            node = match *self.peek() {
                LBracket => {
                    self.expect(LBracket);
                    let indexing_expr = self.parse_expr();
                    self.expect(RBracket);
                    IndexExpr(~current_index, ~indexing_expr)
                },
                // ColonColon is invalid in this position -- it is only valid after an Ident,
                // and right now we are in expression context.  TODO kemurphy handle this with modules
                /*ColonColon => {
                    self.expect(ColonColon);
                    self.expect(Less);
                    let type_params = parse_list!(self.parse_type()
                                                  until Greater);
                    self.expect(Greater);
                    self.expect(LParen);
                    let args = parse_list!(self.parse_expr() until RParen);
                    self.expect(RParen);
                    CallExpr(~current_index, args.val, type_params)
                } */
                LParen => {
                    self.expect(LParen);
                    let args = parse_list!(self.parse_expr() until RParen);
                    self.expect(RParen);
                    CallExpr(~current_index, args.val)
                },
                As => {
                    self.expect(As);
                    CastExpr(~current_index, self.parse_type())
                },
                Period => {
                    self.expect(Period);
                    let field = self.parse_ident();
                    DotExpr(~current_index, field)
                }
                Arrow => {
                    self.expect(Arrow);
                    let field = self.parse_ident();
                    ArrowExpr(~current_index, field)
                }
                _ => return current_index
            };
        }
        unreachable!()
    }

    fn parse_factor(&mut self) -> Expr {
        /*
        Parse a factor.

        FACTOR ::= INDEX
                 | '*' INDEX
                 | '&' INDEX
        */
        let start_span = self.peek_span();
        let node = match *self.peek() {
            Star => {
                self.expect(Star);
                let index = self.parse_index();
                UnOpExpr(span!(Deref, start_span), ~index)
            }
            Ampersand => {
                self.expect(Ampersand);
                let index = self.parse_index();
                UnOpExpr( span!(AddrOf, start_span), ~index)
            }
            _ => return self.parse_index()
        };

        span!(node, start_span.to(self.last_span))
    }

    fn parse_term(&mut self) -> Expr {
        /*
        Parse a term.

        TERM ::= FACTOR [ '*' TERM ]
               | FACTOR [ '/' TERM ]
               | FACTOR [ '&' TERM ]
        */
        let start_span = self.peek_span();
        let parsed_factor = self.parse_factor();
        macro_rules! term(
            ($tok:ident, $op:ident)
                => (binop_handler!($tok, $op, self.parse_term(), parsed_factor)))
        let node = match *self.peek() {
            Star         => term!(Star, TimesOp),
            ForwardSlash => term!(ForwardSlash, DivideOp),
            Ampersand    => term!(Ampersand, BitAndOp),
            _            => return parsed_factor
        };

        span!(node, start_span.to(self.last_span))
    }

    pub fn parse_arith(&mut self) -> Expr {
        /*
        Parse an arithmetic expression.

        ARITH ::= TERM [ '+' ARITH ]
                | TERM [ '-' ARITH ]
                | TERM [ '|' ARITH ]
         */
        let start_span = self.peek_span();
        let parsed_term = self.parse_term();
        macro_rules! arith(
            ($tok:ident, $op:ident)
                => (binop_handler!($tok, $op, self.parse_arith(), parsed_term)))
        let node = match *self.peek() {
            Plus => arith!(Plus, PlusOp),
            Dash => arith!(Dash, MinusOp),
            Pipe => arith!(Pipe, BitOrOp),
            _    => return parsed_term
        };

        span!(node, start_span.to(self.last_span))
    }

    pub fn parse_bool_factor(&mut self) -> Expr {
        /*
        Parse a boolean factor.

        BOOL_FACTOR ::= ARITH [ '==' BOOL_FACTOR ]
                      | ARITH [ '<=' BOOL_FACTOR ]
                      | ARITH [ '>=' BOOL_FACTOR ]
                      | ARITH [ '<' BOOL_FACTOR ]
                      | ARITH [ '>' BOOL_FACTOR ]
        */
        let start_span = self.peek_span();
        let parsed_arith = self.parse_arith();

        macro_rules! bool_factor(
            ($tok:ident, $op:ident)
                => (binop_handler!($tok, $op, self.parse_bool_factor(), parsed_arith)))

        let node = match *self.peek() {
            EqEq      => bool_factor!(EqEq, EqualsOp),
            LessEq    => bool_factor!(LessEq, LessEqOp),
            Less      => bool_factor!(Less, LessOp),
            GreaterEq => bool_factor!(GreaterEq, GreaterEqOp),
            Greater   => bool_factor!(Greater, GreaterOp),
            _         => return parsed_arith
        };

        span!(node, start_span.to(self.last_span))
    }

    pub fn parse_bool_term(&mut self) -> Expr {
        /*
        Parse a boolean term.

        BOOL_TERM ::= BOOL_FACTOR [ '&&' BOOL_TERM ]
        */
        let start_span = self.peek_span();
        let parsed_factor = self.parse_bool_factor();

        macro_rules! bool_term(
            ($tok:ident, $op:ident)
                => (binop_handler!($tok, $op, self.parse_bool_term(), parsed_factor)))

        let node = match *self.peek() {
            AmpAmp => bool_term!(AmpAmp, AndAlsoOp),
            _      => return parsed_factor
        };

        span!(node, start_span.to(self.last_span))
    }

    pub fn parse_bool_arith(&mut self) -> Expr {
        /*
        Parse a boolean arithmetic expression.

        BOOL_ARITH ::= BOOL_TERM [ '||' BOOL_ARITH ]
        */
        let start_span = self.peek_span();
        let parsed_term = self.parse_bool_term();

        macro_rules! bool_arith(
            ($tok:ident, $op:ident)
                => (binop_handler!($tok, $op, self.parse_bool_arith(), parsed_term)))

        let node = match *self.peek() {
            PipePipe => bool_arith!(PipePipe, OrElseOp),
            _        => return parsed_term
        };

        span!(node, start_span.to(self.last_span))
    }

    pub fn parse_possible_assignment(&mut self) -> Expr {
        /*
        Parse a (possible) assignment.

        ASSIGN ::= BOOL_ARITH [ '=' ASSIGN ]
        */
        let start_span = self.peek_span();
        let parsed_bool_arith = self.parse_bool_arith();

        let node = match *self.peek() {
            Eq => {
                self.expect(Eq);
                AssignExpr(~parsed_bool_arith, ~self.parse_possible_assignment())
            },
            _ => return parsed_bool_arith
        };

        span!(node, start_span.to(self.last_span))
    }

    pub fn parse_type(&mut self) -> Type {
        let start_span = self.peek_span();
        let mut node = match *self.peek() {
            Star => {
                self.expect(Star);
                PtrType(~self.parse_type())
            }
            LParen => {
                self.expect(LParen);
                let mut inner_types = parse_list!(self.parse_type()
                                                  until RParen);
                self.expect(RParen);
                if inner_types.val.len() == 0 {
                    UnitType
                } else if inner_types.val.len() == 1 {
                    inner_types.val.pop().take_unwrap().val
                } else {
                    TupleType(inner_types.val)
                }
            }
            IdentTok(_) => {
                let mut id = self.parse_ident();
                id.tps = match *self.peek() {
                    Less => {
                        self.expect(Less);
                        let ps = parse_list!(self.parse_type()
                                             until Greater);
                        self.expect(Greater);
                        Some(ps.val)
                    }
                    _ => None
                };
                NamedType(id)
            }
            _ => { fail!(); }
        };

        let mut result;
        while {
            result = span!(node, start_span.to(self.last_span));
            true
        } {
            node = match *self.peek() {
                Arrow => {
                    self.expect(Arrow);
                    FuncType(~result, ~self.parse_type())
                },
                LBracket => {
                    self.expect(LBracket);
                    let len = self.expect_number();
                    self.expect(RBracket);
                    ArrayType(~result, len)
                }
                _ => return result
            }
        }
        unreachable!()
    }

    pub fn parse_variable_declaration(&mut self) -> Stmt {
        let start_span = self.peek_span();
        self.expect(Let);
        let var_name = self.parse_ident();
        let var_type = match *self.peek() {
            Colon => {
                self.expect(Colon);
                Some(self.parse_type())
            },
            Eq => None,
            _ => fail!()
        };

        match *self.peek() {
            Semicolon => {
                let span = start_span.to(self.peek_span());
                self.expect(Semicolon);
                span!(LetStmt(var_name, var_type, None),
                            span)
            },
            Eq => {
                self.expect(Eq);
                let var_value = self.parse_expr();
                let span = start_span.to(self.peek_span());
                self.expect(Semicolon);
                span!(LetStmt(var_name, var_type, Some(var_value)),
                            span)
            },
            _ => { fail!(); }
        }
    }

    pub fn parse_if_statement(&mut self) -> Expr {
        let start_span = self.peek_span();
        self.expect(If);

        let cond = self.parse_expr();
        let true_block = self.parse_block_expr();

        let false_block = match *self.peek() {
            Else => {
                self.expect(Else);
                self.parse_block_expr()
            }
            _ => {
                let fake_span = mk_sp(self.last_span.end, 0);
                Block {
                    items: vec!(),
                    stmts: vec!(),
                    expr: Some(span!(UnitExpr, fake_span)),
                    sp: fake_span,
                }
            }
        };

        span!(IfExpr(~cond, ~true_block, ~false_block),
              start_span.to(self.last_span))
    }

    pub fn parse_return_statement(&mut self) -> Expr {
        let start_span = self.peek_span();
        self.expect(Return);
        let result = ReturnExpr(~self.parse_expr());
        let span = start_span.to(self.peek_span());
        self.expect(Semicolon);
        span!(result, span)
    }

    pub fn parse_simple_expr(&mut self) -> Expr {
        match *self.peek() {
            If => self.parse_if_statement(),
            Return => self.parse_return_statement(),
            _ => self.parse_possible_assignment()
        }
    }

    pub fn parse_block_expr(&mut self) -> Block {
        let start_span = self.peek_span();
        self.expect(LBrace);
        let mut statements = vec!();
        loop {
            match *self.peek() {
                Let => {
                    let variable_decl = self.parse_variable_declaration();
                    statements.push(variable_decl);
                }
                Semicolon => {
                    // We ignore empty statements when they're not the
                    // very last one.
                    self.expect(Semicolon);
                }
                RBrace => {
                    self.expect(RBrace);
                    return Block {
                        items: vec!(),
                        stmts: statements,
                        expr: None,
                        sp: start_span.to(self.last_span),
                    }
                }
                _ => {
                    let simple_expr = self.parse_simple_expr();
                    match *self.peek() {
                        Semicolon => {
                            let span = simple_expr.sp.clone();
                            let simple_expr_span = span!(
                                ExprStmt(simple_expr),
                                span);
                            statements.push(simple_expr_span);
                            self.expect(Semicolon);
                        },
                        RBrace => {
                            self.expect(RBrace);
                            return Block {
                                items: vec!(),
                                stmts: statements,
                                expr: Some(simple_expr),
                                sp: start_span.to(self.last_span)
                            }
                        }
                        _ => fail!()
                    }
                }
            }
        }
    }

    pub fn parse_expr(&mut self) -> Expr {
        match *self.peek() {
            LBrace => {
                let start_span = self.peek_span();
                span!(BlockExpr(~self.parse_block_expr()),
                      start_span.to(self.last_span))
            }
            _ => self.parse_simple_expr()
        }
    }

    pub fn parse_func_arg(&mut self) -> FuncArg {
        let start_span = self.peek_span();
        let arg_id = self.parse_ident();
        self.expect(Colon);
        let arg_type = self.parse_type();
        FuncArg {
            ident: arg_id,
            argtype: arg_type,
            sp: start_span.to(self.last_span)
        }
    }

    pub fn parse_func_decl(&mut self) -> Item {
        self.expect(Fn);
        let begin = self.last_span;
        let funcname = self.parse_ident();
        let type_params = match *self.peek() {
            Less => {
                self.expect(Less);
                let tps = parse_list!(self.parse_ident()
                                      until Greater);
                self.expect(Greater);
                Some(tps.val)
            },
            LParen => None,
            _ => fail!(),
        }.unwrap_or(vec!());
        self.expect(LParen);
        let args = parse_list!(self.parse_func_arg() until RParen);
        self.expect(RParen);
        let return_type = match *self.peek() {
            Arrow => { self.expect(Arrow); self.parse_type() }
            _ => span!(UnitType,
                             mk_sp(self.last_span.end, 0)),
        };
        let body = self.parse_block_expr();
        span!(FuncItem(funcname, args.val, return_type, body, type_params),
                    begin.to(self.last_span))
    }

    pub fn parse_module(&mut self) -> Module {
        let mut items = vec!();
        loop {
            match *self.peek() {
                Fn => items.push(self.parse_func_decl()),
                Eof => return Module{
                    items: items
                },
                _ => fail!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::defaults;
    use super::Parser;
    use ast::Expr;

    #[test]
    fn test_basic_arith_expr() {
        let mut parser = new_from_string(~r#"1+3*5/2-2*3*(5+6)"#);
        let tree = parser.parse_simple_expr();

        /* TODO handroll the new AST

        fn mknum(n: u64) -> Expr {
            Num(n, defaults::DEFAULT_INT_KIND)
        }

        assert_eq!(tree,
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
        */
        assert_eq!(format!("{}", tree),
                   ~"(1i32+((3i32*(5i32/2i32))-(2i32*(3i32*(5i32+6i32)))))");
    }

    fn compare_canonicalized(raw: ~str, parsed: ~str) {
        let mut parser = new_from_string(raw);
        let tree = parser.parse_variable_declaration();
        assert_eq!(format!("{}", tree), parsed);
    }

    #[test]
    fn test_variable_declarations() {
        compare_canonicalized(
            ~r#"let x: (int -> int[4]) -> *(int[1]) = f(3*x+1, g(x)) * *p;"#,
            ~"let x: ((int -> (int)[4]) -> *((int)[1])) = (f([((3i32*x)+1i32), g([x])])*(*p));"
        );
    }

    #[test]
    fn test_variable_declarations_again() {
        compare_canonicalized(
            ~r#"let x: int -> int[4] -> (*int)[1] = f(3*x+1, g(x)) * *p;"#,
            ~"let x: (int -> ((int)[4] -> (*(int))[1])) = (f([((3i32*x)+1i32), g([x])])*(*p));"
        );
    }
}
