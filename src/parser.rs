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
            loop {
                match *self.peek() {
                    $end_token => break,
                    _ => { res.push($parser);
                           match *self.peek() {
                               $end_token => break,
                               Comma => { self.expect(Comma); },
                               _ => self.peek_error(
                                   format!("Expected {} or comma when parsing list",
                                           $end_token))
                           }
                    }
                }
            }
            res
        }
    )
)

static EOF: Token = Eof;

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
            self.error(format!("Expected {}, found {}",
                               expected, tok),
                       self.last_span.get_begin());
        }
    }
    fn error<'a>(&self, message: &'a str, pos: SourcePos) -> ! {
        fail!("\n{}\nat {}", message, pos)
    }

    fn peek_error<'a>(&mut self, message: &'a str) -> ! {
        let token = self.peek().clone();
        let pos = self.peek_span().get_begin();
        self.error(format!("{} (got token {})", message, token), pos)
    }

    fn new_id(&mut self) -> DefId {
        let id = self.next_id;
        self.next_id += 1;
        DefId(id)
    }

    fn parse_ident(&mut self) -> Ident {
        match self.eat() {
            IdentTok(name) => Ident {
                id: self.new_id(),
                sp: self.last_span,
                tps: None,
                name: name,
            },
            tok => self.error(format!("Expected ident, found {}", tok),
                              self.last_span.get_begin())
        }
    }

    fn expect_number(&mut self) -> u64 {
        match self.eat() {
            NumberTok(num, _) => num,
            tok => self.error(format!("Unexpected {} where number expected",
                                      tok), self.last_span.get_begin())
        }
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
            tok                  => self.error(format!("Unexpected {} where literal expected", tok), self.last_span.get_begin())
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
                if inner_exprs.len() == 0 {
                    UnitExpr
                } else if inner_exprs.len() == 1 {
                    inner_exprs.pop().take_unwrap().val
                } else {
                    TupleExpr(inner_exprs)
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
            _ => self.peek_error("Got unexpected token"),
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
                    CallExpr(~current_index, args)
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
                 | '!' EXPR
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
            Bang => {
                self.expect(Bang);
                let index = self.parse_expr();
                UnOpExpr( span!(Negate, start_span), ~index)
            }
            _ => return self.parse_index()
        };

        span!(node, start_span.to(self.last_span))
    }


    fn parse_binop(&mut self, precedence: uint) -> Expr {
        let num_precedences = 9;

        if precedence == num_precedences {
            return self.parse_factor();
        }

        let mut current = self.parse_binop(precedence+1);

        macro_rules! binop(
            ( $( ($token:ident, $ast_op:ident, $precedence:expr) ),* )
                => ({
                    match *self.peek() {
                        $(
                            $token if precedence == $precedence => {
                                self.expect($token);
                                let rhs = self.parse_binop(precedence+1);
                                let op_span = self.last_span;
                                let exp_span = current.sp.to(rhs.sp);
                                current = 
                                    span!(BinOpExpr(span!($ast_op, op_span),
                                                    ~current, ~rhs),
                                          exp_span);
                            },
                        )+
                            _ => {
                                return current;
                            }
                    }
                })
            );

        loop {
            binop!((Star, TimesOp, num_precedences-1),
                   (ForwardSlash, DivideOp, num_precedences-1),
                   (Percent, ModOp, num_precedences-1),

                   (Plus, PlusOp, 7),
                   (Dash, MinusOp, 7),

                   (Lsh, LeftShiftOp, 6),
                   (Rsh, RightShiftOp, 6),

                   (Ampersand, BitAndOp, 5),
                   (Caret, BitXorOp, 4),
                   (Pipe, BitOrOp, 3),

                   (EqEq, EqualsOp, 2),
                   (BangEq, NotEqualsOp, 2),
                   (LessEq, LessEqOp, 2),
                   (Less, LessOp, 2),
                   (GreaterEq, GreaterEqOp, 2),
                   (Greater, GreaterOp, 2),

                   (AmpAmp, AndAlsoOp, 1),
                   (PipePipe, OrElseOp, 0)
                   );
        }
                
    }

    pub fn parse_possible_assignment(&mut self) -> Expr {
        /*
        Parse a (possible) assignment.

        ASSIGN ::= BOOL_ARITH [ '=' ASSIGN ]
        */
        let start_span = self.peek_span();
        let parsed_bool_arith = self.parse_binop(0);

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
                if inner_types.len() == 0 {
                    UnitType
                } else if inner_types.len() == 1 {
                    inner_types.pop().take_unwrap().val
                } else {
                    TupleType(inner_types)
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
                        Some(ps)
                    }
                    _ => None
                };
                NamedType(id)
            }
            Fn => {
                self.expect(Fn);
                self.expect(LParen);
                let arglist = parse_list!(self.parse_type() until RParen);
                self.expect(RParen);
                self.expect(Arrow);
                FuncType(arglist, ~self.parse_type())
            }
            _ => self.peek_error("Expected *, opening paren, a type name, or fn"),
        };

        let mut result;
        while {
            result = span!(node, start_span.to(self.last_span));
            true
        } {
            node = match *self.peek() {
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
            _ => self.peek_error("Expected \": type\" or \"=\""),
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
            _ => self.peek_error("Expected semicolon or \"=\""),
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
                match *self.peek() {
                    If => {
                        let if_span = self.peek_span();
                        Block {
                            items: vec!(),
                            stmts: vec!(),
                            expr: Some(self.parse_simple_expr()),
                            sp: if_span.to(self.last_span)
                        }
                    },
                    _ => self.parse_block_expr()
                }
            }
            _ => {
                let fake_span = mk_sp(self.last_span.get_end(), 0);
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
                        _ => self.peek_error("Expected a semicolon or closing brace"),
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
                Some(tps)
            },
            LParen => None,
            _ => self.peek_error("Expected type parameters or argument list"),
        }.unwrap_or(vec!());
        self.expect(LParen);
        let args = parse_list!(self.parse_func_arg() until RParen);
        self.expect(RParen);
        let return_type = match *self.peek() {
            Arrow => { self.expect(Arrow); self.parse_type() }
            _ => span!(UnitType,
                       mk_sp(self.last_span.get_end(), 0)),
        };
        let body = self.parse_block_expr();
        span!(FuncItem(funcname, args, return_type, body, type_params),
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
                _ => self.peek_error("Expected a function declaration"),
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
                   ~"((1i32+((3i32*5i32)/2i32))-((2i32*3i32)*(5i32+6i32)))");
    }

    fn compare_canonicalized(raw: ~str, parsed: ~str) {
        let mut parser = new_from_string(raw);
        let tree = parser.parse_variable_declaration();
        assert_eq!(parsed, format!("{}", tree));
    }

    #[test]
    fn test_variable_declarations() {
        compare_canonicalized(
            ~r#"let x: fn(fn(int) -> int[4]) -> *(int[1]) = f(3*x+1, g(x)) * *p;"#,
            ~"let x: ([([int] -> (int)[4])] -> *((int)[1])) = (f([((3i32*x)+1i32), g([x])])*(*p));"
        );
    }

    #[test]
    fn test_variable_declarations_again() {
        compare_canonicalized(
            ~r#"let x: fn(int) -> fn(int[4]) -> (*int)[1] = f(3*x+1, g(x)) * *p;"#,
            ~"let x: ([int] -> ([(int)[4]] -> (*(int))[1])) = (f([((3i32*x)+1i32), g([x])])*(*p));"
        );
    }
}
