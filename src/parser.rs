use ast::IntKind;
use lexer::*;
use lexer::SourceToken;
use std::fmt::{Formatter, Result, Show};
use std::iter::Peekable;
use std::num;
use std::vec;

// Numbers by default are 32-bit, signed.
mod defaults {
    use ast::{IntKind, Signed, Width32};
    pub static DEFAULT_INT_KIND: IntKind = IntKind { signedness: Signed, width: Width32 };
}

#[deriving(Eq, Show)]
/// One part of a compound expression.
pub enum CompoundExpressionComponent {
    Declaration(~VariableDeclarationEnum),
    Expression(~ExpressionComponent),
}

#[deriving(Eq)]
pub enum ExpressionComponent {
    Num(u64, IntKind),
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
    FunctionApplication(~ExpressionComponent, Vec<ExpressionComponent>),
    FunctionApplicationTypeParams(~ExpressionComponent,
                                  Vec<Type>,
                                  Vec<ExpressionComponent>),
    Cast(~ExpressionComponent, ~Type),
    CompoundExpression(Vec<CompoundExpressionComponent>),
    IfExpression(~ExpressionComponent,
                 ~ExpressionComponent),
    IfElseExpression(~ExpressionComponent,
                     ~ExpressionComponent,
                     ~ExpressionComponent),
    TupleExpr(Vec<ExpressionComponent>),
    Unit,
    FieldAccess(~ExpressionComponent,
                ~str),
    ReturnExpr(~ExpressionComponent),
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
            FunctionApplicationTypeParams(ref e1, ref typeargs, ref args) =>
                write!(f.buf, "{}::<{}>({})", e1, typeargs, args),
            Cast(ref e, ref t) => write!(f.buf, "({} as {})", e, t),
            CompoundExpression(ref l) =>
                write!(f.buf, "\\{ {} \\}", l),
            IfExpression(ref c, ref b) =>
                write!(f.buf, "if ({}) {}", c, b),
            IfElseExpression(ref c, ref b, ref e) =>
                write!(f.buf, "if ({}) {} else {}", c, b, e),
            TupleExpr(ref t) => write!(f.buf, "({})", t),
            Unit => write!(f.buf, "Unit"),
            FieldAccess(ref e, ref c) => write!(f.buf, "({}.{})", e, c),
            ReturnExpr(ref e) => write!(f.buf, "return ({});", e),
        }
    }
}

/// A single argument to a function.
#[deriving(Eq)]
pub struct FuncArg {
    name: ~str,
    argtype: Type,
}

impl Show for FuncArg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f.buf, "{}: {}", self.name, self.argtype)
    }
}

/// Anything that is declared at the top level of a file.
#[deriving(Eq)]
pub enum TopLevelDecl {
    FunctionDecl(~str, Vec<FuncArg>, Type, ExpressionComponent),
    FunctionDeclWithTypeParam(~str, Vec<~str>, Vec<FuncArg>, Type,
                              ExpressionComponent),
}

impl Show for TopLevelDecl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            FunctionDecl(ref name, ref args, ref functype, ref body) =>
                write!(f.buf, "fn {}({}) -> {} {}",
                       name, args, functype, body),
            FunctionDeclWithTypeParam(ref name, ref type_params, ref args,
                                      ref functype, ref body) =>
                write!(f.buf, "fn {}<{}>({}) -> {} {}",
                       name, type_params, args, functype, body),

        }
    }
}

#[deriving(Eq)]
pub enum TopLevel {
    TopLevelStatements(Vec<TopLevelDecl>),
}

impl Show for TopLevel {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            TopLevelStatements(ref v) => {
                write!(f.buf, "{}",
                       v.iter()
                       .fold(~"", |s, i| (s+"\n"+format!("{}", i))))
            }
        }
    }
}


/// Types
#[deriving(Eq)]
pub enum Type {
    PointerTo(~Type),
    NamedType(~str),
    FunctionType(~Type, ~Type),
    ArrayType(~Type, u64),
    ParameterizedType(~str, Vec<Type>),
    UnitType,
    TupleType(Vec<Type>),
}


impl Show for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            PointerTo(ref t) => write!(f.buf, "(*{})", t),
            NamedType(ref s) => write!(f.buf, "{}", s),
            FunctionType(ref t1, ref t2) => write!(f.buf, "({} -> {})", t1, t2),
            ArrayType(ref t1, n) => write!(f.buf, "({}[{}])", t1, n),
            ParameterizedType(ref t, ref p) => write!(f.buf, "{}<{}>", t, p),
            UnitType => write!(f.buf, "()"),
            TupleType(ref v) => write!(f.buf, "({})", v),
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

pub fn new_from_string(s: ~str) -> 
    Parser<SourceToken,
Lexer<vec::MoveItems<~str>>>
{
    let lexer = Lexer::new(vec!(s).move_iter());
    Parser::new(lexer)
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
                               _ => fail!()
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
            Some(st) => st.tok,
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

    fn expect_ident(&mut self) -> ~str {
        match self.eat() {
            Ident(name) => name,
            tok => self.error(format!("Expected ident, found {}", tok))
        }
    }

    fn expect_number(&mut self) -> u64 {
        match self.eat() {
            Number(num, _) => num,
            tok => self.error(format!("Unexpected {} where number expected", tok))
        }
    }

    fn error<'a>(&self, message: &'a str) -> ! {
        fail!("{}", message)
    }

    fn parse_typed_literal(&mut self) -> ExpressionComponent {
        match self.eat() {
            True              => TrueConstant,
            False             => FalseConstant,
            String(s)         => StringConstant(s),
            Number(num, kind) => Num(num, kind.unwrap_or(defaults::DEFAULT_INT_KIND)),
            tok               => self.error(format!("Unexpected {} where literal expected", tok))
        }
    }

    fn parse_index(&mut self) -> ExpressionComponent {
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
        let mut current_index =
            match *self.peek() {
                // '(' expr ')'
                LParen => {
                    self.expect(LParen);
                    let mut inner_exprs = parse_list!(self.parse_expr()
                                                      until RParen);
                    self.expect(RParen);
                    if inner_exprs.len() == 0 {
                        Unit
                    } else if inner_exprs.len() == 1 {
                        inner_exprs.pop().unwrap()
                    } else {
                        TupleExpr(inner_exprs)
                    }
                },
                LBrace => {
                    self.parse_compound_expr()
                }
                Number(..) | True | False | String(..) => {
                    self.parse_typed_literal()
                },
                Ident(_) => {
                    Identifier(self.expect_ident())
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

        loop {
            current_index =
                match *self.peek() {
                    LBracket => {
                        self.expect(LBracket);
                        let indexing_expr = self.parse_expr();
                        self.expect(RBracket);
                        Index(~current_index, ~indexing_expr)
                    },
                    ColonColon => {
                        self.expect(ColonColon);
                        self.expect(Less);
                        let type_params = parse_list!(self.parse_type()
                                                      until Greater);
                        self.expect(Greater);
                        self.expect(LParen);
                        let args = parse_list!(self.parse_expr() until RParen);
                        self.expect(RParen);
                        FunctionApplicationTypeParams(
                            ~current_index,
                            type_params,
                            args)
                    }
                    LParen => {
                        self.expect(LParen);
                        let args = parse_list!(self.parse_expr() until RParen);
                        self.expect(RParen);
                        FunctionApplication(~current_index, args)
                    },
                    As => {
                        self.expect(As);
                        let type_specifier = self.parse_type();
                        Cast(~current_index, ~type_specifier)
                    },
                    Period => {
                        self.expect(Period);
                        let field = self.expect_ident();
                        FieldAccess(~current_index, field)
                    }
                    Arrow => {
                        self.expect(Arrow);
                        let field = self.expect_ident();
                        FieldAccess(~Dereference(~current_index), field)
                    }
                    _ => return current_index
                };
        }

    }

    fn parse_factor(&mut self) -> ExpressionComponent {
        /*
        Parse a factor.

        FACTOR ::= INDEX
                 | '*' INDEX
                 | '&' INDEX
        */
        match *self.peek() {
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
        match *self.peek() {
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
        match *self.peek() {
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
        match *self.peek() {
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
        match *self.peek() {
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
        match *self.peek() {
            PipePipe => { self.expect(PipePipe);
                          LogicalOr(~parsed_term, ~self.parse_bool_term())
            },
            _ => parsed_term
        }
    }

    pub fn parse_possible_assignment(&mut self) -> ExpressionComponent {
        /*
        Parse a (possible) assignment.

        ASSIGN ::= BOOL_ARITH [ '=' ASSIGN ]
        */
        let parsed_bool_arith = self.parse_bool_arith();
        match *self.peek() {
            Eq => { self.expect(Eq);
                    Assignment(~parsed_bool_arith,
                               ~self.parse_possible_assignment())
            },
            _ => parsed_bool_arith
        }
    }

    pub fn parse_type(&mut self) -> Type {
        let mut result = match *self.peek() {
            Star => {
                self.expect(Star);
                let inner_type = self.parse_type();
                PointerTo(~inner_type)
            }
            LParen => {
                self.expect(LParen);
                let mut inner_types = parse_list!(self.parse_type()
                                                  until RParen);
                self.expect(RParen);
                if inner_types.len() == 0 {
                    UnitType
                } else if inner_types.len() == 1 {
                    inner_types.pop().unwrap()
                } else {
                    TupleType(inner_types)
                }
            }
            Ident(_) => {
                let ident_name = self.expect_ident();
                match *self.peek() {
                    Less => {
                        self.expect(Less);
                        let params = parse_list!(self.parse_type()
                                                 until Greater);
                        self.expect(Greater);
                        ParameterizedType(ident_name, params)
                    }
                    _ => NamedType(ident_name)
                }
            }
            _ => { fail!(); }
        };

        loop {
            match *self.peek() {
                Arrow => {
                    self.expect(Arrow);
                    let dest_type = self.parse_type();
                    return FunctionType(~result, ~dest_type);
                },
                LBracket => {
                    self.expect(LBracket);
                    let dimension = self.expect_number();
                    self.expect(RBracket);
                    result = ArrayType(~result, dimension);
                }
                _ => return result
            }
        }
    }

    pub fn parse_variable_declaration(&mut self) -> VariableDeclarationEnum {
        self.expect(Let);
        let var_name = self.expect_ident();
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

    pub fn parse_if_statement(&mut self) -> ExpressionComponent {
        self.expect(If);
        let cond = self.parse_expr();
        let conditional_statement = self.parse_compound_expr();
        match *self.peek() {
            Else => {
                self.expect(Else);
                let else_statement = self.parse_compound_expr();
                IfElseExpression(~cond, ~conditional_statement, ~else_statement)
            },
            _ => IfExpression(~cond, ~conditional_statement)
        }
    }

    pub fn parse_return_statement(&mut self) -> ExpressionComponent {
        self.expect(Return);
        let result = ReturnExpr(~self.parse_expr());
        self.expect(Semicolon);
        result
    }

    pub fn parse_simple_expr(&mut self) -> ExpressionComponent {
        match *self.peek() {
            If => self.parse_if_statement(),
            Return => self.parse_return_statement(),
            _ => self.parse_possible_assignment()
        }
    }

    pub fn parse_compound_expr(&mut self) -> ExpressionComponent {
        self.expect(LBrace);
        let mut statements = vec!();
        loop {
            match *self.peek() {
                Let => statements.push(
                    Declaration(~self.parse_variable_declaration())),
                Semicolon => {
                    // We ignore empty statements when they're not the
                    // very last one.
                    self.expect(Semicolon);
                }
                RBrace => {
                    self.expect(RBrace);
                    statements.push(Expression(~Unit));
                    return CompoundExpression(statements);
                }
                _ => {
                    statements.push(
                        Expression(~self.parse_simple_expr()));
                    match *self.peek() {
                        Semicolon => {
                            self.expect(Semicolon);
                        },
                        RBrace => {
                            self.expect(RBrace);
                            return CompoundExpression(statements);
                        }
                        _ => fail!()
                    }
                }
            }
        }
    }

    pub fn parse_expr(&mut self) -> ExpressionComponent {
        match *self.peek() {
            LBrace => self.parse_compound_expr(),
            _ => self.parse_simple_expr()
        }
    }

    pub fn parse_function_argument(&mut self) -> FuncArg {
        let arg_name = self.expect_ident();
        self.expect(Colon);
        let arg_type = self.parse_type();
        FuncArg { name: arg_name,
                  argtype: arg_type }
    }

    pub fn parse_function_declaration(&mut self) -> TopLevelDecl {
        self.expect(Fn);
        let funcname = self.expect_ident();
        let type_params = match *self.peek() {
            Less => {
                self.expect(Less);
                let p = Some(parse_list!(self.expect_ident()
                                         until Greater));
                self.expect(Greater);
                p
            },
            LParen => None,
            _ => fail!(),
        };
        self.expect(LParen);
        let args = parse_list!(self.parse_function_argument() until RParen);
        self.expect(RParen);
        let return_type = match *self.peek() {
            Arrow => { self.expect(Arrow); self.parse_type() }
            _ => UnitType,
        };
        let body = self.parse_compound_expr();
        match type_params {
            Some(v) => FunctionDeclWithTypeParam(funcname, v, args,
                                                 return_type, body),
            None => FunctionDecl(funcname, args, return_type, body)
        }
    }

    pub fn parse_toplevel(&mut self) -> TopLevel {
        let mut result = vec!();
        loop {
            match *self.peek() {
                Fn => result.push(self.parse_function_declaration()),
                Eof => return TopLevelStatements(result),
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
    use super::ExpressionComponent;

    fn mknum(n: u64) -> ExpressionComponent {
        Num(n, defaults::DEFAULT_INT_KIND)
    }

    #[test]
    fn test_basic_arith_expr() {
        let mut parser = new_from_string(~r#"1+3*5/2-2*3*(5+6)"#);
        let tree = parser.parse_simple_expr();
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
            ~"TypedVariableDeclarationInit(x, ((int -> (int[4])) -> (*(int[1]))), (f([((3i32*x)+1i32), g([x])])*(*p)))"
        );
    }
}
