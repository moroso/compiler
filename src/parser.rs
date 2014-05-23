/* This is the parser for the Moroso compiler, taking the stream of tokens
 * produced by the lexer and giving us an abstract syntax tree (AST).
 *
 * The parser is a predictive recursive descent parser
 * (see http://en.wikipedia.org/wiki/Recursive_descent_parser ). The idea
 * is that for each node in the grammar, we have a corresponding function
 * to parse that node. We may peek at the next token in the stream (this
 * is done with the peek() function), or consume tokens from the stream
 * (with the expect(token) function, which verifies that the token we're
 * consuming is the one we expected to have, or the eat() function, which
 * will consume the next token regardless of what it is).
 *
 * Each item in the token stream, and each node in the AST, also has a "span"
 * associated with it, which keeps track of what characters in the input file
 * correspond to it. As we build AST nodes, we have to take the spans for
 * the tokens and the other AST nodes and use those to build spans for the
 * nodes we build.
 */

use collections::{HashMap, TreeMap};
use session::Interner;
use span::{SourcePos, Span, mk_sp};
use util::Name;

use std::{io, mem, num, vec};
use std::iter::Peekable;

use ast::*;
use values::*;
use lexer::*;

/// Context associated with a full parsing session
pub struct Parser {
    /// Each AST node is given a unique identifier. This keeps track of the
    /// next number to assign to an identifier.
    next_id: uint,
    /// Tracks the corresponding source position of each AST node.
    spanmap: TreeMap<NodeId, Span>,
    /// Tracks the corresponding file name of each AST node.
    filemap: TreeMap<NodeId, Name>,
}

/// The state for parsing a stream of tokens into an AST node
pub struct StreamParser<'a, T> {
    /// The token stream.
    tokens: Peekable<SourceToken, Lexer< T>>,
    /// The name of the current stream being parsed.
    name: Name,
    /// The span corresponding to the last token we consumed from the stream.
    last_span: Span,
    /// The parser that is parsing this stream
    parser: &'a mut Parser,
    /// Reference to the session's interner
    interner: &'a mut Interner,
}

enum Assoc {
    LeftAssoc,
    RightAssoc,
    NonAssoc,
}

struct OpTable {
    rows: &'static [OpTableRow],
}

struct OpTableRow {
    assoc: Assoc,
    ops: uint,
}

fn unop_from_token(t: &Token) -> Option<UnOpNode> {
    match *t {
        Dash => Some(Negate),
        Tilde => Some(BitNot),
        Bang => Some(LogNot),
        Ampersand => Some(AddrOf),
        Star => Some(Deref),
        _ => None,
    }
}

fn binop_from_token(t: &Token) -> Option<BinOpNode> {
    match *t {
        Plus => Some(PlusOp),
        Dash => Some(MinusOp),
        Star => Some(TimesOp),
        ForwardSlash => Some(DivideOp),
        Percent => Some(ModOp),
        EqEq => Some(EqualsOp),
        BangEq => Some(NotEqualsOp),
        Less => Some(LessOp),
        LessEq => Some(LessEqOp),
        Greater => Some(GreaterOp),
        GreaterEq => Some(GreaterEqOp),
        AmpAmp => Some(AndAlsoOp),
        PipePipe => Some(OrElseOp),
        Ampersand => Some(BitAndOp),
        Pipe => Some(BitOrOp),
        Caret => Some(BitXorOp),
        Lsh => Some(LeftShiftOp),
        Rsh => Some(RightShiftOp),
        _ => None,
    }
}

fn unop_token(op: UnOpNode) -> Token {
    match op {
        Deref => Star,
        AddrOf => Ampersand,
        Negate => Dash,
        LogNot => Bang,
        BitNot => Tilde,
    }
}

fn binop_token(op: BinOpNode) -> Token {
    match op {
        PlusOp => Plus,
        MinusOp => Dash,
        TimesOp => Star,
        DivideOp => ForwardSlash,
        ModOp => Percent,
        EqualsOp => EqEq,
        NotEqualsOp => BangEq,
        LessOp => Less,
        LessEqOp => LessEq,
        GreaterOp => Greater,
        GreaterEqOp => GreaterEq,
        AndAlsoOp => AmpAmp,
        OrElseOp => PipePipe,
        BitAndOp => Ampersand,
        BitOrOp => Pipe,
        BitXorOp => Caret,
        LeftShiftOp => Lsh,
        RightShiftOp => Rsh,
    }
}

// Convenience function for testing
pub fn ast_from_str<U>(s: &str, f: |&mut StreamParser<io::BufferedReader<io::MemReader>>| -> U) -> (Interner, U) {
    let mut parser = Parser::new();
    let mut interner = Interner::new();
    let tree = parser.parse_with(lexer_from_str(s), &mut interner, f);
    (interner, tree)
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            next_id: 0,
            spanmap: TreeMap::new(),
            filemap: TreeMap::new(),
        }
    }

    /// Get the Span of a certain node in the AST.
    pub fn span_of(&self, id: &NodeId) -> Span {
        *self.spanmap.find(id).unwrap()
    }

    /// Get the name of a certain node in the AST.
    pub fn filename_of(&self, id: &NodeId) -> Name {
        *self.filemap.find(id).unwrap()
    }

    fn new_id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id += 1;
        NodeId(id)
    }

    pub fn parse<T: Buffer>(&mut self, lexer: Lexer<T>, interner: &mut Interner) -> Module {
        self.parse_with(lexer, interner, |p| p.parse_module())
    }

    pub fn parse_with<T: Buffer, U>(&mut self,
                                    lexer: Lexer<T>,
                                    interner: &mut Interner,
                                    f: |&mut StreamParser<T>| -> U) -> U {
        let mut tokp = StreamParser::new(lexer, interner, self);
        f(&mut tokp)
    }
}

/**
A helper macro to parse a comma-separated lists.
`parser` is an expression to parse one item of the list, and
`end_token` is the token that indicates the end of the list (probably
a closing paren, brace, or similar).
For example,
```ignore
self.expect(LParen);
parse_list(self.parse_expr() until RParen);
self.expect(RParen);
```
will parse a parenthesized, comma-separated list of expressions.
**/
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

impl<'a, T: Buffer> StreamParser<'a, T> {
    fn new(lexer: Lexer<T>, interner: &'a mut Interner, parser: &'a mut Parser) -> StreamParser<'a, T> {
        let name = interner.intern(lexer.get_name());
        let tokens = lexer.peekable();

        StreamParser {
            name: name,
            tokens: tokens,
            parser: parser,
            interner: interner,
            last_span: mk_sp(SourcePos::new(), 0),
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
            None => fail!("Tried to peek past EOF"),
        }
    }

    /// Consume the next token from the stream, returning it.
    fn eat(&mut self) -> Token {
        match self.tokens.next() {
            Some(st) => {
                self.last_span = st.sp;
                st.tok
            }
            None => fail!("Tried to read past EOF"),
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

    fn error<'a, T: Str>(&self, message: T, pos: SourcePos) -> ! {
        fail!("\n{}\nat {}", message.as_slice(), pos)
    }

    /// A convenience function to generate an error message when we've
    /// peeked at a token, but it doesn't match any token we were expecting.
    fn peek_error<'a, T: Str>(&mut self, message: T) -> ! {
        let tok = self.peek().clone();
        let pos = self.peek_span().get_begin();
        self.error(format!("{} (got token {})", message.as_slice(), tok), pos)
    }

    fn add_id_and_span<T>(&mut self, val: T, sp: Span) -> WithId<T> {
        let id = self.parser.new_id();
        self.parser.spanmap.insert(id, sp);
        self.parser.filemap.insert(id, self.name);
        WithId { val: val, id: id }
    }

    /////////////////////////////////////////////////////////////////////
    // The actual parser functions begin here!
    // Most functions from this point on are for parsing a specific node
    // in the grammar.

    fn parse_name(&mut self) -> Name {
        match self.eat() {
            IdentTok(name) => self.interner.intern(name),
            tok => self.error(format!("Expected ident, found {}", tok),
                              self.last_span.get_begin())
        }
    }

    fn parse_ident(&mut self) -> Ident {
        let ident = IdentNode {
            name: self.parse_name(),
            tps: None,
        };

        self.add_id_and_span(ident, self.last_span)
    }

    fn expect_number(&mut self) -> u64 {
        match self.eat() {
            NumberTok(num, _) => num,
            tok => self.error(format!("Unexpected {} where number expected",
                                      tok), self.last_span.get_begin())
        }
    }

    pub fn parse_lit(&mut self) -> Lit {
        let node = match self.eat() {
            True                 => BoolLit(true),
            False                => BoolLit(false),
            StringTok(s)         => StringLit(s),
            NumberTok(num, kind) => NumLit(num, kind),
            tok                  => self.error(format!("Unexpected {} where literal expected", tok), self.last_span.get_begin())
        };

        self.add_id_and_span(node, self.last_span)
    }

    fn parse_pat_common(&mut self, allow_types: bool) -> Pat {
        let start_span = self.peek_span();

        let maybe_type = |me: &mut StreamParser<'a, T>, allow_types| {
            match *me.peek() {
                Colon if allow_types => {
                    me.expect(Colon);
                    let t = me.parse_type();
                    Some(t)
                }
                _ => None
            }
        };

        let pat = match *self.peek() {
            IdentTok(..) => {
                let ident = self.parse_ident();
                match *self.peek() {
                    LParen => {
                        self.expect(LParen);
                        let args = parse_list!(self.parse_pat_common(allow_types)
                                               until RParen);
                        self.expect(RParen);
                        VariantPat(ident, args)
                    }
                    DoubleArrow => {
                        // Empty variant.
                        VariantPat(ident, vec!())
                    }
                    LBrace => {
                        self.expect(LBrace);
                        let field_pats = parse_list!(self.parse_field_pat()
                                                     until RBrace);
                        self.expect(RBrace);
                        StructPat(ident, field_pats)
                    }
                    _ => IdentPat(ident, maybe_type(self, allow_types))
                }
            }
            LParen => {
                self.expect(LParen);
                let args = parse_list!(self.parse_pat_common(allow_types)
                                       until RParen);
                self.expect(RParen);
                TuplePat(args)
            }
            Underscore => {
                DiscardPat(maybe_type(self, allow_types))
            }
            _ => self.peek_error("Unexpected token while parsing pattern")
        };

        self.add_id_and_span(pat, start_span.to(self.last_span))
    }

    pub fn parse_pat(&mut self) -> Pat {
        self.parse_pat_common(true)
    }

    pub fn parse_typeless_pat(&mut self) -> Pat {
        self.parse_pat_common(false)
    }

    pub fn parse_field_pat(&mut self) -> FieldPat {
        let name = self.parse_name();
        self.expect(Colon);
        let pat = self.parse_typeless_pat();
        FieldPat {
            name: name,
            pat: pat,
        }
    }

    pub fn parse_type(&mut self) -> Type {
        /*
        Parse a type.
        */
        let start_span = self.peek_span();
        let mut node = match *self.peek() {
            IntTypeTok(ik) => {
                self.eat();
                IntType(ik)
            }
            Bool => {
                self.expect(Bool);
                BoolType
            }
            Star => {
                self.expect(Star);
                PtrType(box self.parse_type())
            }
            LParen => {
                self.expect(LParen);
                let mut inner_types = parse_list!(self.parse_type()
                                                  until RParen);
                self.expect(RParen);
                if inner_types.len() == 0 {
                    UnitType
                } else if inner_types.len() == 1 {
                    inner_types.pop().unwrap().val
                } else {
                    TupleType(inner_types)
                }
            }
            IdentTok(_) => {
                let mut id = self.parse_ident();
                id.val.tps = match *self.peek() {
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
                FuncType(arglist, box self.parse_type())
            }
            _ => self.peek_error("Expected *, opening paren, a type name, or fn"),
        };

        let mut result;
        while {
            result = self.add_id_and_span(node, start_span.to(self.last_span));
            true
        } {
            node = match *self.peek() {
                LBracket => {
                    self.expect(LBracket);
                    let len = self.expect_number();
                    self.expect(RBracket);
                    ArrayType(box result, len)
                }
                _ => return result
            }
        }
        unreachable!()
    }

    pub fn parse_let_stmt(&mut self) -> Stmt {
        /* Parse a 'let' statement. There are a bunch of variations on this:
         * * `let x: int` to declare a typed variable, but not initialize it;
         * * `let x: int = 5` to declare a typed variable and initialize it;
         * * `let x = 5` to declare and initialize x, and infer the type;
         * * `let (x, y, z) = t` to deconstruct the tuple t into components.
         */
        let start_span = self.peek_span();
        self.expect(Let);

        let pat = self.parse_pat();

        let expr = match *self.peek() {
            Semicolon => {
                self.expect(Semicolon);
                None
            },
            Eq => {
                self.expect(Eq);
                let var_value = self.parse_expr();
                self.expect(Semicolon);
                Some(var_value)
            },
            _ => self.peek_error("Expected semicolon or \"=\""),
        };

        self.add_id_and_span(LetStmt(pat, expr), start_span.to(self.last_span))
    }

    fn parse_if_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        self.expect(If);

        let cond = self.parse_expr();
        let true_block = self.parse_block();

        let false_block = match *self.peek() {
            Else => {
                self.expect(Else);
                match *self.peek() {
                    If => {
                        Block {
                            items: vec!(),
                            stmts: vec!(),
                            expr: Some(self.parse_expr()),
                        }
                    },
                    _ => self.parse_block()
                }
            }
            _ => {
                let fake_span = mk_sp(self.last_span.get_end(), 0);

                Block {
                    items: vec!(),
                    stmts: vec!(),
                    expr: Some(self.add_id_and_span(UnitExpr, fake_span)),
                }
            }
        };

        self.add_id_and_span(IfExpr(box cond, box true_block, box false_block),
                         start_span.to(self.last_span))
    }

    fn parse_return_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        self.expect(Return);
        let result = ReturnExpr(box self.parse_expr());
        self.add_id_and_span(result, start_span.to(self.last_span))
    }

    fn parse_while_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        self.expect(While);
        let cond = self.parse_expr();
        let body = self.parse_block();
        self.add_id_and_span(WhileExpr(box cond, box body), start_span.to(self.last_span))
    }

    fn parse_for_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        self.expect(For);
        self.expect(LParen);
        let init = self.parse_expr();
        self.expect(Semicolon);
        let cond = self.parse_expr();
        self.expect(Semicolon);
        let iter = self.parse_expr();
        self.expect(RParen);
        let body = self.parse_block();
        self.add_id_and_span(ForExpr(box init, box cond, box iter, box body), start_span.to(self.last_span))
    }

    fn parse_match_arm(&mut self) -> MatchArm {
        let pat = self.parse_pat();
        self.expect(DoubleArrow);
        let body = self.parse_expr();

        MatchArm {
            pat:   pat,
            body:  body,
        }
    }

    fn parse_match_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        self.expect(Match);
        let matched_expr = self.parse_expr();
        self.expect(LBrace);
        let match_items = parse_list!(self.parse_match_arm() until RBrace);
        self.expect(RBrace);
        self.add_id_and_span(MatchExpr(box matched_expr, match_items), start_span.to(self.last_span))
    }

    fn parse_unop_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        let op = match *self.peek() {
            ref tok if unop_from_token(tok).is_some() =>
                Some(unop_from_token(tok).unwrap()),
            _ => None,
        };

        match op {
            Some(op) => {
                self.expect(unop_token(op));
                let op = self.add_id_and_span(op, self.last_span);
                let e = self.parse_simple_expr();
                let node = UnOpExpr(op, box e);
                self.add_id_and_span(node, start_span.to(self.last_span))
            }
            _ => self.parse_simple_expr()
        }
    }

    fn parse_unop_expr_maybe_cast(&mut self) -> Expr {
        fn maybe_parse_cast<'a, T: Buffer>(me: &mut StreamParser<'a, T>, expr: Expr, start_span: Span) -> Expr {
            match *me.peek() {
                As => {
                    me.expect(As);
                    let t = me.parse_type();
                    let node = CastExpr(box expr, t);
                    let castexpr = me.add_id_and_span(node, start_span.to(me.last_span));
                    maybe_parse_cast(me, castexpr, start_span)
                }
                _ => expr,
            }
        }

        let start_span = self.peek_span();
        let e = self.parse_unop_expr();
        maybe_parse_cast(self, e, start_span)
    }

    fn parse_binop_expr(&mut self) -> Expr {
        macro_rules! ops {
            () => (0);
            (,) => (0);
            ($($op:expr),+,) => (ops!($($ops),+));
            ($($op:expr),+) => ($((1 << ($op as uint)))|+);
        }

        macro_rules! row {
            ($a:expr, [$($ops:expr),+,]) => (row!($a, [$($ops),+]));
            ($a:expr, [$($ops:expr),*]) => (
                (OpTableRow { assoc: $a, ops: ops!($($ops),*) })
            )
        }

        macro_rules! left {
            ($($ops:expr),+,) => (left!($($ops),+));
            ($($ops:expr),*) => (row!(LeftAssoc, [$($ops),+]));
        }

        macro_rules! non {
            ($($ops:expr),+,) => (non!($($ops),+));
            ($($ops:expr),*) => (row!(NonAssoc, [$($ops),+]));
        }

        macro_rules! optable {
            ($($rows:expr),+,) => (optable!($($rows),+));
            ($($rows:expr),*) => (OpTable { rows: [$($rows),+] });
        }

        fn parse_binop_expr_from_optable<'a, T: Buffer>(parser: &mut StreamParser<'a, T>, table: &OpTable) -> Expr {
            fn maybe_parse_binop<'a, T: Buffer>(ops: uint,
                                                assoc: Assoc,
                                                parser: &mut StreamParser<'a, T>,
                                                parse_simpler_expr: |&mut StreamParser<'a, T>| -> Expr,
                                                e: Expr,
                                                start_span: Span)
                                             -> Expr {
                let op = match *parser.peek() {
                    ref tok if binop_from_token(tok).map_or(false, |op| ops & (1 << (op as uint)) != 0) => binop_from_token(tok).unwrap(),
                    _ => return e,
                };

                let op_span = parser.peek_span();
                parser.expect(binop_token(op));
                let op = parser.add_id_and_span(op, op_span);

                match assoc {
                    RightAssoc => {
                        let r_span = parser.peek_span();
                        let r = parse_simpler_expr(parser);
                        let r = maybe_parse_binop(ops, assoc, parser, parse_simpler_expr, r, r_span);
                        let node = BinOpExpr(op, box e, box r);
                        parser.add_id_and_span(node, start_span)
                    }
                    LeftAssoc => {
                        let r = parse_simpler_expr(parser);
                        let node = BinOpExpr(op, box e, box r);
                        let e = parser.add_id_and_span(node, start_span);
                        maybe_parse_binop(ops, assoc, parser, parse_simpler_expr, e, start_span)
                    }
                    NonAssoc => {
                        let r = parse_simpler_expr(parser);
                        let node = BinOpExpr(op, box e, box r);
                        parser.add_id_and_span(node, start_span)
                    }
                }
            }

            fn parse_row<'a, T: Buffer>(r: uint, parser: &mut StreamParser<'a, T>, rows: &[OpTableRow]) -> Expr {
                if r == 0 {
                    parser.parse_unop_expr_maybe_cast()
                } else {
                    let row = &rows[r - 1];
                    let start_span = parser.peek_span();
                    let parse_simpler_expr = |p: &mut StreamParser<'a, T>| parse_row(r - 1, p, rows);
                    let e = parse_simpler_expr(parser);
                    maybe_parse_binop(row.ops, row.assoc, parser, parse_simpler_expr, e, start_span)
                }
            }

            parse_row(table.rows.len(), parser, table.rows)
        }

        static optable: OpTable = optable! {
            left!(TimesOp, DivideOp, ModOp),
            left!(PlusOp, MinusOp),
            left!(LeftShiftOp, RightShiftOp),
            left!(BitAndOp),
            left!(BitXorOp),
            left!(BitOrOp),
            non!(GreaterOp, LessOp, GreaterEqOp, LessEqOp),
            left!(EqualsOp, NotEqualsOp),
            left!(AndAlsoOp),
            left!(OrElseOp),
        };

        parse_binop_expr_from_optable(self, &optable)
    }

    pub fn parse_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        let lv = self.parse_binop_expr();

        // Look for any assignments
        match *self.peek() {
            Eq => {
                self.expect(Eq);
                let e = self.parse_expr();
                let node = AssignExpr(box lv, box e);
                self.add_id_and_span(node, start_span.to(self.last_span))
            }
            _ => lv,
        }
    }

    fn parse_simple_expr(&mut self) -> Expr {
        /*
        Parse an expression.

        EXPR ::= Number | String | True | False
               |'(' EXPR [ , EXPR ...] ')'
               | BLOCK
               | EXPR '[' EXPR ']'
               | EXPR '(' ARGLIST ')'
               | EXPR '.' IDENT
               | EXPR 'as' TYPE
               | IDENT
        */
        use std::iter::Unfold;

        let start_span = self.peek_span();
        let mut expr = match *self.peek() {
            If => self.parse_if_expr(),
            Return => self.parse_return_expr(),
            Match => self.parse_match_expr(),
            For => self.parse_for_expr(),
            While => self.parse_while_expr(),
            LBrace => self.parse_block_expr(),
            LParen => self.parse_paren_expr(),
            NumberTok(..) | StringTok(..) | True | False => {
                let start_span = self.peek_span();
                let node = LitExpr(self.parse_lit());
                self.add_id_and_span(node, start_span.to(self.last_span))
            }
            IdentTok(..) => {
                let start_span = self.peek_span();
                let node = IdentExpr(self.parse_ident());
                self.add_id_and_span(node, start_span.to(self.last_span))
            }
            _ => self.peek_error("Expected expression")
        };

        loop {
            let node = match *self.peek() {
                Period => {
                    self.expect(Period);
                    let field = self.parse_name();
                    DotExpr(box expr, field)
                }
                Arrow => {
                    self.expect(Arrow);
                    let field = self.parse_name();
                    ArrowExpr(box expr, field)
                }
                LBracket => {
                    self.expect(LBracket);
                    let subscript = self.parse_expr();
                    self.expect(RBracket);
                    IndexExpr(box expr, box subscript)
                }
                LParen => {
                    self.expect(LParen);
                    let args = parse_list!(self.parse_expr() until RParen);
                    self.expect(RParen);
                    CallExpr(box expr, args)
                }
                _ => return expr
            };

            expr = self.add_id_and_span(node, start_span.to(self.last_span));
        }
    }

    fn parse_paren_expr(&mut self) -> Expr {
        let start_span = self.peek_span();

        self.expect(LParen);
        let mut inner_exprs = parse_list!(self.parse_expr()
                                          until RParen);
        self.expect(RParen);

        let node = if inner_exprs.len() == 0 {
            UnitExpr
        } else if inner_exprs.len() == 1 {
            GroupExpr(box inner_exprs.pop().unwrap())
        } else {
            TupleExpr(inner_exprs)
        };

        self.add_id_and_span(node, start_span.to(self.last_span))
    }

    fn parse_block_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        let block = self.parse_block();
        self.add_id_and_span(BlockExpr(box block), start_span.to(self.last_span))
    }

    fn parse_block(&mut self) -> Block {
        /* Parse a "block" (compound expression), such as
           `{ 1+1; f(x); 2 }`.
        */
        self.expect(LBrace);
        let mut statements = vec!();
        loop {
            match *self.peek() {
                Let => {
                    statements.push(self.parse_let_stmt());
                }
                RBrace => {
                    self.expect(RBrace);
                    return Block {
                        items: vec!(),
                        stmts: statements,
                        expr: None,
                    }
                }
                _ => {
                    let start_span = self.peek_span();
                    let expr = self.parse_expr();
                    match *self.peek() {
                        RBrace => {
                            self.expect(RBrace);
                            return Block {
                                items: vec!(),
                                stmts: statements,
                                expr: Some(expr),
                            }
                        }
                        Semicolon => {
                            self.expect(Semicolon);
                            statements.push(self.add_id_and_span(SemiStmt(expr), start_span.to(self.last_span)));
                        },
                        _ => {
                            statements.push(self.add_id_and_span(ExprStmt(expr), start_span.to(self.last_span)))
                        },
                    }
                }
            }
        }
    }

    fn parse_func_arg(&mut self) -> FuncArg {
        /* Parse a single argument as part of a function declaration.
           For example, in
           `let f(x: int, y: int) -> int { ... }`,
           this would parse "`x: int`" or "`y: int`".
        */
        let arg_id = self.parse_ident();
        self.expect(Colon);
        let arg_type = self.parse_type();

        FuncArg {
            ident: arg_id,
            argtype: arg_type,
        }
    }

    fn parse_func_item(&mut self) -> Item {
        let start_span = self.peek_span();
        self.expect(Fn);
        let funcname = self.parse_ident();
        let type_params = self.parse_type_params(LParen);
        self.expect(LParen);
        let args = parse_list!(self.parse_func_arg() until RParen);
        self.expect(RParen);
        let return_type = match *self.peek() {
            Arrow => {
                self.expect(Arrow);
                self.parse_type()
            }
            _ => {
                let dummy_span = mk_sp(self.last_span.get_end(), 0);
                self.add_id_and_span(UnitType, dummy_span)
            }
        };
        let body = self.parse_block();
        self.add_id_and_span(FuncItem(funcname, args, return_type, body, type_params),
                         start_span.to(self.last_span))
    }

    fn parse_type_params(&mut self, other_token: Token) -> Vec<Ident> {
        /* Parse type parameters to a function or struct declaration.
           This is the `<T>` in `let f<T>(x: *T)`, for example.

           `other_token` is the token that indicates that there are no
           type parameters. In the example above, it would be LParen.
        */
        match self.peek().clone() {
            Less => {
                self.expect(Less);
                let tps = parse_list!(self.parse_ident()
                                      until Greater);
                self.expect(Greater);
                Some(tps)
            },
            ref tok if *tok == other_token => None,
            _ => self.peek_error("Expected type parameters or argument list")
        }.unwrap_or(vec!())
    }

    fn parse_struct_field(&mut self) -> Field {
        let name = self.parse_name();
        self.expect(Colon);
        let field_type = self.parse_type();

        Field {
            name:    name,
            fldtype: field_type,
        }
    }

    fn parse_struct_item(&mut self) -> Item {
        let start_span = self.peek_span();
        self.expect(Struct);
        let structname = self.parse_ident();
        let type_params = self.parse_type_params(LBrace);
        self.expect(LBrace);
        let body = parse_list!(self.parse_struct_field() until RBrace);
        self.expect(RBrace);
        self.add_id_and_span(StructItem(structname, body, type_params), start_span.to(self.last_span))
    }

    fn parse_variant(&mut self) -> Variant {
        let ident = self.parse_ident();
        let types = match *self.peek() {
            LParen => {
                self.expect(LParen);
                let typelist = parse_list!(self.parse_type() until RParen);
                self.expect(RParen);
                typelist
            },
            _ => {
                vec!()
            }
        };

        Variant {
            ident: ident,
            args:  types,
        }
    }

    fn parse_enum_item(&mut self) -> Item {
        let start_span = self.peek_span();
        self.expect(Enum);
        let enumname = self.parse_ident();
        let type_params = self.parse_type_params(LBrace);
        self.expect(LBrace);
        let body = parse_list!(self.parse_variant() until RBrace);
        self.expect(RBrace);
        self.add_id_and_span(EnumItem(enumname, body, type_params), start_span.to(self.last_span))
    }

    pub fn parse_module(&mut self) -> Module {
        /* This is the highest level node of the AST. This function
         * is the one that will parse an entire file. */
        let mut items = vec!();
        loop {
            match *self.peek() {
                Fn => items.push(self.parse_func_item()),
                Struct => items.push(self.parse_struct_item()),
                Enum => items.push(self.parse_enum_item()),
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
    use super::Parser;
    use ast::Expr;

    #[test]
    fn test_basic_arith_expr() {
        let (_, tree) = ast_from_str(r#"1+3*5/2-2*3*(5+6)"#, |p| p.parse_expr());

        /* TODO handroll the new AST

        fn mknum(n: u64) -> Expr {
            Num(n, GenericInt)
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
        assert_eq!(format!("{}", tree).as_slice(),
                   "((1+((3*5)/2))-((2*3)*((5+6))))");
    }

    // These tests disabled until we have a pretty printer
    /*
    fn compare_canonicalized(raw: &str, parsed: &str) {
        let (_, tree) = ast_from_str(raw, |p| p.parse_let_stmt());
        assert_eq!(format!("{}", tree).as_slice(), parsed);
    }

    #[test]
    fn test_variable_declarations() {
        compare_canonicalized(
            r#"let x: fn(fn(int) -> int[4]) -> *(int[1]) = f(3*x+1, g(x)) * *p;"#,
            "let x: ([([int] -> (int)[4])] -> *((int)[1])) = (f([((3*x)+1), g([x])])*(*p));"
        );
    }

    #[test]
    fn test_variable_declarations_again() {
        compare_canonicalized(
            r#"let x: fn(int) -> fn(int[4]) -> (*int)[1] = f(3*x+1, g(x)) * *p;"#,
            "let x: ([int] -> ([(int)[4]] -> (*(int))[1])) = (f([((3*x)+1), g([x])])*(*p));"
        );
    }
    */
}
