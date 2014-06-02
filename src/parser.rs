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

    /// Utility to parse a comma-separated list of things
    fn parse_list<U>(&mut self, p: |&mut StreamParser<'a, T>| -> U, end: Token, allow_trailing_comma: bool) -> Vec<U> {
        if *self.peek() == end {
            return vec!();
        }

        let mut res = vec!(p(self));
        while *self.peek() != end {
            match *self.peek() {
                Comma => {
                    self.expect(Comma);
                    if !allow_trailing_comma || *self.peek() != end {
                        res.push(p(self))
                    }
                }
                _ => {
                    self.peek_error(format!("Expected comma or {}", end))
                }
            }
        }

        res
    }

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

    fn parse_type_params(&mut self) -> Vec<Type> {
        self.expect(Less);
        let ps = self.parse_list(|p| p.parse_type(), Greater, false);
        self.expect(Greater);
        ps
    }

    fn parse_item_type_params(&mut self, other_token: Token) -> Vec<Ident> {
        if *self.peek() == other_token {
            return vec!();
        }

        match *self.peek() {
            Less => {
                self.expect(Less);
                let tps = self.parse_list(|p| p.parse_ident(), Greater, false);
                self.expect(Greater);
                tps
            },
            _ => self.peek_error("Expected type parameters or argument list")
        }
    }

    fn parse_path_common(&mut self, with_tps: bool) -> Path {
        let start_span = self.peek_span();

        let global = match *self.peek() {
            ColonColon => {
                self.expect(ColonColon);
                true
            }
            _ => false,
        };

        let mut path = PathNode {
            global: global,
            elems: vec!(self.parse_ident()),
        };

        while *self.peek() == ColonColon {
            let start_span = self.peek_span();

            self.expect(ColonColon);
            match *self.peek() {
                Less if with_tps => {
                    let elem = path.elems.mut_last().unwrap();
                    let tps = self.parse_type_params();
                    elem.val.tps = Some(tps);
                }
                _ => {
                    path.elems.push(self.parse_ident());
                }
            }

            let id = path.elems.last().unwrap().id;
            self.parser.spanmap.insert(id, start_span.to(self.last_span));
        }

        self.add_id_and_span(path, start_span.to(self.last_span))
    }

    fn parse_path(&mut self) -> Path {
        self.parse_path_common(true)
    }

    fn parse_path_no_tps(&mut self) -> Path {
        self.parse_path_common(false)
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

        let maybe_type = |p: &mut StreamParser<'a, T>, allow_types| {
            match *p.peek() {
                Colon if allow_types => {
                    p.expect(Colon);
                    let t = p.parse_type();
                    Some(t)
                }
                _ => None
            }
        };

        let pat = match *self.peek() {
            ColonColon | IdentTok(..) => {
                let path = self.parse_path();
                match *self.peek() {
                    LParen => {
                        self.expect(LParen);
                        let args = self.parse_list(|p| p.parse_pat_common(allow_types), RParen, false);
                        self.expect(RParen);
                        VariantPat(path, args)
                    }
                    DoubleArrow => {
                        // Empty variant.
                        VariantPat(path, vec!())
                    }
                    LBrace => {
                        self.expect(LBrace);
                        let field_pats = self.parse_list(|p| p.parse_field_pat(), RBrace, true);
                        self.expect(RBrace);
                        StructPat(path, field_pats)
                    }
                    _ => {
                        if path.val.global || path.val.elems.len() != 1 {
                            self.error(format!("Expected ident, found path"), self.last_span.get_begin());
                        }
                        let mut elems = path.val.elems;
                        let ident = elems.pop().unwrap();
                        IdentPat(ident, maybe_type(self, allow_types))
                    }
                }
            }
            LParen => {
                self.expect(LParen);
                let args = self.parse_list(|p| p.parse_pat_common(allow_types), RParen, false);
                self.expect(RParen);
                TuplePat(args)
            }
            Underscore => {
                self.expect(Underscore);
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
                let mut inner_types = self.parse_list(|p| p.parse_type(), RParen, false);
                self.expect(RParen);
                if inner_types.len() == 0 {
                    UnitType
                } else if inner_types.len() == 1 {
                    inner_types.pop().unwrap().val
                } else {
                    TupleType(inner_types)
                }
            }
            ColonColon | IdentTok(..) => {
                let mut path = self.parse_path_no_tps();
                match *self.peek() {
                    Less => {
                        let elem = path.val.elems.mut_last().unwrap();
                        elem.val.tps = Some(self.parse_type_params());
                    }
                    _ => {}
                }
                NamedType(path)
            }
            Fn => {
                self.expect(Fn);
                self.expect(LParen);
                let arglist = self.parse_list(|p| p.parse_type(), RParen, false);
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

        let cond = self.parse_expr_no_structs();
        let true_block = self.parse_block();

        let false_block = match *self.peek() {
            Else => {
                self.expect(Else);
                match *self.peek() {
                    If => {
                        Block {
                            items: vec!(),
                            stmts: vec!(),
                            expr: Some(self.parse_if_expr()),
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
        let cond = self.parse_expr_no_structs();
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
        let matched_expr = self.parse_expr_no_structs();
        self.expect(LBrace);
        let match_items = self.parse_list(|p| p.parse_match_arm(), RBrace, true); // TODO don't require comma when there is a closing brace
        self.expect(RBrace);
        self.add_id_and_span(MatchExpr(box matched_expr, match_items), start_span.to(self.last_span))
    }

    fn parse_unop_expr(&mut self, allow_structs: bool) -> Expr {
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
                let e = self.parse_simple_expr(allow_structs);
                let node = UnOpExpr(op, box e);
                self.add_id_and_span(node, start_span.to(self.last_span))
            }
            _ => self.parse_simple_expr(allow_structs)
        }
    }

    fn parse_unop_expr_maybe_cast(&mut self, allow_structs: bool) -> Expr {
        fn maybe_parse_cast<'a, T: Buffer>(p: &mut StreamParser<'a, T>, expr: Expr, start_span: Span) -> Expr {
            match *p.peek() {
                As => {
                    p.expect(As);
                    let t = p.parse_type();
                    let node = CastExpr(box expr, t);
                    let castexpr = p.add_id_and_span(node, start_span.to(p.last_span));
                    maybe_parse_cast(p, castexpr, start_span)
                }
                _ => expr,
            }
        }

        let start_span = self.peek_span();
        let e = self.parse_unop_expr(allow_structs);
        maybe_parse_cast(self, e, start_span)
    }

    fn parse_binop_expr(&mut self, allow_structs: bool) -> Expr {
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

        fn parse_binop_expr_from_optable<'a, T: Buffer>(parser: &mut StreamParser<'a, T>, table: &OpTable, allow_structs: bool) -> Expr {
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

            fn parse_row<'a, T: Buffer>(r: uint, parser: &mut StreamParser<'a, T>, rows: &[OpTableRow], allow_structs: bool) -> Expr {
                if r == 0 {
                    parser.parse_unop_expr_maybe_cast(allow_structs)
                } else {
                    let row = &rows[r - 1];
                    let start_span = parser.peek_span();
                    let parse_simpler_expr = |p: &mut StreamParser<'a, T>| parse_row(r - 1, p, rows, allow_structs);
                    let e = parse_simpler_expr(parser);
                    maybe_parse_binop(row.ops, row.assoc, parser, parse_simpler_expr, e, start_span)
                }
            }

            parse_row(table.rows.len(), parser, table.rows, allow_structs)
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

        parse_binop_expr_from_optable(self, &optable, allow_structs)
    }

    pub fn parse_expr(&mut self) -> Expr {
        self.parse_expr_common(true)
    }

    fn parse_expr_no_structs(&mut self) -> Expr {
        self.parse_expr_common(false)
    }

    fn parse_expr_common(&mut self, allow_structs: bool) -> Expr {
        let start_span = self.peek_span();
        let lv = self.parse_binop_expr(allow_structs);

        macro_rules! assignments(
            ( $( $tok:ident => $op:ident ),* ) => (
            match *self.peek() {
                Eq => {
                    self.expect(Eq);
                    let e = self.parse_expr_common(allow_structs);
                    let node = AssignExpr(box lv, box e);
                    self.add_id_and_span(node, start_span.to(self.last_span))
                },
                $(
                    $tok => {
                        self.expect($tok);
                        let op_span = self.last_span;
                        let e = self.parse_expr_common(allow_structs);
                        let node = AssignOpExpr(
                            self.add_id_and_span($op, op_span),
                            box lv, box e);
                        self.add_id_and_span(node, start_span.to(self.last_span))
                    },
                    )*
                    _ => lv,
            }
            )
                                 )

        assignments!(PlusEq => PlusOp)
    }

    fn parse_path_or_struct_expr(&mut self) -> Expr {
        let start_span = self.peek_span();
        let path = self.parse_path();
        let node = match *self.peek() {
            LBrace => {
                self.expect(LBrace);
                let fields = self.parse_list(|p| {
                    let name = p.parse_name();
                    p.expect(Colon);
                    let expr = p.parse_expr();
                    (name, expr)
                }, RBrace, true);
                self.expect(RBrace);
                StructExpr(path, fields)
            }
            _ => PathExpr(path),
        };

        self.add_id_and_span(node, start_span.to(self.last_span))
    }

    fn parse_simple_expr(&mut self, allow_structs: bool) -> Expr {
        let start_span = self.peek_span();
        let mut expr = match *self.peek() {
            If => self.parse_if_expr(),
            Return => self.parse_return_expr(),
            Match => self.parse_match_expr(),
            For => self.parse_for_expr(),
            While => self.parse_while_expr(),
            LBrace => self.parse_block_expr(),
            LParen => self.parse_paren_expr(),
            ColonColon | IdentTok(..) => {
                if allow_structs {
                    self.parse_path_or_struct_expr()
                } else {
                    let path = self.parse_path();
                    self.add_id_and_span(PathExpr(path), start_span.to(self.last_span))
                }
            }
            NumberTok(..) | StringTok(..) | True | False => {
                let start_span = self.peek_span();
                let node = LitExpr(self.parse_lit());
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
                    let args = self.parse_list(|p| p.parse_expr(), RParen, false);
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
        let mut inner_exprs = self.parse_list(|p| p.parse_expr(), RParen, false);
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

    fn parse_stmt(&mut self) -> Stmt {
        match *self.peek() {
            Let => self.parse_let_stmt(),
            _ => {
                let start_span = self.peek_span();
                let expr = self.parse_expr();
                match *self.peek() {
                    Semicolon => {
                        self.expect(Semicolon);
                        self.add_id_and_span(SemiStmt(expr), start_span.to(self.last_span))
                    },
                    _ => {
                        self.add_id_and_span(ExprStmt(expr), start_span.to(self.last_span))
                    },
                }
            }
        }
    }

    fn parse_block(&mut self) -> Block {
        /* Parse a "block" (compound expression), such as
           `{ 1+1; f(x); 2 }`.
        */
        self.expect(LBrace);
        let mut statements = vec!();
        let mut items = vec!();
        loop {
            match *self.peek() {
                RBrace => break,
                Fn | Struct | Enum | Mod => items.push(self.parse_item()),
                _ => statements.push(self.parse_stmt()),
            }
        }

        self.expect(RBrace);

        let expr = statements.pop().and_then(|stmt| {
            match stmt.val {
                ExprStmt(expr) => {
                    Some(expr)
                }
                _ => {
                    statements.push(stmt);
                    None
                },
            }
        });

        return Block {
            items: items,
            stmts: statements,
            expr: expr,
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
        let type_params = self.parse_item_type_params(LParen);
        self.expect(LParen);
        let args = self.parse_list(|p| p.parse_func_arg(), RParen, false);
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
        let type_params = self.parse_item_type_params(LBrace);
        self.expect(LBrace);
        let body = self.parse_list(|p| p.parse_struct_field(), RBrace, true);
        self.expect(RBrace);
        self.add_id_and_span(StructItem(structname, body, type_params), start_span.to(self.last_span))
    }

    fn parse_variant(&mut self) -> Variant {
        let ident = self.parse_ident();
        let types = match *self.peek() {
            LParen => {
                self.expect(LParen);
                let typelist = self.parse_list(|p| p.parse_type(), RParen, false);
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
        let type_params = self.parse_item_type_params(LBrace);
        self.expect(LBrace);
        let body = self.parse_list(|p| p.parse_variant(), RBrace, true);
        self.expect(RBrace);
        self.add_id_and_span(EnumItem(enumname, body, type_params), start_span.to(self.last_span))
    }

    fn parse_module_until(&mut self, end: Token) -> Module {
        let start_span = self.peek_span();
        let mut items = vec!();
        while *self.peek() != end {
            items.push(self.parse_item());
        }
        let node = ModuleNode { items: items };
        let end_span = self.peek_span();
        self.add_id_and_span(node, start_span.to(end_span))
    }

    fn parse_mod_item(&mut self) -> Item {
        let start_span = self.peek_span();
        self.expect(Mod);
        let name = self.parse_ident();
        self.expect(LBrace);
        let module = self.parse_module_until(RBrace);
        self.expect(RBrace);
        self.add_id_and_span(ModItem(name, module), start_span.to(self.last_span))
    }

    fn parse_item(&mut self) -> Item {
        match *self.peek() {
            Fn => self.parse_func_item(),
            Struct => self.parse_struct_item(),
            Enum => self.parse_enum_item(),
            Mod => self.parse_mod_item(),
            _ => self.peek_error("Expected an item definition (fn, struct, enum, mod)"),
        }
    }

    pub fn parse_module(&mut self) -> Module {
        /* This is the highest level node of the AST. This function
         * is the one that will parse an entire file. */
        let module = self.parse_module_until(Eof);
        self.expect(Eof);
        module
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
