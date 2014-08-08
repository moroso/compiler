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

use span::{SourcePos, Span, mk_sp};
use util::Name;

use super::session::Session;
use super::session::get_cur_rel_path;

use std::{io, mem, num, vec};
use std::collections::{HashMap, TreeMap, TreeSet};
use std::iter::Peekable;

use super::ast::*;
use super::lexer::*;

use values::*;

use FilePath = std::path::Path;

type FuncProto = (Ident, Vec<FuncArg>, Type, Vec<Ident>);
type StaticDecl = (Ident, Type);

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
    tokens: T,
    /// The next token in the stream
    next: Option<SourceToken<Token>>,
    /// The name of the current stream being parsed.
    name: Name,
    /// The span corresponding to the last token we consumed from the stream.
    last_span: Span,
    /// The session in which we're parsing this stream
    session: &'a mut Session,
    /// Any parsing restriction in the current context
    restriction: Restriction,
    /// Path to current source file
    source: FilePath,
}

#[deriving(PartialEq, Eq)]
enum Restriction {
    ExprStmtRestriction,
    NoAmbiguousLBraceRestriction,
    NoRestriction,
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

fn can_start_item(t: &Token) -> bool {
    match *t {
        Fn | Static | Extern |
        Enum | Struct | Mod |
        Macro | Const | Type
            => true,
        _   => false
    }
}

fn can_start_expr(t: &Token) -> bool {
    match *t {
        If | Return | Break | Continue |
        Match | For | While |
        True | False | Null |
        LBrace | LParen |
        ColonColon | IdentTok(..) |
        NumberTok(..) | StringTok(..) |
        IdentBangTok(..)
            => true,
        _   => false
    }
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
        SxbOp |
        SxhOp |
        Identity => fail!("Op {} should never come up in the language.",
                          op),
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
pub fn ast_from_str<U>(s: &str, f: |&mut StreamParser<Lexer<::std::io::BufferedReader<::std::io::MemReader>, Token>>| -> U) -> (Session, U) {
    let mut session = Session::new();
    let tree = Parser::parse_with(&mut session, lexer_from_str(s), f);
    (session, tree)
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

    /// Get all of the files used by this parse.
    pub fn get_all_filenames(&self) -> TreeSet<Name> {
        FromIterator::from_iter(self.filemap.iter().map(|(_,v)| *v))
    }

    fn new_id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id += 1;
        NodeId(id)
    }

    pub fn parse<T: Buffer>(session: &mut Session, lexer: Lexer<T, Token>) -> Module {
        Parser::parse_with(session, lexer, |p| p.parse_module())
    }

    pub fn parse_with<T: Buffer, U>(session: &mut Session,
                                    lexer: Lexer<T, Token>,
                                    f: |&mut StreamParser<Lexer<T, Token>>| -> U) -> U {
        let name = session.interner.intern(lexer.get_name());
        let mut tokp = StreamParser::new(session, name, lexer);
        f(&mut tokp)
    }

    pub fn parse_stream<T: Iterator<SourceToken<Token>>, U>(session: &mut Session,
                                                            name: Name,
                                                            tokens: T,
                                                            f: |&mut StreamParser<T>| -> U) -> U {
        let mut tokp = StreamParser::new(session, name, tokens);
        f(&mut tokp)
    }
}

impl OpTable {
    fn parse_expr<'a, T: Iterator<SourceToken<Token>>>(&self, parser: &mut StreamParser<'a, T>) -> Expr {
        fn parse_row<'a, T: Iterator<SourceToken<Token>>>(r: uint, rows: &[OpTableRow], parser: &mut StreamParser<'a, T>) -> Expr {
            if r == 0 {
                parser.parse_unop_expr_maybe_cast()
            } else {
                let row = &rows[r - 1];
                let start_span = parser.cur_span();
                let parse_simpler_expr = |p: &mut StreamParser<'a, T>| parse_row(r - 1, rows, p);
                let e = parse_simpler_expr(parser);
                parser.maybe_parse_binop(row.ops, row.assoc, parse_simpler_expr, e, start_span)
            }
        }

        parse_row(self.rows.len(), self.rows, parser)
    }
}

impl<'a, T: Iterator<SourceToken<Token>>> StreamParser<'a, T> {
    fn new(session: &'a mut Session, name: Name, tokens: T) -> StreamParser<'a, T> {
        StreamParser {
            name: name,
            next: None,
            tokens: tokens,
            session: session,
            last_span: mk_sp(SourcePos::new(), 0),
            restriction: NoRestriction,
            source: FilePath::new("."),
        }
    }

    /// Get the current cursor position as a zero-width span
    fn cur_span(&mut self) -> Span {
        let peek_begin = self.peek_span().get_begin();
        mk_sp(peek_begin, 0)
    }

    fn advance(&mut self) {
        self.next = self.tokens.next();

        match self.next {
            None => fail!("Tried to advance past EOF"),
            Some(ref x) =>
                self.name = self.session.interner.intern(x.filename.clone())
        }
    }

    /// "Peek" at the next token, returning the token, without consuming
    /// it from the stream.
    fn peek<'a>(&'a mut self) -> &'a Token {
        if self.next.is_none() {
            self.advance();
        }

        self.next.as_ref().map(|st| &st.tok).unwrap()
    }

    /// Peek at the Span of the next token.
    fn peek_span(&mut self) -> Span {
        if self.next.is_none() {
            self.advance();
        }

        self.next.as_ref().map(|st| st.sp).unwrap()
    }

    /// Consume the next token from the stream, returning it.
    fn eat(&mut self) -> Token {
        if self.next.is_none() {
            self.advance();
        }

        let st = self.next.take_unwrap();
        self.last_span = st.sp;
        st.tok
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
        let path = self.session.interner.name_to_str(&self.name);

        let s = format!("Parse error: {}\n    at {} {}\n",
                        message.as_slice(), path, pos);
        let _ = io::stderr().write_str(s.as_slice());
        fail!()
    }

    /// A convenience function to generate an error message when we've
    /// peeked at a token, but it doesn't match any token we were expecting.
    fn peek_error<'a, T: Str>(&mut self, message: T) -> ! {
        let tok = self.peek().clone();
        let pos = self.peek_span().get_begin();
        self.error(format!("{} (got token {})", message.as_slice(), tok), pos)
    }

    fn add_id_and_span<T>(&mut self, val: T, sp: Span) -> WithId<T> {
        let id = self.session.parser.new_id();
        self.session.parser.spanmap.insert(id, sp);
        self.session.parser.filemap.insert(id, self.name);
        WithId { val: val, id: id }
    }

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

    fn with_restriction<U>(&mut self, r: Restriction, p: |&mut StreamParser<'a, T>| -> U) -> U {
        let old = self.restriction;
        self.restriction = r;
        let ret = p(self);
        self.restriction = old;
        ret
    }

    /////////////////////////////////////////////////////////////////////
    // The actual parser functions begin here!
    // Most functions from this point on are for parsing a specific node
    // in the grammar.

    fn parse_name(&mut self) -> Name {
        match self.eat() {
            IdentTok(name) => self.session.interner.intern(name),
            tok => self.error(format!("Expected ident, found {}", tok),
                              self.last_span.get_begin())
        }
    }

    fn parse_ident(&mut self) -> Ident {
        let ident = IdentNode {
            name: self.parse_name(),
            tps: None,
        };

        let span = self.last_span;
        self.add_id_and_span(ident, span)
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

    fn parse_use(&mut self) -> Import {
        let start_span = self.cur_span();

        let path = self.parse_path_common(false, true);
        let import = match *self.peek() {
            Star => {
                self.expect(Star);
                ImportNode {
                    elems: path.val.elems,
                    global: path.val.global,
                    import: ImportAll
                }
            }
            LBrace => {
                self.expect(LBrace);
                let mut idents = vec!(self.parse_ident());

                while *self.peek() == Comma {
                    self.expect(Comma);
                    idents.push(self.parse_ident());
                }
                self.expect(RBrace);

                ImportNode {
                    elems: path.val.elems,
                    global: path.val.global,
                    import: ImportNames(idents)
                }
            }
            _ => {
                let mut v = path.val.elems;
                let last = v.pop().expect("path can't be empty");

                ImportNode {
                    elems: v,
                    global: path.val.global,
                    import: ImportNames(vec!(last))
                }
            }
        };


        let end_span = self.cur_span();
        self.add_id_and_span(import, start_span.to(end_span))
    }

    // If for_use is true, then we tolerate a { or a * appearing after
    // a ::.
    fn parse_path_common(&mut self, with_tps: bool, for_use: bool) -> Path {
        let start_span = self.cur_span();

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
            let start_span = self.cur_span();

            self.expect(ColonColon);
            match *self.peek() {
                Less if with_tps => {
                    let elem = path.elems.mut_last().unwrap();
                    let tps = self.parse_type_params();
                    elem.val.tps = Some(tps);
                }
                Star | LBrace if for_use => {
                    break;
                }
                _ => {
                    path.elems.push(self.parse_ident());
                }
            }

            let id = path.elems.last().unwrap().id;

            let end_span = self.cur_span();
            self.session.parser.spanmap.insert(id, start_span.to(end_span));
        }

        let end_span = self.cur_span();
        self.add_id_and_span(path, start_span.to(end_span))
    }

    fn parse_path(&mut self) -> Path {
        self.parse_path_common(true, false)
    }

    fn parse_path_no_tps(&mut self) -> Path {
        self.parse_path_common(false, false)
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
            Null                 => NullLit,
            StringTok(s)         => StringLit(s),
            NumberTok(num, kind) => NumLit(num, kind),
            tok                  => self.error(format!("Unexpected {} where literal expected", tok), self.last_span.get_begin())
        };

        let span = self.last_span;
        self.add_id_and_span(node, span)
    }

    fn parse_pat_common(&mut self, allow_types: bool) -> Pat {
        let start_span = self.cur_span();

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
                        let args = self.parse_list(|p| p.parse_pat_common(allow_types), RParen, true);
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
                            self.error(String::from_str("Expected ident, found path"), self.last_span.get_begin());
                        }
                        let mut elems = path.val.elems;
                        let ident = elems.pop().unwrap();
                        IdentPat(ident, maybe_type(self, allow_types))
                    }
                }
            }
            LParen => {
                self.expect(LParen);
                let args = self.parse_list(|p| p.parse_pat_common(allow_types), RParen, true);
                self.expect(RParen);
                TuplePat(args)
            }
            Underscore => {
                self.expect(Underscore);
                DiscardPat(maybe_type(self, allow_types))
            }
            _ => self.peek_error("Unexpected token while parsing pattern")
        };

        let end_span = self.cur_span();
        self.add_id_and_span(pat, start_span.to(end_span))
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
        let start_span = self.cur_span();
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
                let mut inner_types = self.parse_list(|p| p.parse_type(), RParen, true);
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
                let arglist = self.parse_list(|p| p.parse_type(), RParen, true);
                self.expect(RParen);
                self.expect(Arrow);
                FuncType(arglist, box self.parse_type())
            }
            _ => self.peek_error("Expected *, opening paren, a type name, or fn"),
        };

        let mut result;
        while {
            let end_span = self.cur_span();
            result = self.add_id_and_span(node, start_span.to(end_span));
            true
        } {
            node = match *self.peek() {
                LBracket => {
                    self.expect(LBracket);
                    let d = self.parse_expr();
                    self.expect(RBracket);
                    ArrayType(box result, box d)
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
        let start_span = self.cur_span();
        self.expect(Let);

        let pat = self.parse_pat();

        let expr = match *self.peek() {
            Semicolon => {
                self.expect(Semicolon);
                None
            },
            Eq => {
                self.expect(Eq);
                let var_value = self.with_restriction(NoRestriction, |p| p.parse_expr());
                self.expect(Semicolon);
                Some(var_value)
            },
            _ => self.peek_error("Expected semicolon or \"=\""),
        };

        let end_span = self.cur_span();
        self.add_id_and_span(LetStmt(pat, expr), start_span.to(end_span))
    }

    fn parse_if_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(If);

        let cond = self.parse_expr_no_structs();
        let true_block = self.parse_block();

        let false_block = match *self.peek() {
            Else => {
                self.expect(Else);
                match *self.peek() {
                    If => {
                        let start_span = self.cur_span();
                        let node = BlockNode {
                            items: vec!(),
                            stmts: vec!(),
                            expr: Some(self.parse_if_expr()),
                        };
                        let end_span = self.cur_span();
                        self.add_id_and_span(node, start_span.to(end_span))
                    },
                    _ => self.parse_block()
                }
            }
            _ => {
                let fake_span = self.cur_span();
                let node = BlockNode {
                    items: vec!(),
                    stmts: vec!(),
                    expr: Some(self.add_id_and_span(UnitExpr, fake_span)),
                };
                self.add_id_and_span(node, fake_span)
            }
        };

        let end_span = self.cur_span();
        self.add_id_and_span(IfExpr(box cond, box true_block, box false_block),
                             start_span.to(end_span))
    }

    fn parse_return_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Return);
        let result = ReturnExpr(box self.parse_expr());
        let end_span = self.cur_span();
        self.add_id_and_span(result, start_span.to(end_span))
    }

    fn parse_while_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(While);
        let cond = self.parse_expr_no_structs();
        let body = self.parse_block();
        let end_span = self.cur_span();
        self.add_id_and_span(WhileExpr(box cond, box body), start_span.to(end_span))
    }

    fn parse_for_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(For);
        self.expect(LParen);
        let this_span = self.cur_span();
        let init = match *self.peek() {
            Semicolon => self.add_id_and_span(UnitExpr, this_span),
            _ => self.parse_expr(),
        };
        self.expect(Semicolon);
        let this_span = self.cur_span();
        let cond = match *self.peek() {
            Semicolon => self.add_id_and_span(UnitExpr, this_span),
            _ => self.parse_expr(),
        };
        self.expect(Semicolon);
        let this_span = self.cur_span();
        let iter = match *self.peek() {
            RParen => self.add_id_and_span(UnitExpr, this_span),
            _ => self.parse_expr(),
        };
        self.expect(RParen);
        let body = self.parse_block();
        let end_span = self.cur_span();
        self.add_id_and_span(ForExpr(box init, box cond, box iter, box body), start_span.to(end_span))
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
        let start_span = self.cur_span();
        self.expect(Match);
        let matched_expr = self.parse_expr_no_structs();
        self.expect(LBrace);
        let match_items = self.parse_list(|p| p.parse_match_arm(), RBrace, true); // TODO don't require comma when there is a closing brace
        self.expect(RBrace);
        let end_span = self.cur_span();
        self.add_id_and_span(MatchExpr(box matched_expr, match_items), start_span.to(end_span))
    }

    fn parse_unop_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let op = match *self.peek() {
            ref tok if unop_from_token(tok).is_some() =>
                Some(unop_from_token(tok).unwrap()),
            _ => None,
        };

        match op {
            Some(op) => {
                self.expect(unop_token(op));
                let span = self.last_span;
                let op = self.add_id_and_span(op, span);
                let e = self.parse_simple_expr();
                let node = UnOpExpr(op, box e);
                let end_span = self.cur_span();
                self.add_id_and_span(node, start_span.to(end_span))
            }
            _ => self.parse_simple_expr()
        }
    }

    fn parse_unop_expr_maybe_cast(&mut self) -> Expr {
        fn maybe_parse_cast<'a, T: Iterator<SourceToken<Token>>>(p: &mut StreamParser<'a, T>, expr: Expr, start_span: Span) -> Expr {
            match *p.peek() {
                As => {
                    p.expect(As);
                    let t = p.parse_type();
                    let node = CastExpr(box expr, t);
                    let end_span = p.cur_span();
                    let castexpr = p.add_id_and_span(node, start_span.to(end_span));
                    maybe_parse_cast(p, castexpr, start_span)
                }
                _ => expr,
            }
        }

        let start_span = self.cur_span();
        let e = self.parse_unop_expr();
        maybe_parse_cast(self, e, start_span)
    }

    fn expr_is_complete(&mut self, e: &Expr) -> bool {
        // An expression is 'complete' if the expression:
        //   a) does not require a semicolon, and
        //   b) is a statement.
        match e.val {
              IfExpr(..)
            | ForExpr(..)
            | WhileExpr(..)
            | MatchExpr(..)
            | BlockExpr(..)
              => self.restriction == ExprStmtRestriction,
            _ => false,
        }
    }

    fn maybe_parse_binop(&mut self,
                         ops: uint,
                         assoc: Assoc,
                         parse_simpler_expr: |&mut StreamParser<'a, T>| -> Expr,
                         e: Expr,
                         start_span: Span)
                      -> Expr {
        if self.expr_is_complete(&e) {
            return e;
        }

        let op = match *self.peek() {
            ref tok if binop_from_token(tok).map_or(false, |op| ops & (1 << (op as uint)) != 0) => binop_from_token(tok).unwrap(),
            _ => return e,
        };

        self.expect(binop_token(op));
        let span = self.last_span;
        let op = self.add_id_and_span(op, span);

        match assoc {
            RightAssoc => {
                let r_span = self.cur_span();
                let r = parse_simpler_expr(self);
                let r = self.maybe_parse_binop(ops, assoc, parse_simpler_expr, r, r_span);
                let node = BinOpExpr(op, box e, box r);
                let end_span = self.cur_span();
                self.add_id_and_span(node, start_span.to(end_span))
            }
            LeftAssoc => {
                let r = parse_simpler_expr(self);
                let node = BinOpExpr(op, box e, box r);
                let e = self.add_id_and_span(node, start_span);
                self.maybe_parse_binop(ops, assoc, parse_simpler_expr, e, start_span)
            }
            NonAssoc => {
                let r = parse_simpler_expr(self);
                let node = BinOpExpr(op, box e, box r);
                let end_span = self.cur_span();
                self.add_id_and_span(node, start_span.to(end_span))
            }
        }
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

        optable.parse_expr(self)
    }

    fn parse_expr_no_structs(&mut self) -> Expr {
        self.with_restriction(NoAmbiguousLBraceRestriction, |p| p.parse_expr())
    }

    pub fn parse_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let lv = self.parse_binop_expr();

        let op = match *self.peek() {
            PlusEq    => Some(PlusOp),
            MinusEq   => Some(MinusOp),
            TimesEq   => Some(TimesOp),
            SlashEq   => Some(DivideOp),
            PipeEq    => Some(BitOrOp),
            CaretEq   => Some(BitXorOp),
            AmpEq     => Some(BitAndOp),
            LshEq     => Some(LeftShiftOp),
            RshEq     => Some(RightShiftOp),
            PercentEq => Some(ModOp),
            Eq        => None,
            _         => return lv,
        }.map(|op| self.add_id_and_span(op, self.peek_span()));

        self.eat();
        let e = self.parse_expr();
        let node = AssignExpr(op, box lv, box e);
        let end_span = self.cur_span();
        self.add_id_and_span(node, start_span.to(end_span))
    }

    fn parse_path_or_struct_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let path = self.parse_path();
        let node = match *self.peek() {
            LBrace if self.restriction != NoAmbiguousLBraceRestriction => {
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

        let end_span = self.cur_span();
        self.add_id_and_span(node, start_span.to(end_span))
    }

    fn parse_break_expr(&mut self) -> Expr {
        self.expect(Break);
        let span = self.last_span;
        self.add_id_and_span(BreakExpr, span)
    }

    fn parse_continue_expr(&mut self) -> Expr {
        self.expect(Continue);
        let span = self.last_span;
        self.add_id_and_span(ContinueExpr, span)
    }

    fn eat_token_tree(&mut self) -> Vec<Token> {
        let mut tokens = vec!();

        macro_rules! get_token_tree {
            ($l:expr, $r:expr) => ({
                tokens.push($l);
                while *self.peek() != $r {
                    tokens.push_all_move(self.eat_token_tree());
                }
                self.expect($r);
                tokens.push($r);
            })
        }

        match self.eat() {
            LParen   => get_token_tree!(LParen, RParen),
            LBrace   => get_token_tree!(LBrace, RBrace),
            LBracket => get_token_tree!(LBracket, RBracket),
            t => tokens.push(t),
        }

        tokens
    }

    fn eat_macro_token_tree(&mut self, args: &TreeSet<Name>) -> Vec<MacroToken> {
        let mut tokens = vec!();

        macro_rules! get_token_tree {
            ($l:expr, $r:expr) => ({
                tokens.push(MacroTok($l));
                while *self.peek() != $r {
                    tokens.push_all_move(self.eat_macro_token_tree(args));
                }
                self.expect($r);
                tokens.push(MacroTok($r));
            })
        }

        match self.eat() {
            LParen   => get_token_tree!(LParen, RParen),
            LBrace   => get_token_tree!(LBrace, RBrace),
            LBracket => get_token_tree!(LBracket, RBracket),
            Dollar => {
                let name = self.parse_name();
                if args.contains(&name) {
                    tokens.push(MacroVar(name));
                } else {
                    fail!("No such argument `${}`", name);
                }
            }
            t => tokens.push(MacroTok(t)),
        }

        tokens
    }

    fn parse_macro_expr_arg(&mut self) -> Vec<Token> {
        let mut tokens = vec!();
        while match *self.peek() { Comma | RParen => false, _ => true } {
            tokens.push_all_move(self.eat_token_tree())
        }

        tokens
    }

    fn parse_macro_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let name = match self.eat() {
            IdentBangTok(name) => self.session.interner.intern(name),
            _ => unreachable!(),
        };

        self.expect(LParen);
        let args = self.parse_list(|p| p.parse_macro_expr_arg(), RParen, true);
        self.expect(RParen);

        let end_span = self.cur_span();
        self.add_id_and_span(MacroExpr(name, args), start_span.to(end_span))
    }

    fn parse_sizeof_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Sizeof);
        self.expect(LParen);
        let ty = self.parse_type();
        self.expect(RParen);

        let end_span = self.cur_span();
        self.add_id_and_span(SizeofExpr(ty), start_span.to(end_span))
    }

    fn parse_simple_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let mut expr = match *self.peek() {
            If                        => self.parse_if_expr(),
            Return                    => self.parse_return_expr(),
            Break                     => self.parse_break_expr(),
            Continue                  => self.parse_continue_expr(),
            Match                     => self.parse_match_expr(),
            For                       => self.parse_for_expr(),
            While                     => self.parse_while_expr(),
            LBrace                    => self.parse_block_expr(),
            LParen                    => self.parse_paren_expr(),
            ColonColon | IdentTok(..) => self.parse_path_or_struct_expr(),
            IdentBangTok(..)          => self.parse_macro_expr(),
            NumberTok(..) | StringTok(..) | True | False | Null => {
                let start_span = self.cur_span();
                let node = LitExpr(self.parse_lit());
                let end_span = self.cur_span();
                self.add_id_and_span(node, start_span.to(end_span))
            },
            Sizeof                    => self.parse_sizeof_expr(),
            _ => self.peek_error("Expected expression"),
        };

        // While the next token cannot start an expression and expr is not complete
        while !(can_start_expr(self.peek()) && self.expr_is_complete(&expr)) {
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
                    let args = self.parse_list(|p| p.parse_expr(), RParen, true);
                    self.expect(RParen);
                    CallExpr(box expr, args)
                }
                _ => break,
            };

            let end_span = self.cur_span();
            expr = self.add_id_and_span(node, start_span.to(end_span));
        }

        expr
    }

    fn parse_paren_expr(&mut self) -> Expr {
        let start_span = self.cur_span();

        self.expect(LParen);
        let mut inner_exprs = self.with_restriction(NoRestriction, |p| p.parse_list(|p| p.parse_expr(), RParen, true));
        self.expect(RParen);

        let node = if inner_exprs.len() == 0 {
            UnitExpr
        } else if inner_exprs.len() == 1 {
            GroupExpr(box inner_exprs.pop().unwrap())
        } else {
            TupleExpr(inner_exprs)
        };

        let end_span = self.cur_span();
        self.add_id_and_span(node, start_span.to(end_span))
    }

    fn parse_block_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let block = self.parse_block();
        let end_span = self.cur_span();
        self.add_id_and_span(BlockExpr(box block), start_span.to(end_span))
    }

    fn parse_stmt(&mut self) -> Stmt {
        match *self.peek() {
            Let => self.parse_let_stmt(),
            _ => {
                let start_span = self.cur_span();
                let expr = self.with_restriction(ExprStmtRestriction, |p| p.parse_expr());
                match *self.peek() {
                    Semicolon => {
                        self.expect(Semicolon);
                        let end_span = self.cur_span();
                        self.add_id_and_span(SemiStmt(expr), start_span.to(end_span))
                    },
                    _ => {
                        let end_span = self.cur_span();
                        self.add_id_and_span(ExprStmt(expr), start_span.to(end_span))
                    },
                }
            }
        }
    }

    fn parse_block(&mut self) -> Block {
        /* Parse a "block" (compound expression), such as
           `{ 1+1; f(x); 2 }`.
        */
        let start_span = self.cur_span();
        self.expect(LBrace);
        let mut statements = vec!();
        let items = self.parse_items_until(RBrace, |me| statements.push(me.parse_stmt()));
        self.expect(RBrace);
        let end_span = self.cur_span();

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

        let node = BlockNode {
            items: items,
            stmts: statements,
            expr: expr,
        };
        self.add_id_and_span(node, start_span.to(end_span))
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

    fn parse_extern_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Extern);
        match *self.peek() {
            Fn => {
                let (funcname, args, return_type, type_params) = self.parse_func_prototype();
                self.expect(Semicolon);
                let end_span = self.cur_span();
                self.add_id_and_span(FuncItem(funcname, args, return_type, None, type_params),
                                     start_span.to(end_span))
            }
            Static => {
                let (name, ty) = self.parse_static_decl();
                self.expect(Semicolon);
                let end_span = self.cur_span();
                self.add_id_and_span(StaticItem(name, ty, None, true),
                                     start_span.to(end_span))
            }
            _ => self.peek_error("Expected 'fn' or 'static'"),
        }
    }

    fn parse_func_prototype(&mut self) -> FuncProto {
        self.expect(Fn);
        let funcname = self.parse_ident();
        let type_params = self.parse_item_type_params(LParen);
        self.expect(LParen);
        let args = self.parse_list(|p| p.parse_func_arg(), RParen, true);
        self.expect(RParen);
        let return_type = match *self.peek() {
            Arrow => {
                self.expect(Arrow);
                match *self.peek() {
                    Bang => {
                        self.expect(Bang);
                        let span = self.last_span;
                        self.add_id_and_span(DivergingType, span)
                    }
                    _ => self.parse_type(),
                }
            }
            _ => {
                let dummy_span = self.cur_span();
                self.add_id_and_span(UnitType, dummy_span)
            }
        };

        (funcname, args, return_type, type_params)
    }

    fn parse_func_item(&mut self) -> Item {
        let start_span = self.cur_span();
        let (funcname, args, return_type, type_params) = self.parse_func_prototype();
        let body = Some(self.parse_block());
        let end_span = self.cur_span();
        self.add_id_and_span(FuncItem(funcname, args, return_type, body, type_params),
                             start_span.to(end_span))
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
        let start_span = self.cur_span();
        self.expect(Struct);
        let structname = self.parse_ident();
        let type_params = self.parse_item_type_params(LBrace);
        self.expect(LBrace);
        let body = self.parse_list(|p| p.parse_struct_field(), RBrace, true);
        self.expect(RBrace);
        let end_span = self.cur_span();
        self.add_id_and_span(StructItem(structname, body, type_params), start_span.to(end_span))
    }

    fn parse_variant(&mut self) -> Variant {
        let ident = self.parse_ident();
        let types = match *self.peek() {
            LParen => {
                self.expect(LParen);
                let typelist = self.parse_list(|p| p.parse_type(), RParen, true);
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
        let start_span = self.cur_span();
        self.expect(Enum);
        let enumname = self.parse_ident();
        let type_params = self.parse_item_type_params(LBrace);
        self.expect(LBrace);
        let body = self.parse_list(|p| p.parse_variant(), RBrace, true);
        self.expect(RBrace);
        let end_span = self.cur_span();
        self.add_id_and_span(EnumItem(enumname, body, type_params), start_span.to(end_span))
    }

    fn parse_type_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Type);
        let typename = self.parse_ident();
        let type_params = self.parse_item_type_params(Eq);
        self.expect(Eq);
        let typedef = self.parse_type();
        self.expect(Semicolon);
        let end_span = self.cur_span();
        self.add_id_and_span(TypeItem(typename, typedef, type_params), start_span.to(end_span))
    }

    fn parse_use_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Use);
        let mut path = self.parse_use();
        self.expect(Semicolon);

        // 'use' items are always globally-scoped
        path.val.global = true;

        let end_span = self.cur_span();
        self.add_id_and_span(UseItem(path), start_span.to(end_span))
    }

    fn parse_items_until<U>(&mut self, end: Token, unmatched: |&mut StreamParser<'a, T>| -> U) -> Vec<Item> {
        let mut items = vec!();
        let mut use_items = vec!();
        let mut count = 0;
        while *self.peek() != end {
            match *self.peek() {
                Use => {
                    if count > use_items.len() {
                        let spot = self.cur_span().get_begin();
                        self.error("'use' declarations must come before everything else", spot)
                    } else {
                        use_items.push(self.parse_use_item())
                    }
                }
                _ => {
                    if can_start_item(self.peek()) {
                        items.push(self.parse_item())
                    } else {
                        unmatched(self);
                    }
                }
            }

            count += 1;
        }

        // we don't have unshift_all_move :(
        { use_items.push_all_move(items); use_items }
    }

    fn parse_module_until(&mut self, end: Token) -> Module {
        let start_span = self.cur_span();
        let items = self.parse_items_until(end, |me| me.parse_item());
        let node = ModuleNode { items: items };
        let end_span = self.cur_span();
        self.add_id_and_span(node, start_span.to(end_span))
    }

    fn parse_mod_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Mod);
        let ident = self.parse_ident();
        let module = match *self.peek() {
            LBrace => {
                self.expect(LBrace);
                let module = self.parse_module_until(RBrace);
                self.expect(RBrace);
                module
            }
            Semicolon => {
                self.expect(Semicolon);
                let file = {
                    let name = self.session.interner.name_to_str(&ident.val.name);

                    let base = get_cur_rel_path();
                    let filename1 = base.join(FilePath::new(format!("{}.mb", name)));
                    let filename2 = base.join(FilePath::new(format!("{}/mod.mb", name)));

                    let filename = match (filename1.exists(), filename2.exists()) {
                        (true,  true)  => self.error(format!("ambiguous module name: both {} and {} exist.",
                                                             filename1.display(), filename2.display()),
                                                     start_span.get_begin()),
                        (false, false) => self.error(format!("no such module: neither {} nor {} exist.",
                                                             filename1.display(), filename2.display()),
                                                     start_span.get_begin()),
                        (true,  false) => filename1,
                        (false, true)  => filename2,
                    };

                    ::std::io::File::open(&filename).unwrap_or_else(|e| {
                        self.error(format!("failed to open {}: {}", filename.display(), e), start_span.get_begin())
                    })
                };

                self.session.parse_file(file)
            }
            _ => self.peek_error("Expected opening brace or semicolon"),
        };
        let end_span = self.cur_span();
        self.add_id_and_span(ModItem(ident, module), start_span.to(end_span))
    }

    fn parse_static_decl(&mut self) -> StaticDecl {
        self.expect(Static);
        let name = self.parse_ident();
        self.expect(Colon);
        let ty = self.parse_type();
        (name, ty)
    }

    fn parse_static_item(&mut self) -> Item {
        let start_span = self.cur_span();
        let (name, ty) = self.parse_static_decl();
        let expr = match *self.peek() {
            Eq => {
                self.expect(Eq);
                Some(self.parse_expr())
            },
            _ => None,
        };
        self.expect(Semicolon);

        let end_span = self.cur_span();
        self.add_id_and_span(StaticItem(name, ty, expr, false),
                             start_span.to(end_span))
    }

    fn parse_macro_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Macro);
        let name = match self.eat() {
            IdentBangTok(name) => self.session.interner.intern(name),
            tok => self.error(format!("Expected macro identifier, found {}", tok),
                              self.last_span.get_begin())
        };

        self.expect(LParen);

        let args = self.parse_list(|me| me.parse_name(), RParen, true);

        let mut args_map = TreeSet::new();
        for arg in args.iter() {
            args_map.insert(*arg);
        }

        self.expect(RParen);
        self.expect(LBrace);

        let mut body = vec!();
        while *self.peek() != RBrace {
            body.push_all_move(self.eat_macro_token_tree(&args_map));
        }

        self.expect(RBrace);
        let end_span = self.cur_span();

        let def = MacroDef {
            name: name,
            args: args,
            body: body,
        };

        self.add_id_and_span(MacroDefItem(def), start_span.to(end_span))
    }

    fn parse_const_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Const);
        let name = self.parse_ident();
        self.expect(Colon);
        let ty = self.parse_type();
        self.expect(Eq);
        let expr = self.parse_expr();
        self.expect(Semicolon);
        let end_span = self.cur_span();
        self.add_id_and_span(ConstItem(name, ty, expr), start_span.to(end_span))
    }

    fn parse_item(&mut self) -> Item {
        match *self.peek() {
            Fn => self.parse_func_item(),
            Struct => self.parse_struct_item(),
            Enum => self.parse_enum_item(),
            Type => self.parse_type_item(),
            Mod => self.parse_mod_item(),
            Static => self.parse_static_item(),
            Extern => self.parse_extern_item(),
            Macro => self.parse_macro_item(),
            Const => self.parse_const_item(),
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
    use super::Parser;
    use super::super::ast::Expr;

    use super::*;

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
