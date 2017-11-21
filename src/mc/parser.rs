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

use super::session::{Session, Options};
use super::session::get_cur_rel_path;

use std::io;
use std::collections::{BTreeMap, BTreeSet};
use std::iter::FromIterator;

use mas::ast::{InstNode, InstPacket};
use mas::lexer::asm_lexer_from_str;
use mas::parser::AsmParser;

use super::ast;
use super::ast::*;
use super::lexer::*;

use std::path::PathBuf;
use std::path::Path as FilePath;

type FuncProto = (Ident, Vec<FuncArg>, ast::Type, Vec<Ident>);
type StaticDecl = (Ident, ast::Type);

/// Context associated with a full parsing session
pub struct Parser {
    /// Each AST node is given a unique identifier. This keeps track of the
    /// next number to assign to an identifier.
    next_id: usize,
    /// Tracks the corresponding source position of each AST node.
    pub spanmap: BTreeMap<NodeId, Span>,
    /// Tracks the corresponding file name of each AST node.
    pub filemap: BTreeMap<NodeId, Name>,
}

/// The state for parsing a stream of tokens into an AST node
pub struct StreamParser<'a, 'b: 'a, T: Iterator<Item=SourceToken<Token>>> {
    /// The token stream.
    tokens: T,
    /// The next token in the stream
    next: Option<SourceToken<Token>>,
    /// The name of the current stream being parsed.
    name: Name,
    /// The span corresponding to the last token we consumed from the stream.
    last_span: Span,
    /// The session in which we're parsing this stream
    session: &'a mut Session<'b>,
    /// Any parsing restriction in the current context
    restriction: Restriction,
    /// Path to current source file
    source: PathBuf,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Restriction {
    ExprStmtRestriction,
    NoAmbiguousLBraceRestriction,
    NoRestriction,
}

#[derive(Clone, Copy)]
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
    ops: usize,
}

fn can_start_item(t: &Token) -> bool {
    match *t {
        Token::Fn | Token::Static | Token::Extern |
        Token::Enum | Token::Struct | Token::Mod |
        Token::Macro | Token::Const | Token::Type
            => true,
        _   => false
    }
}

fn can_start_expr(t: &Token) -> bool {
    match *t {
        Token::If | Token::Return | Token::Break | Token::Continue |
        Token::Match | Token::For | Token::While | Token::Do |
        Token::True | Token::False | Token::Null |
        Token::LBrace | Token::LParen |
        Token::ColonColon | Token::IdentTok(..) |
        Token::NumberTok(..) | Token::StringTok(..) |
        Token::IdentBangTok(..)
            => true,
        _   => false
    }
}

fn unop_from_token(t: &Token) -> Option<UnOpNode> {
    match *t {
        Token::Dash => Some(Negate),
        Token::Tilde => Some(BitNot),
        Token::Bang => Some(LogNot),
        Token::Ampersand => Some(AddrOf),
        Token::Star => Some(Deref),
        _ => None,
    }
}

fn binop_from_token(t: &Token) -> Option<BinOpNode> {
    match *t {
        Token::Plus => Some(PlusOp),
        Token::Dash => Some(MinusOp),
        Token::Star => Some(TimesOp),
        Token::ForwardSlash => Some(DivideOp),
        Token::Percent => Some(ModOp),
        Token::EqEq => Some(EqualsOp),
        Token::BangEq => Some(NotEqualsOp),
        Token::Less => Some(LessOp),
        Token::LessEq => Some(LessEqOp),
        Token::Greater => Some(GreaterOp),
        Token::GreaterEq => Some(GreaterEqOp),
        Token::AmpAmp => Some(AndAlsoOp),
        Token::PipePipe => Some(OrElseOp),
        Token::Ampersand => Some(BitAndOp),
        Token::Pipe => Some(BitOrOp),
        Token::Caret => Some(BitXorOp),
        Token::Lsh => Some(LeftShiftOp),
        Token::Rsh => Some(RightShiftOp),
        _ => None,
    }
}

fn unop_token(op: UnOpNode) -> Token {
    match op {
        Deref => Token::Star,
        AddrOf => Token::Ampersand,
        Negate => Token::Dash,
        LogNot => Token::Bang,
        BitNot => Token::Tilde,
        SxbOp |
        SxhOp |
        Identity => panic!("Op {} should never come up in the language.",
                          op),
    }
}

fn binop_token(op: BinOpNode) -> Token {
    match op {
        PlusOp => Token::Plus,
        MinusOp => Token::Dash,
        TimesOp => Token::Star,
        DivideOp => Token::ForwardSlash,
        ModOp => Token::Percent,
        EqualsOp => Token::EqEq,
        NotEqualsOp => Token::BangEq,
        LessOp => Token::Less,
        LessEqOp => Token::LessEq,
        GreaterOp => Token::Greater,
        GreaterEqOp => Token::GreaterEq,
        AndAlsoOp => Token::AmpAmp,
        OrElseOp => Token::PipePipe,
        BitAndOp => Token::Ampersand,
        BitOrOp => Token::Pipe,
        BitXorOp => Token::Caret,
        LeftShiftOp => Token::Lsh,
        RightShiftOp => Token::Rsh,
    }
}

// Convenience function for testing
pub fn ast_from_str<'a, U, F>(s: &str, f: F) -> (Session<'a>, U)
    where F: Fn(&mut StreamParser<Lexer<::std::io::BufReader<&[u8]>, Token>>) -> U {
    let mut session = Session::new(Options::new());
    let tree = Parser::parse_with(&mut session, lexer_from_str(s), f);
    (session, tree)
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            next_id: 0,
            spanmap: BTreeMap::new(),
            filemap: BTreeMap::new(),
        }
    }

    /// Get the Span of a certain node in the AST.
    pub fn span_of(&self, id: &NodeId) -> Span {
        *self.spanmap.get(id).unwrap()
    }

    /// Get the name of a certain node in the AST.
    pub fn filename_of(&self, id: &NodeId) -> Name {
        *self.filemap.get(id).unwrap()
    }

    /// Get all of the files used by this parse.
    pub fn get_all_filenames(&self) -> BTreeSet<Name> {
        FromIterator::from_iter(self.filemap.iter().map(|(_,v)| *v))
    }

    fn new_id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id += 1;
        NodeId(id)
    }

    pub fn parse<T: BufReader>(session: &mut Session, lexer: Lexer<T, Token>) -> Module {
        Parser::parse_with(session, lexer, |p| p.parse_module())
    }

    pub fn parse_with<T: BufReader, U, F>(session: &mut Session,
                                          lexer: Lexer<T, Token>,
                                          f: F) -> U
        where F: Fn(&mut StreamParser<Lexer<T, Token>>) -> U {
            let name = session.interner.intern(lexer.get_name());
            let mut tokp = StreamParser::new(session, name, lexer);
            f(&mut tokp)
    }

    pub fn parse_stream<T: Iterator<Item=SourceToken<Token>>, U, F>(session: &mut Session,
                                           name: Name,
                                           tokens: T,
                                           f: F) -> U
        where F: Fn(&mut StreamParser<T>) -> U {
            let mut tokp = StreamParser::new(session, name, tokens);
            f(&mut tokp)
    }
}

impl OpTable {
    fn parse_expr<T: Iterator<Item=SourceToken<Token>>>(&self, parser: &mut StreamParser<T>) -> Expr {
        fn parse_row<T: Iterator<Item=SourceToken<Token>>>(r: usize, rows: &[OpTableRow], parser: &mut StreamParser<T>) -> Expr {
            if r == 0 {
                parser.parse_unop_expr_maybe_cast()
            } else {
                let row = &rows[r - 1];
                let start_span = parser.cur_span();
                let parse_simpler_expr = |p: &mut StreamParser<T>| parse_row(r - 1, rows, p);
                let e = parse_simpler_expr(parser);
                parser.maybe_parse_binop(row.ops, row.assoc,
                                         parse_simpler_expr, e, start_span)
            }
        }

        parse_row(self.rows.len(), self.rows, parser)
    }
}

impl<'a, 'b, T: Iterator<Item=SourceToken<Token>>> StreamParser<'a, 'b, T> {
    fn new(session: &'a mut Session<'b>, name: Name, tokens: T) -> StreamParser<'a, 'b, T> {
        StreamParser {
            name: name,
            next: None,
            tokens: tokens,
            session: session,
            last_span: mk_sp(SourcePos::new(), 0),
            restriction: Restriction::NoRestriction,
            source: FilePath::new(".").to_path_buf(),
        }
    }

    /// Get the current cursor position as a zero-width span
    fn cur_span(&mut self) -> Span {
        let peek_begin = self.peek_span().get_begin();
        mk_sp(peek_begin, 0)
    }

    fn advance(&mut self) {
        self.next = self.tokens.next();

        if self.next.is_none() {
            panic!("Tried to advance past EOF")
        }
    }

    /// "Peek" at the next token, returning the token, without consuming
    /// it from the stream.
    fn peek(&mut self) -> &Token {
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

        let st = self.next.take().unwrap();
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

    fn error<U: AsRef<str>>(&self, message: U, pos: SourcePos) -> ! {
        use std::io::prelude::*;
        let path = self.session.interner.name_to_str(&self.name);

        let s = format!("Parse error: {}\n    at {} {}\n",
                        message.as_ref(), path, pos);
        let _ = writeln!(&mut io::stderr(), "{}", s);
        panic!()
    }

    /// A convenience function to generate an error message when we've
    /// peeked at a token, but it doesn't match any token we were expecting.
    fn peek_error<U: AsRef<str>>(&mut self, message: U) -> ! {
        let tok = self.peek().clone();
        let pos = self.peek_span().get_begin();
        self.error(format!("{} (got token {})", message.as_ref(), tok), pos)
    }

    fn add_id_and_span<U>(&mut self, val: U, sp: Span) -> WithId<U> {
        let id = self.session.parser.new_id();
        self.session.parser.spanmap.insert(id, sp);
        self.session.parser.filemap.insert(id, self.name);
        WithId { val: val, id: id }
    }

    /// Utility to parse a comma-separated list of things
    fn parse_list<U, F>(&mut self, p: F, end: Token,
                        allow_trailing_comma: bool) -> Vec<U>
        where F: Fn(&mut StreamParser<T>) -> U {
        if *self.peek() == end {
            return vec!();
        }

        let mut res = vec!(p(self));
        while *self.peek() != end {
            match *self.peek() {
                Token::Comma => {
                    self.expect(Token::Comma);
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

    fn with_restriction<U, F>(&mut self, r: Restriction, p: F) -> U
        where F: Fn(&mut StreamParser<T>) -> U {
            let mut old = r;
            ::std::mem::swap(&mut old, &mut self.restriction);
            let ret = p(self);
            ::std::mem::swap(&mut old, &mut self.restriction);
            ret
        }

    /////////////////////////////////////////////////////////////////////
    // The actual parser functions begin here!
    // Most functions from this point on are for parsing a specific node
    // in the grammar.

    fn parse_name(&mut self) -> Name {
        match self.eat() {
            Token::IdentTok(name) => self.session.interner.intern(name),
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

    fn parse_type_params(&mut self) -> Vec<ast::Type> {
        self.expect(Token::Less);
        let ps = self.parse_list(|p| p.parse_type(), Token::Greater, false);
        self.expect(Token::Greater);
        ps
    }

    fn parse_item_type_params(&mut self, other_token: Token) -> Vec<Ident> {
        if *self.peek() == other_token {
            return vec!();
        }

        match *self.peek() {
            Token::Less => {
                self.expect(Token::Less);
                let tps = self.parse_list(|p| p.parse_ident(), Token::Greater, false);
                self.expect(Token::Greater);
                tps
            },
            _ => self.peek_error("Expected type parameters or argument list")
        }
    }

    fn parse_use(&mut self) -> Import {
        let start_span = self.cur_span();

        let path = self.parse_path_common(false, true);
        let import = match *self.peek() {
            Token::Star => {
                self.expect(Token::Star);
                ImportNode {
                    elems: path.val.elems,
                    global: path.val.global,
                    import: ImportAll
                }
            }
            Token::LBrace => {
                self.expect(Token::LBrace);
                let mut idents = vec!(self.parse_ident());

                while *self.peek() == Token::Comma {
                    self.expect(Token::Comma);
                    idents.push(self.parse_ident());
                }
                self.expect(Token::RBrace);

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
            Token::ColonColon => {
                self.expect(Token::ColonColon);
                true
            }
            _ => false,
        };

        let mut path = PathNode {
            global: global,
            elems: vec!(self.parse_ident()),
        };

        while *self.peek() == Token::ColonColon {
            let start_span = self.cur_span();

            self.expect(Token::ColonColon);
            match *self.peek() {
                Token::Less if with_tps => {
                    let l = path.elems.len()-1;
                    let elem = &mut path.elems[l];
                    let tps = self.parse_type_params();
                    elem.val.tps = Some(tps);
                }
                Token::Star | Token::LBrace if for_use => {
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
            Token::NumberTok(num, _) => num,
            tok => self.error(format!("Unexpected {} where number expected",
                                      tok), self.last_span.get_begin())
        }
    }

    fn parse_string_lit(&mut self) -> String {
        match *self.peek() {
            Token::StringTok(_) => {},
            _ => {
                let tok = self.eat();
                self.error(format!("Unexpected {} where string literal expected", tok),
                           self.last_span.get_begin())
            }
        }

        // Concatenate consecutive string tokens.
        let mut result = "".to_string();
        loop {
            match *self.peek() {
                Token::StringTok(_) => {
                    if let Token::StringTok(s) = self.eat() {
                        result = result + &s;
                    } else {
                        panic!("Peek and eat gave different tokens!")
                    }
                },
                _ => break,
            }
        }
        result
    }

    pub fn parse_lit(&mut self) -> Lit {
        let start_span = self.cur_span();

        let node = match *self.peek() {
            Token::True => { self.expect(Token::True); BoolLit(true) },
            Token::False => { self.expect(Token::False); BoolLit(false) },
            Token::Null => { self.expect(Token::Null); NullLit },
            Token::StringTok(_) => { StringLit(self.parse_string_lit()) },
            Token::NumberTok(num, kind) => { self.expect_number(); NumLit(num, kind) },
            _ => {
                let tok = self.eat();
                self.error(format!("Unexpected {} where literal expected", tok),
                           self.last_span.get_begin())
            }
        };

        let end_span = self.cur_span();
        self.add_id_and_span(node, start_span.to(end_span))
    }

    fn parse_pat_common(&mut self, allow_types: bool) -> Pat {
        let start_span = self.cur_span();

        let maybe_type = |p: &mut StreamParser<T>, allow_types| {
            match *p.peek() {
                Token::Colon if allow_types => {
                    p.expect(Token::Colon);
                    let t = p.parse_type();
                    Some(t)
                }
                _ => None
            }
        };

        let pat = match *self.peek() {
            Token::ColonColon | Token::IdentTok(..) => {
                let path = self.parse_path();
                match *self.peek() {
                    Token::LParen => {
                        self.expect(Token::LParen);
                        let args = self.parse_list(|p| p.parse_pat_common(allow_types), Token::RParen, true);
                        self.expect(Token::RParen);
                        VariantPat(path, args)
                    }
                    Token::DoubleArrow => {
                        // Empty variant.
                        VariantPat(path, vec!())
                    }
                    Token::LBrace => {
                        self.expect(Token::LBrace);
                        let field_pats = self.parse_list(|p| p.parse_field_pat(), Token::RBrace, true);
                        self.expect(Token::RBrace);
                        StructPat(path, field_pats)
                    }
                    _ => {
                        if path.val.global || path.val.elems.len() != 1 {
                            self.error("Expected ident, found path".to_string(), self.last_span.get_begin());
                        }
                        let mut elems = path.val.elems;
                        let ident = elems.pop().unwrap();
                        IdentPat(ident, maybe_type(self, allow_types))
                    }
                }
            }
            Token::LParen => {
                self.expect(Token::LParen);
                let args = self.parse_list(|p| p.parse_pat_common(allow_types), Token::RParen, true);
                self.expect(Token::RParen);
                TuplePat(args)
            }
            Token::Underscore => {
                self.expect(Token::Underscore);
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
        self.expect(Token::Colon);
        let pat = self.parse_typeless_pat();
        FieldPat {
            name: name,
            pat: pat,
        }
    }

    pub fn parse_type(&mut self) -> ast::Type {
        /*
        Parse a type.
        */
        let start_span = self.cur_span();
        let node = match *self.peek() {
            Token::IntTypeTok(ik) => {
                self.eat();
                IntType(ik)
            }
            Token::Bool => {
                self.expect(Token::Bool);
                BoolType
            }
            Token::Star => {
                self.expect(Token::Star);
                PtrType(Box::new(self.parse_type()))
            }
            Token::LParen => {
                self.expect(Token::LParen);
                let mut inner_types = self.parse_list(|p| p.parse_type(), Token::RParen, true);
                self.expect(Token::RParen);
                if inner_types.len() == 0 {
                    UnitType
                } else if inner_types.len() == 1 {
                    inner_types.pop().unwrap().val
                } else {
                    TupleType(inner_types)
                }
            }
            Token::ColonColon | Token::IdentTok(..) => {
                let mut path = self.parse_path_no_tps();
                match *self.peek() {
                    Token::Less => {
                        let l = path.val.elems.len() - 1;
                        let elem = &mut path.val.elems[l];
                        elem.val.tps = Some(self.parse_type_params());
                    }
                    _ => {}
                }
                NamedType(path)
            }
            Token::Fn => {
                self.expect(Token::Fn);
                self.expect(Token::LParen);
                let arglist = self.parse_list(|p| p.parse_type(), Token::RParen, true);
                self.expect(Token::RParen);
                self.expect(Token::Arrow);
                FuncType(arglist, Box::new(self.parse_type()))
            }
            _ => self.peek_error("Expected *, opening paren, a type name, or fn"),
        };

        let mut dims = vec!();
        while *self.peek() == Token::LBracket {
            self.expect(Token::LBracket);
            let dim = self.parse_expr();
            self.expect(Token::RBracket);
            dims.push(dim);
        }

        // XXX this span is bogus but so is our array type syntax :P
        let end_span = self.cur_span();
        let sp = start_span.to(end_span);

        let mut result = self.add_id_and_span(node, sp);
        while !dims.is_empty() {
            let dim = dims.pop().unwrap();
            result = self.add_id_and_span(ArrayType(Box::new(result),
                                                    Box::new(dim)), sp);
        }

        result
    }

    pub fn parse_let_stmt(&mut self) -> Stmt {
        /* Parse a 'let' statement. There are a bunch of variations on this:
         * * `let x: int` to declare a typed variable, but not initialize it;
         * * `let x: int = 5` to declare a typed variable and initialize it;
         * * `let x = 5` to declare and initialize x, and infer the type;
         * * `let (x, y, z) = t` to deconstruct the tuple t into components.
         */
        let start_span = self.cur_span();
        self.expect(Token::Let);

        let pat = self.parse_pat();

        let expr = match *self.peek() {
            Token::Semicolon => {
                self.expect(Token::Semicolon);
                None
            },
            Token::Eq => {
                self.expect(Token::Eq);
                let var_value = self.with_restriction(Restriction::NoRestriction, |p| p.parse_expr());
                self.expect(Token::Semicolon);
                Some(var_value)
            },
            _ => self.peek_error("Expected semicolon or \"=\""),
        };

        let end_span = self.cur_span();
        self.add_id_and_span(LetStmt(pat, expr), start_span.to(end_span))
    }

    fn parse_asm(&mut self, asm_str: &str) -> Vec<Vec<InstNode>> {
        let asm_lexer = asm_lexer_from_str(asm_str);
        let mut asm_parser = AsmParser::new(asm_lexer.peekable());
        let (insts, _) = asm_parser.parse_toplevel();
        // TODO: fix this if Rust gets
        // impl<T> Clone for [T; 4] where T: Clone
        insts.into_iter().map(|x: InstPacket| vec!(x[0].clone(),
                                                   x[1].clone(),
                                                   x[2].clone(),
                                                   x[3].clone())).collect()
    }

    fn parse_asm_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::Asm);
        self.expect(Token::LParen);

        let asm_str = self.parse_string_lit();

        self.expect(Token::RParen);
        self.expect(Token::Semicolon);

        let end_span = self.cur_span();

        let insts = self.parse_asm(&asm_str);
        self.add_id_and_span(AsmExpr(insts), start_span.to(end_span))
    }

    fn parse_if_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::If);

        let cond = self.parse_expr_no_structs();
        let true_block = self.parse_block();

        let false_block = match *self.peek() {
            Token::Else => {
                self.expect(Token::Else);
                match *self.peek() {
                    Token::If => {
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
        self.add_id_and_span(IfExpr(Box::new(cond), Box::new(true_block),
                                    Box::new(false_block)),
                             start_span.to(end_span))
    }

    fn parse_return_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::Return);
        let result = ReturnExpr(Box::new(self.parse_expr()));
        let end_span = self.cur_span();
        self.add_id_and_span(result, start_span.to(end_span))
    }

    fn parse_while_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::While);
        let cond = self.parse_expr_no_structs();
        let body = self.parse_block();
        let end_span = self.cur_span();
        self.add_id_and_span(WhileExpr(Box::new(cond), Box::new(body)), start_span.to(end_span))
    }

    fn parse_do_while_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::Do);
        let body = self.parse_block();
        self.expect(Token::While);
        let cond = self.parse_expr_no_structs();
        let end_span = self.cur_span();
        self.add_id_and_span(DoWhileExpr(Box::new(cond), Box::new(body)),
                             start_span.to(end_span))
    }

    fn parse_for_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::For);
        self.expect(Token::LParen);
        let this_span = self.cur_span();
        let init = match *self.peek() {
            Token::Semicolon => self.add_id_and_span(UnitExpr, this_span),
            _ => self.parse_expr(),
        };
        self.expect(Token::Semicolon);
        let this_span = self.cur_span();
        let cond = match *self.peek() {
            Token::Semicolon => self.add_id_and_span(UnitExpr, this_span),
            _ => self.parse_expr(),
        };
        self.expect(Token::Semicolon);
        let this_span = self.cur_span();
        let iter = match *self.peek() {
            Token::RParen => self.add_id_and_span(UnitExpr, this_span),
            _ => self.parse_expr(),
        };
        self.expect(Token::RParen);
        let body = self.parse_block();
        let end_span = self.cur_span();
        self.add_id_and_span(ForExpr(Box::new(init), Box::new(cond),
                                     Box::new(iter), Box::new(body)),
                             start_span.to(end_span))
    }

    fn parse_match_arm(&mut self) -> MatchArm {
        let pat = self.parse_pat();
        self.expect(Token::DoubleArrow);
        let body = self.parse_expr();

        MatchArm {
            pat:   pat,
            body:  body,
        }
    }

    fn parse_match_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::Match);
        let matched_expr = self.parse_expr_no_structs();
        self.expect(Token::LBrace);
        let match_items = self.parse_list(|p| p.parse_match_arm(), Token::RBrace, true); // TODO don't require comma when there is a closing brace
        self.expect(Token::RBrace);
        let end_span = self.cur_span();
        self.add_id_and_span(MatchExpr(Box::new(matched_expr), match_items),
                             start_span.to(end_span))
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
                let node = UnOpExpr(op, Box::new(e));
                let end_span = self.cur_span();
                self.add_id_and_span(node, start_span.to(end_span))
            }
            _ => self.parse_simple_expr()
        }
    }

    fn parse_unop_expr_maybe_cast(&mut self) -> Expr {
        fn maybe_parse_cast<T: Iterator<Item=SourceToken<Token>>>(p: &mut StreamParser<T>, expr: Expr, start_span: Span) -> Expr {
            match *p.peek() {
                Token::As => {
                    p.expect(Token::As);
                    let t = p.parse_type();
                    let node = CastExpr(Box::new(expr), t);
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
              => self.restriction == Restriction::ExprStmtRestriction,
            _ => false,
        }
    }

    fn maybe_parse_binop<F>(&mut self,
                            ops: usize,
                            assoc: Assoc,
                            parse_simpler_expr: F,
                            e: Expr,
                            start_span: Span)
                            -> Expr
        where F: Fn(&mut StreamParser<T>) -> Expr {
        if self.expr_is_complete(&e) {
            return e;
        }

        let op = match *self.peek() {
            ref tok if binop_from_token(tok).map_or(false, |op| ops & (1 << (op as usize)) != 0) => binop_from_token(tok).unwrap(),
            _ => return e,
        };

        self.expect(binop_token(op));
        let span = self.last_span;
        let op = self.add_id_and_span(op, span);

        match assoc {
            Assoc::RightAssoc => {
                let r_span = self.cur_span();
                let r = parse_simpler_expr(self);
                let r = self.maybe_parse_binop(ops, assoc, parse_simpler_expr, r, r_span);
                let node = BinOpExpr(op, Box::new(e), Box::new(r));
                let end_span = self.cur_span();
                self.add_id_and_span(node, start_span.to(end_span))
            }
            Assoc::LeftAssoc => {
                let r = parse_simpler_expr(self);
                let node = BinOpExpr(op, Box::new(e), Box::new(r));
                let e = self.add_id_and_span(node, start_span);
                self.maybe_parse_binop(ops, assoc, parse_simpler_expr, e, start_span)
            }
            Assoc::NonAssoc => {
                let r = parse_simpler_expr(self);
                let node = BinOpExpr(op, Box::new(e), Box::new(r));
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
            ($($op:expr),+) => ($((1 << ($op as usize)))|+);
        }

        macro_rules! row {
            ($a:expr, [$($ops:expr),+,]) => (row!($a, [$($ops),+]));
            ($a:expr, [$($ops:expr),*]) => (
                (OpTableRow { assoc: $a, ops: ops!($($ops),*) })
            )
        }

        macro_rules! left {
            ($($ops:expr),+,) => (left!($($ops),+));
            ($($ops:expr),*) => (row!(Assoc::LeftAssoc, [$($ops),+]));
        }

        macro_rules! non {
            ($($ops:expr),+,) => (non!($($ops),+));
            ($($ops:expr),*) => (row!(Assoc::NonAssoc, [$($ops),+]));
        }

        macro_rules! optable {
            ($($rows:expr),+,) => (optable!($($rows),+));
            ($($rows:expr),*) => (OpTable { rows: &[$($rows),+] });
        }

        static OP_TABLE: OpTable = optable! {
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

        OP_TABLE.parse_expr(self)
    }

    fn parse_expr_no_structs(&mut self) -> Expr {
        self.with_restriction(Restriction::NoAmbiguousLBraceRestriction, |p| p.parse_expr())
    }

    pub fn parse_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let lv = self.parse_binop_expr();

        let peeked_span = self.peek_span();
        let op = match *self.peek() {
            Token::PlusEq    => Some(PlusOp),
            Token::MinusEq   => Some(MinusOp),
            Token::TimesEq   => Some(TimesOp),
            Token::SlashEq   => Some(DivideOp),
            Token::PipeEq    => Some(BitOrOp),
            Token::CaretEq   => Some(BitXorOp),
            Token::AmpEq     => Some(BitAndOp),
            Token::LshEq     => Some(LeftShiftOp),
            Token::RshEq     => Some(RightShiftOp),
            Token::PercentEq => Some(ModOp),
            Token::Eq        => None,
            _         => return lv,
        }.map(|op| self.add_id_and_span(op, peeked_span));

        self.eat();
        let e = self.parse_expr();
        let node = AssignExpr(op, Box::new(lv), Box::new(e));
        let end_span = self.cur_span();
        self.add_id_and_span(node, start_span.to(end_span))
    }

    fn parse_path_or_struct_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let path = self.parse_path();
        let node = match *self.peek() {
            Token::LBrace if self.restriction != Restriction::NoAmbiguousLBraceRestriction => {
                self.expect(Token::LBrace);
                let fields = self.parse_list(|p| {
                    let name = p.parse_name();
                    p.expect(Token::Colon);
                    let expr = p.parse_expr();
                    (name, expr)
                }, Token::RBrace, true);
                self.expect(Token::RBrace);
                StructExpr(path, fields)
            }
            _ => PathExpr(path),
        };

        let end_span = self.cur_span();
        self.add_id_and_span(node, start_span.to(end_span))
    }

    fn parse_break_expr(&mut self) -> Expr {
        self.expect(Token::Break);
        let span = self.last_span;
        self.add_id_and_span(BreakExpr, span)
    }

    fn parse_continue_expr(&mut self) -> Expr {
        self.expect(Token::Continue);
        let span = self.last_span;
        self.add_id_and_span(ContinueExpr, span)
    }

    fn eat_token_tree(&mut self) -> Vec<Token> {
        let mut tokens = vec!();

        macro_rules! get_token_tree {
            ($l:expr, $r:expr) => ({
                tokens.push($l);
                while *self.peek() != $r {
                    tokens.extend(self.eat_token_tree().into_iter());
                }
                self.expect($r);
                tokens.push($r);
            })
        }

        match self.eat() {
            Token::LParen   => get_token_tree!(Token::LParen, Token::RParen),
            Token::LBrace   => get_token_tree!(Token::LBrace, Token::RBrace),
            Token::LBracket => get_token_tree!(Token::LBracket, Token::RBracket),
            t => tokens.push(t),
        }

        tokens
    }

    fn eat_macro_token_tree(&mut self, args: &BTreeSet<Name>) -> Vec<MacroToken> {
        let mut tokens = vec!();

        macro_rules! get_token_tree {
            ($l:expr, $r:expr) => ({
                tokens.push(MacroTok($l));
                while *self.peek() != $r {
                    tokens.extend(self.eat_macro_token_tree(args).into_iter());
                }
                self.expect($r);
                tokens.push(MacroTok($r));
            })
        }

        match self.eat() {
            Token::LParen   => get_token_tree!(Token::LParen, Token::RParen),
            Token::LBrace   => get_token_tree!(Token::LBrace, Token::RBrace),
            Token::LBracket => get_token_tree!(Token::LBracket, Token::RBracket),
            Token::Dollar => {
                let name = self.parse_name();
                if args.contains(&name) {
                    tokens.push(MacroVar(name));
                } else {
                    panic!("No such argument `${}`", name);
                }
            }
            Token::DotDotDot => tokens.push(MacroVarArgs),
            t => tokens.push(MacroTok(t)),
        }

        tokens
    }

    fn parse_macro_expr_arg(&mut self) -> Vec<Token> {
        let mut tokens = vec!();
        while match *self.peek() { Token::Comma | Token::RParen => false, _ => true } {
            tokens.extend(self.eat_token_tree().into_iter())
        }

        tokens
    }

    // Used by macro expansion also
    pub fn parse_macro_call(&mut self) -> (Name, Vec<Vec<Token>>) {
        let name = match self.eat() {
            Token::IdentBangTok(name) => self.session.interner.intern(name),
            _ => unreachable!(),
        };

        self.expect(Token::LParen);
        let args = self.parse_list(|p| p.parse_macro_expr_arg(), Token::RParen, true);
        self.expect(Token::RParen);

        (name, args)
    }

    fn parse_macro_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let (name, args) = self.parse_macro_call();
        let end_span = self.cur_span();
        self.add_id_and_span(MacroExpr(name, args), start_span.to(end_span))
    }

    fn parse_sizeof_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::Sizeof);
        self.expect(Token::LParen);
        let ty = self.parse_type();
        self.expect(Token::RParen);

        let end_span = self.cur_span();
        self.add_id_and_span(SizeofExpr(ty), start_span.to(end_span))
    }

    fn parse_array_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        self.expect(Token::LBracket);
        let elems = self.parse_list(|p| p.parse_expr(), Token::RBracket, true);
        self.expect(Token::RBracket);

        let end_span = self.cur_span();
        self.add_id_and_span(ArrayExpr(elems), start_span.to(end_span))
    }

    fn parse_simple_expr(&mut self) -> Expr {
        let start_span = self.cur_span();
        let mut expr = match *self.peek() {
            Token::If                        => self.parse_if_expr(),
            Token::Return                    => self.parse_return_expr(),
            Token::Break                     => self.parse_break_expr(),
            Token::Continue                  => self.parse_continue_expr(),
            Token::Match                     => self.parse_match_expr(),
            Token::For                       => self.parse_for_expr(),
            Token::While                     => self.parse_while_expr(),
            Token::Do                        => self.parse_do_while_expr(),
            Token::LBrace                    => self.parse_block_expr(),
            Token::LParen                    => self.parse_paren_expr(),
            Token::ColonColon | Token::IdentTok(..) => self.parse_path_or_struct_expr(),
            Token::IdentBangTok(..)          => self.parse_macro_expr(),
            Token::Asm                       => self.parse_asm_expr(),
            Token::NumberTok(..) | Token::StringTok(..) | Token::True | Token::False | Token::Null => {
                let start_span = self.cur_span();
                let node = LitExpr(self.parse_lit());
                let end_span = self.cur_span();
                self.add_id_and_span(node, start_span.to(end_span))
            },
            Token::Sizeof                    => self.parse_sizeof_expr(),
            Token::LBracket                  => self.parse_array_expr(),
            _ => self.peek_error("Expected expression"),
        };

        // While the next token cannot start an expression and expr is not complete
        while !(can_start_expr(self.peek()) && self.expr_is_complete(&expr)) {
            let node = match *self.peek() {
                Token::Period => {
                    self.expect(Token::Period);
                    let field = self.parse_name();
                    DotExpr(Box::new(expr), field)
                }
                Token::Arrow => {
                    self.expect(Token::Arrow);
                    let field = self.parse_name();
                    ArrowExpr(Box::new(expr), field)
                }
                Token::LBracket => {
                    self.expect(Token::LBracket);
                    let subscript = self.parse_expr();
                    self.expect(Token::RBracket);
                    IndexExpr(Box::new(expr), Box::new(subscript))
                }
                Token::LParen => {
                    self.expect(Token::LParen);
                    let args = self.parse_list(|p| p.parse_expr(), Token::RParen, true);
                    self.expect(Token::RParen);
                    CallExpr(Box::new(expr), args)
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

        self.expect(Token::LParen);
        let mut inner_exprs = self.with_restriction(Restriction::NoRestriction,
                                                    |p| p.parse_list(|p| p.parse_expr(), Token::RParen, true));
        self.expect(Token::RParen);

        let node = if inner_exprs.len() == 0 {
            UnitExpr
        } else if inner_exprs.len() == 1 {
            GroupExpr(Box::new(inner_exprs.pop().unwrap()))
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
        self.add_id_and_span(BlockExpr(Box::new(block)),
                             start_span.to(end_span))
    }

    fn parse_stmt(&mut self) -> Stmt {
        match *self.peek() {
            Token::Let => self.parse_let_stmt(),
            _ => {
                let start_span = self.cur_span();
                let expr = self.with_restriction(Restriction::ExprStmtRestriction, |p| p.parse_expr());
                match *self.peek() {
                    Token::Semicolon => {
                        self.expect(Token::Semicolon);
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
        self.expect(Token::LBrace);
        let mut statements = vec!();
        let items = self.parse_items_until(Token::RBrace, |me| statements.push(me.parse_stmt()));
        self.expect(Token::RBrace);
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
        self.expect(Token::Colon);
        let arg_type = self.parse_type();

        FuncArg {
            ident: arg_id,
            argtype: arg_type,
        }
    }

    fn parse_extern_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Token::Extern);

        let abi = match *self.peek() {
            Token::StringTok(..) => match self.eat() { Token::StringTok(s) => Some(s), _ => unreachable!() },
            _ => None,
        };

        match *self.peek() {
            Token::Fn => {
                let (funcname, args, return_type, type_params) = self.parse_func_prototype();
                let body_opt = match *self.peek() {
                    Token::Semicolon => { self.expect(Token::Semicolon); None },
                    _ => Some(self.parse_block())
                };
                let end_span = self.cur_span();
                let abi = self.session.interner.intern(abi.unwrap_or("C".to_string()));
                self.add_id_and_span(FuncItem(funcname, args, return_type,
                                              ExternFn(abi, body_opt), type_params),
                                     start_span.to(end_span))
            }
            Token::Static => {
                if abi != None {
                    self.peek_error("ABI specifiers are invalid on extern static items");
                }

                let (name, ty) = self.parse_static_decl();
                self.expect(Token::Semicolon);
                let end_span = self.cur_span();
                self.add_id_and_span(StaticItem(name, ty, None, true),
                                     start_span.to(end_span))
            }
            _ => self.peek_error("Expected 'fn' or 'static'"),
        }
    }

    fn parse_func_prototype(&mut self) -> FuncProto {
        self.expect(Token::Fn);
        let funcname = self.parse_ident();
        let type_params = self.parse_item_type_params(Token::LParen);
        self.expect(Token::LParen);
        let args = self.parse_list(|p| p.parse_func_arg(), Token::RParen, true);
        self.expect(Token::RParen);
        let return_type = match *self.peek() {
            Token::Arrow => {
                self.expect(Token::Arrow);
                match *self.peek() {
                    Token::Bang => {
                        self.expect(Token::Bang);
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
        let body = self.parse_block();
        let end_span = self.cur_span();
        self.add_id_and_span(FuncItem(funcname, args, return_type, LocalFn(body), type_params),
                             start_span.to(end_span))
    }

    fn parse_struct_field(&mut self) -> Field {
        let name = self.parse_name();
        self.expect(Token::Colon);
        let field_type = self.parse_type();

        Field {
            name:    name,
            fldtype: field_type,
        }
    }

    fn parse_struct_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Token::Struct);
        let structname = self.parse_ident();
        let type_params = self.parse_item_type_params(Token::LBrace);
        self.expect(Token::LBrace);
        let body = self.parse_list(|p| p.parse_struct_field(), Token::RBrace, true);
        self.expect(Token::RBrace);
        let end_span = self.cur_span();
        self.add_id_and_span(StructItem(structname, body, type_params), start_span.to(end_span))
    }

    fn parse_variant(&mut self) -> Variant {
        let ident = self.parse_ident();
        let types = match *self.peek() {
            Token::LParen => {
                self.expect(Token::LParen);
                let typelist = self.parse_list(|p| p.parse_type(), Token::RParen, true);
                self.expect(Token::RParen);
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
        self.expect(Token::Enum);
        let enumname = self.parse_ident();
        let type_params = self.parse_item_type_params(Token::LBrace);
        self.expect(Token::LBrace);
        let body = self.parse_list(|p| p.parse_variant(), Token::RBrace, true);
        self.expect(Token::RBrace);
        let end_span = self.cur_span();
        self.add_id_and_span(EnumItem(enumname, body, type_params), start_span.to(end_span))
    }

    fn parse_type_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Token::Type);
        let typename = self.parse_ident();
        let type_params = self.parse_item_type_params(Token::Eq);
        self.expect(Token::Eq);
        let typedef = self.parse_type();
        self.expect(Token::Semicolon);
        let end_span = self.cur_span();
        self.add_id_and_span(TypeItem(typename, typedef, type_params), start_span.to(end_span))
    }

    fn parse_use_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Token::Use);
        let mut path = self.parse_use();
        self.expect(Token::Semicolon);

        // 'use' items are always globally-scoped
        path.val.global = true;

        let end_span = self.cur_span();
        self.add_id_and_span(UseItem(path), start_span.to(end_span))
    }

    fn parse_items_until<U, F>(&mut self, end: Token,
                               mut unmatched: F) -> Vec<Item>
        where F: FnMut(&mut StreamParser<T>) -> U {
        let mut items = vec!();
        let mut use_items = vec!();
        let mut count = 0;
        while *self.peek() != end {
            match *self.peek() {
                Token::Use => {
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
        { use_items.extend(items.into_iter()); use_items }
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
        self.expect(Token::Mod);
        let ident = self.parse_ident();
        let module = match *self.peek() {
            Token::LBrace => {
                self.expect(Token::LBrace);
                let module = self.parse_module_until(Token::RBrace);
                self.expect(Token::RBrace);
                module
            }
            Token::Semicolon => {
                self.expect(Token::Semicolon);
                let filename = {
                    let name = self.session.interner.name_to_str(&ident.val.name);

                    let base = get_cur_rel_path();
                    let filename1 = base.join(FilePath::new(&format!("{}.mb", name)));
                    let filename2 = base.join(FilePath::new(&format!("{}/mod.mb", name)));

                    match (filename1.exists(), filename2.exists()) {
                        (true,  false) => filename1,
                        (false, true)  => filename2,
                        (false, false) => {
                            // Both missing, so check our search path.
                            // Search path does *not* use the current relative path.
                            // It is based on the invocation location.
                            // (And may well be absolute, even!)
                            match self.session.options.search_paths.get(&name.to_string()) {
                                Some(path) => path.clone(),
                                None =>
                                    self.error(format!("no such module: neither {} nor {} exist.",
                                                       filename1.display(), filename2.display()),
                                               start_span.get_begin())
                            }
                        }
                        (true,  true)  =>
                            self.error(format!("ambiguous module name: both {} and {} exist.",
                                               filename1.display(), filename2.display()),
                                       start_span.get_begin()),
                    }


                };

                let file = ::std::fs::File::open(&filename).unwrap_or_else(|e| {
                    self.error(format!("failed to open {}: {}",
                                       filename.display(), e), start_span.get_begin())
                });

                self.session.parse_file(&*filename, file)
            }
            _ => self.peek_error("Expected opening brace or semicolon"),
        };
        let end_span = self.cur_span();
        self.add_id_and_span(ModItem(ident, module), start_span.to(end_span))
    }

    fn parse_static_decl(&mut self) -> StaticDecl {
        self.expect(Token::Static);
        let name = self.parse_ident();
        self.expect(Token::Colon);
        let ty = self.parse_type();
        (name, ty)
    }

    fn parse_static_item(&mut self) -> Item {
        let start_span = self.cur_span();
        let (name, ty) = self.parse_static_decl();
        let expr = match *self.peek() {
            Token::Eq => {
                self.expect(Token::Eq);
                Some(self.parse_expr())
            },
            _ => None,
        };
        self.expect(Token::Semicolon);

        let end_span = self.cur_span();
        self.add_id_and_span(StaticItem(name, ty, expr, false),
                             start_span.to(end_span))
    }

    fn parse_macro_item(&mut self) -> Item {
        let start_span = self.cur_span();
        self.expect(Token::Macro);
        let name = match self.eat() {
            Token::IdentBangTok(name) => self.session.interner.intern(name),
            tok => self.error(format!("Expected macro identifier, found {}", tok),
                              self.last_span.get_begin())
        };

        self.expect(Token::LParen);

        let args = self.parse_list(|me| me.parse_name(), Token::RParen, true);

        let mut args_map = BTreeSet::new();
        for arg in args.iter() {
            args_map.insert(*arg);
        }

        self.expect(Token::RParen);
        self.expect(Token::LBrace);

        let mut body = vec!();
        while *self.peek() != Token::RBrace {
            body.extend(self.eat_macro_token_tree(&args_map).into_iter());
        }

        self.expect(Token::RBrace);
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
        self.expect(Token::Const);
        let name = self.parse_ident();
        self.expect(Token::Colon);
        let ty = self.parse_type();
        self.expect(Token::Eq);
        let expr = self.parse_expr();
        self.expect(Token::Semicolon);
        let end_span = self.cur_span();
        self.add_id_and_span(ConstItem(name, ty, expr), start_span.to(end_span))
    }

    fn parse_item(&mut self) -> Item {
        match *self.peek() {
            Token::Fn => self.parse_func_item(),
            Token::Struct => self.parse_struct_item(),
            Token::Enum => self.parse_enum_item(),
            Token::Type => self.parse_type_item(),
            Token::Mod => self.parse_mod_item(),
            Token::Static => self.parse_static_item(),
            Token::Extern => self.parse_extern_item(),
            Token::Macro => self.parse_macro_item(),
            Token::Const => self.parse_const_item(),
            _ => self.peek_error("Expected an item definition (fn, struct, enum, mod)"),
        }
    }

    pub fn parse_module(&mut self) -> Module {
        /* This is the highest level node of the AST. This function
         * is the one that will parse an entire file. */
        let module = self.parse_module_until(Token::Eof);
        self.expect(Token::Eof);
        module
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(&format!("{}", tree)[..],
                   "((1+((3*5)/2))-((2*3)*((5+6))))");
    }

    // These tests disabled until we have a pretty printer
    /*
    fn compare_canonicalized(raw: &str, parsed: &str) {
        let (_, tree) = ast_from_str(raw, |p| p.parse_let_stmt());
        assert_eq!(&format!("{}", tree)[..], parsed);
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
