use util::Name;
use span::Span;
use mc::lexer::Token;
use mc::session::Session;

use std::fmt;
use std::collections::TreeMap;
use std::fmt::{Formatter, Show};
use std::mem::swap;
use std::ops::Fn;

use super::mut_visitor::*;
use super::*;

pub struct MacroExpander {
    macros: TreeMap<Name, Box<Expander>>,
}

struct MacroExpanderVisitor<'a> {
    session: &'a mut Session,
}

fn expand_file(input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
    use mc::lexer::StringTok;

    assert!(input.len() == 0);

    let filename = session.parser.filename_of(&id);
    vec!(StringTok(format!("{}", filename)))
}

fn expand_line(input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
    use util::GenericInt;
    use mc::lexer::NumberTok;

    assert!(input.len() == 0);

    let span = session.parser.span_of(&id);
    vec!(NumberTok(span.get_begin().row as u64, GenericInt))
}

fn expand_concat(input: Vec<Vec<Token>>, _: NodeId, _: &mut Session) -> Vec<Token> {
    use mc::lexer::StringTok;

    let mut concat = vec!();
    println!("{}", input);
    for mut arg in input.move_iter() {
        match arg.pop() {
            Some(StringTok(s)) => concat.push(s),
            t => fail!("expected string in concat!, found {}", t),
        }

        if arg.len() > 0 {
            fail!("expected comma in concat!")
        }
    }

    vec!(StringTok(concat.connect("")))
}

fn expand_stringify(input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
    use mc::lexer::{StringTok, new_mb_lexer, SourceToken, Eof};
    use mc::parser::Parser;

    let span = session.parser.span_of(&id);
    let filename = session.parser.filename_of(&id);
    let filename_str = format!("{}", filename);

    let mut concat = vec!();
    for mut arg in input.move_iter() {
        arg.push(Eof);
        let stream = arg.move_iter().map(|t| SourceToken { sp: span, tok: t, filename: filename_str.clone() });
        let temp = Parser::parse_stream(session, filename, stream, |p| p.parse_expr());
        concat.push(format!("{}", temp));
    }

    vec!(StringTok(concat.connect(", ")))
}

type ExpanderFnSig = fn(Vec<Vec<Token>>, NodeId, &mut Session) -> Vec<Token>;
struct ExpanderFn(ExpanderFnSig);

static builtin_macros: &'static [(&'static str, ExpanderFnSig)] = &[
    ("concat", expand_concat),
    ("stringify", expand_stringify),
    ("file", expand_file),
    ("line", expand_line),
];

trait Expander {
    fn expand(&self, input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token>;
}

impl Expander for ExpanderFn {
    fn expand(&self, input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
        let ExpanderFn(f) = *self;
        f(input, id, session)
    }
}

impl Expander for WithId<MacroDef> {
    fn expand(&self, input: Vec<Vec<Token>>, _: NodeId, _: &mut Session) -> Vec<Token> {
        let mut output = vec!();

        let mut args = TreeMap::new();
        for (&name, arg) in self.val.args.iter().zip(input.move_iter()) {
            args.insert(name, arg);
        }

        for tok in self.val.body.iter() {
            match *tok {
                MacroVar(name) => {
                    output.push_all(args.find(&name).unwrap().as_slice());
                }
                MacroTok(ref tok) => {
                    output.push(tok.clone());
                }
            }
        }

        output
    }
}

struct MacroCollector {
    macros: Vec<WithId<MacroDef>>
}

impl MacroCollector {
    fn collect(module: &mut Module) -> Vec<WithId<MacroDef>> {
        let mut collector = MacroCollector { macros: vec!() };
        collector.visit_module(module);
        collector.macros
    }

    fn filter_items(&mut self, node_items: &mut Vec<Item>) {
        let mut items = vec!();
        swap(&mut items, node_items);
        for item in items.move_iter() {
            match item.val {
                MacroDefItem(def) => self.macros.push(def.with_id(item.id)),
                _ => node_items.push(item),
            }
        }
    }
}

impl MutVisitor for MacroCollector {
    fn visit_block(&mut self, block: &mut Block) {
        self.filter_items(&mut block.val.items);
        walk_block(self, block);
    }

    fn visit_module(&mut self, module: &mut Module) {
        self.filter_items(&mut module.val.items);
        walk_module(self, module);
    }
}

impl MacroExpander {
    pub fn new() -> MacroExpander {
        use tls_interner = mc::session::interner;

        let interner = tls_interner.get().unwrap();
        let mut macros = TreeMap::new();

        for &(s, e) in builtin_macros.iter() {
            let name = interner.intern(String::from_str(s));
            macros.insert(name, box ExpanderFn(e) as Box<Expander>);
        }

        MacroExpander {
            macros: macros
        }
    }

    pub fn expand_macros(session: &mut Session, module: &mut Module) {
        let user_macros = MacroCollector::collect(module);
        for def in user_macros.move_iter() {
            let name = def.val.name;
            session.expander.macros.insert(name, box def);
        }

        let mut visitor = MacroExpanderVisitor {
            session: session,
        };

        visitor.visit_module(module);
    }
}

impl<'a> MutVisitor for MacroExpanderVisitor<'a> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        use mc::lexer::Eof;
        use mc::parser::Parser;
        use util::lexer::SourceToken;

        let mut new_expr: Expr = match expr.val {
            MacroExpr(name, ref mut args) => {
                let mut my_args = vec!();
                swap(&mut my_args, args);

                let span = self.session.parser.span_of(&expr.id);
                let filename = self.session.parser.filename_of(&expr.id);

                let mut toks = unsafe {
                    let macro: & &Expander = ::std::mem::transmute(
                        self.session.expander.macros.find(&name).expect(format!("Macro {}! is undefined", name).as_slice())
                    );
                    macro.expand(my_args, expr.id, self.session)
                };

                toks.push(Eof);

                let stream = toks.move_iter().map(|t| SourceToken { sp: span, tok: t, filename: format!("{}", filename) });
                Parser::parse_stream(self.session, filename, stream, |p| p.parse_expr())
            }
            _ => return walk_expr(self, expr),
        };

        self.visit_expr(&mut new_expr);
        ::std::mem::swap(&mut new_expr, expr);
    }
}
