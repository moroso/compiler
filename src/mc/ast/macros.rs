use util::Name;
use mc::lexer::Token;
use mc::session::Session;

use std::fmt;
use std::collections::TreeMap;
use std::fmt::{Formatter, Show};
use std::mem::swap;
use std::ops::Fn;

use super::mut_visitor::*;
use super::*;

type ExpanderArgs = (Vec<Vec<Token>>,);
struct Expander(Box<Fn<ExpanderArgs, Vec<Token>>>);

impl Fn<ExpanderArgs, Vec<Token>> for Expander {
    fn call(&self, args: ExpanderArgs) -> Vec<Token> {
        use mc::lexer::Eof;
        let Expander(ref f) = *self;
        let mut tokens = f.call(args);
        tokens.push(Eof);
        tokens
    }
}

pub struct MacroExpander<'a> {
    session: &'a mut Session,
    macros: TreeMap<Name, Expander>,
}

fn expand_concat(args: ExpanderArgs) -> Vec<Token> {
    use mc::lexer::IdentTok;

    let (input,) = args;

    let mut concat = vec!();
    for mut arg in input.move_iter() {
        match arg.pop() {
            Some(IdentTok(s)) => concat.push(s),
            _ => fail!("expected ident in concat!"),
        }

        if arg.len() > 0 {
            fail!("expected comma in concat!")
        }
    }

    vec!(IdentTok(concat.connect("")))
}

fn expand_stringify(args: ExpanderArgs) -> Vec<Token> {
    use mc::lexer::{IdentTok, NumberTok, StringTok};

    let (input,) = args;

    let mut concat = vec!();
    for mut arg in input.move_iter() {
        match arg.pop() {
            Some(IdentTok(s)) => concat.push(s),
            Some(StringTok(s)) => concat.push(s),
            Some(NumberTok(u, _)) => concat.push(format!("{}", u)),
            _ => fail!("expected ident or literal in stringify!"),
        }

        if arg.len() > 0 {
            fail!("expected comma in stringify!")
        }
    }

    vec!(StringTok(concat.connect("")))
}

type ExpanderFn = fn(ExpanderArgs) -> Vec<Token>;
static builtin_macros: &'static [(&'static str, ExpanderFn)] = &[
    ("concat", expand_concat),
    ("stringify", expand_stringify),
];

struct FnWrapper(ExpanderFn);
impl Fn<ExpanderArgs, Vec<Token>> for FnWrapper {
    fn call(&self, args: ExpanderArgs) -> Vec<Token> {
        let FnWrapper(f) = *self;
        f(args)
    }
}

impl Fn<ExpanderArgs, Vec<Token>> for WithId<MacroDef> {
    fn call(&self, args: ExpanderArgs) -> Vec<Token> {
        let (input,) = args;
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
        self.filter_items(&mut block.val.items)
    }

    fn visit_module(&mut self, module: &mut Module) {
        self.filter_items(&mut module.val.items)
    }
}

impl<'a> MacroExpander<'a> {
    pub fn expand_macros(session: &'a mut Session, module: &mut Module) {
        let mut macros = TreeMap::new();
        for &(s, e) in builtin_macros.iter() {
            let name = session.interner.intern(String::from_str(s));
            macros.insert(name, Expander(box FnWrapper(e)));
        }

        let user_macros = MacroCollector::collect(module);
        for def in user_macros.move_iter() {
            let name = def.val.name;
            macros.insert(name, Expander(box def));
        }

        let mut expander = MacroExpander {
            session: session,
            macros: macros,
        };

        expander.visit_module(module);
    }
}

impl<'a> MutVisitor for MacroExpander<'a> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        use util::lexer::SourceToken;
        use mc::parser::{Parser, StreamParser};

        let mut new_expr: Expr = match expr.val {
            MacroExpr(name, ref mut args) => {
                let mut my_args = vec!();
                swap(&mut my_args, args);
                let expand = self.macros.find(&name).expect(format!("Macro {}! is undefined", name).as_slice());

                let span = self.session.parser.span_of(&expr.id);
                let filename = self.session.parser.filename_of(&expr.id);
                let toks = expand.call((my_args,)); // TODO use the call operator instead of the method
                let stream = toks.move_iter().map(|t| SourceToken { sp: span, tok: t });
                Parser::parse_stream(self.session, filename, stream, |p| p.parse_expr())
            }
            _ => return,
        };

        ::std::mem::swap(&mut new_expr, expr);
    }
}
