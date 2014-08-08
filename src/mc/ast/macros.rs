use util::Name;
use span::Span;
use mc::lexer::Token;
use mc::session::Session;

use std::fmt;
use std::collections::TreeMap;
use std::fmt::{Formatter, Show};
use std::mem::swap;
use std::ops::Fn;
use util::lexer::SourceToken;

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

fn expand_stringify(mut input: Vec<Vec<Token>>, _: NodeId, _: &mut Session) -> Vec<Token> {
    use mc::lexer::{StringTok, new_mb_lexer, SourceToken, Eof};
    use mc::parser::Parser;

    if input.len() == 0 {
        return vec!(StringTok(String::new()));
    }

    let mut concat = vec!();

    let ts = input.pop().unwrap();
    concat.push_all_move(ts.move_iter().map(|t| format!("{}", t)).collect());

    for mut ts in input.move_iter() {
        concat.push(String::from_str(","));
        concat.push_all_move(ts.move_iter().map(|t| format!("{}", t)).collect());
    }

    vec!(StringTok(concat.connect(" ")))
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
        use mc::lexer::Comma;

        let mut output = vec!();

        let mut args = TreeMap::new();
        let mut arg_iter = input.move_iter();
        for (&name, arg) in self.val.args.iter().zip(arg_iter.by_ref()) {
            args.insert(name, arg);
        }
        // This is maybe dubious, but we rely on zip not advancing its
        // second argument if the first is exhausted first to grab the
        // rest of the args that aren't bound to variables.
        let mut vararg_toks = vec!();
        // Collect up any remaining args as a comma delimited token stream
        for arg in arg_iter {
            vararg_toks.push_all(arg.as_slice());
            vararg_toks.push(Comma);
        }
        vararg_toks.pop(); // Pop off a trailing comma if it exists

        for tok in self.val.body.iter() {
            match *tok {
                MacroVar(name) => {
                    output.push_all(args.find(&name).unwrap().as_slice());
                }
                MacroTok(ref tok) => {
                    output.push(tok.clone());
                }
                MacroVarArgs => {
                    output.push_all(vararg_toks.as_slice());
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

// Some debug code
fn format_st_vec(v: &Vec<SourceToken<Token>>) -> String {
    let nv: Vec<Token> = v.iter().map(|t| t.tok.clone()).collect();
    format!("{}", nv)
}

impl<'a> MacroExpanderVisitor<'a> {
    fn expand_macro(&mut self,
                    id: &NodeId,
                    name: Name,
                    my_args: Vec<Vec<Token>>) -> Vec<SourceToken<Token>> {
        use mc::lexer::{Eof, IdentBangTok};
        use mc::parser::Parser;

        let span = self.session.parser.span_of(id);
        let filename = self.session.parser.filename_of(id);

        let mut toks = unsafe {
            let macro: & &Expander = ::std::mem::transmute(
                match self.session.expander.macros.find(&name) {
                    Some(m) => m,
                    None => self.session.error_fatal(*id, format!("Macro {}! is undefined", name).as_slice()),
                }
                );
            macro.expand(my_args, *id, self.session)
        };

        toks.push(Eof);

        let mut stream = toks.move_iter().map(
            |t| SourceToken { sp: span, tok: t, filename: format!("{}", filename) })
            .peekable();

        // Now it is time for the double expando.
        // We scan through the token stream looking for macro calls.
        // If we find one, we parse part of the stream as the call and
        // recursively expand it.
        // Is this just totally fucking wrong? It might be.
        let mut new_toks = vec!();
        while !stream.is_empty() {
            // Do some borrow checker gymnastics. Need to find out whether
            // the next thing is an IdentBangTok without retaining a borrow.
            let is_macro_call = {
                match stream.peek().unwrap().tok {
                    IdentBangTok(_) => true,
                    _ => false
                }
            };

            if is_macro_call {
                let (name, args) =
                    Parser::parse_stream(self.session, filename, stream.by_ref(),
                                         |p| p.parse_macro_call());
                let mut expanded = self.expand_macro(id, name, args);
                expanded.pop(); // Remove the EOF
                //println!("parsed {}", print_thing(&expanded));
                new_toks.push_all_move(expanded);
            } else {
                new_toks.push(stream.next().unwrap());
            }
        }

        //println!("expanded to {}", print_thing(&new_toks));
        new_toks
    }

}

impl<'a> MutVisitor for MacroExpanderVisitor<'a> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        use mc::parser::Parser;

        let mut new_expr: Expr = match expr.val {
            MacroExpr(name, ref mut args) => {
                let mut my_args = vec!();
                swap(&mut my_args, args);
                let filename = self.session.parser.filename_of(&expr.id);
                let stream = self.expand_macro(&expr.id, name, my_args).move_iter();
                Parser::parse_stream(self.session, filename, stream, |p| p.parse_expr())
            }
            _ => return walk_expr(self, expr),
        };

        ::std::mem::swap(&mut new_expr, expr);
    }
}
