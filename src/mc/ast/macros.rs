use util::{Name, Width, IntKind};
use span::Span;
use mc::lexer::Token;
use mc::session::Session;

use std::fmt;
use std::collections::BTreeMap;
use std::fmt::{Formatter, Display};
use std::mem::swap;
use std::ops::Fn;
use util::lexer::SourceToken;

use super::mut_visitor::*;
use super::*;

pub struct MacroExpander<'a> {
    macros: BTreeMap<Name, Box<Expander + 'a>>,
}

struct MacroExpanderVisitor<'a> {
    session: &'a mut Session<'a>,
}

fn expand_file(input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
    assert!(input.len() == 0);

    let filename = session.parser.filename_of(&id);
    vec!(Token::StringTok(format!("{}", filename)))
}

fn expand_line(input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
    assert!(input.len() == 0);

    let span = session.parser.span_of(&id);
    vec!(Token::NumberTok(span.get_begin().row as u64, IntKind::GenericInt))
}

// This lets you produce some bogus idents.
// Also this is probably not very useful.
fn expand_paste(input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
    let mut concat = vec!();
    for arg in input.into_iter() {
        for elem in arg.into_iter() {
            match elem {
                Token::IdentTok(s) => concat.push(s),
                Token::NumberTok(n, _) => concat.push(format!("{}", n)),
                t => session.error(id, format!("expected ident or num in concat!, found {}", t)),
            }
        }
    }

    vec!(Token::IdentTok(concat.connect("")))
}

fn expand_concat(input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
    let mut concat = vec!();
    for mut arg in input.into_iter() {
        match arg.pop() {
            Some(Token::StringTok(s)) => concat.push(s),
            t => session.error(id, format!("expected string in concat!, found {:?}", t)),
        }

        if arg.len() > 0 {
            session.error(id, "expected comma in concat!");
        }
    }

    vec!(Token::StringTok(concat.connect("")))
}

fn expand_stringify(mut input: Vec<Vec<Token>>, _: NodeId, _: &mut Session) -> Vec<Token> {
    if input.len() == 0 {
      return vec!(Token::StringTok(String::new()));
    }

    let mut concat = vec!();

    let ts = input.pop().unwrap();
    concat.extend(ts.into_iter().map(|t| format!("{}", t)));

    for ts in input.into_iter() {
        concat.push(",".to_string());
        concat.extend(ts.into_iter().map(|t| format!("{}", t)));
    }

    vec!(Token::StringTok(concat.connect(" ")))
}

fn expand_map_macro(input: Vec<Vec<Token>>, id: NodeId, session: &mut Session) -> Vec<Token> {
    if input.len() < 1 {
        session.error_fatal(id, "Not enough arguments to map_toks!");
    }
    let mut iter = input.into_iter();
    let prefix = iter.next().unwrap();

    let mut out = vec!();
    for toks in iter {
        out.push_all(prefix.as_slice());
        out.push(Token::LParen);
        out.push_all(toks.as_slice());
        out.push(Token::RParen);
        out.push(Token::Comma);
    }
    out.pop(); // pop the trailing comma, if there is one

    out
}



type ExpanderFnSig = fn(Vec<Vec<Token>>, NodeId, &mut Session) -> Vec<Token>;
struct ExpanderFn(ExpanderFnSig);

static builtin_macros: &'static [(&'static str, ExpanderFnSig)] = &[
    ("paste", expand_paste),
    ("concat", expand_concat),
    ("stringify", expand_stringify),
    ("file", expand_file),
    ("line", expand_line),
    ("map_macro", expand_map_macro),
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

        let mut args = BTreeMap::new();
        let mut arg_iter = input.into_iter();
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
            vararg_toks.push(Token::Comma);
        }
        vararg_toks.pop(); // Pop off a trailing comma if it exists

        let mut skip_comma = false;
        for tok in self.val.body.iter() {
            let mut is_empty_varargs = false;
            match *tok {
                MacroVar(name) => {
                    output.push_all(args.get(&name).unwrap().as_slice());
                }
                // We skip a comma that occurs after an empty ...
                MacroTok(Token::Comma) if skip_comma => {}
                MacroTok(ref tok) => {
                    output.push(tok.clone());
                }
                MacroVarArgs => {
                    output.push_all(vararg_toks.as_slice());
                    is_empty_varargs = vararg_toks.is_empty();
                }
            }
            skip_comma = is_empty_varargs;
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
        for item in items.into_iter() {
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

impl<'a> MacroExpander<'a> {
    pub fn new() -> MacroExpander<'a> {
        use mc::session::interner;

        let mut macros = BTreeMap::new();

        for &(s, e) in builtin_macros.iter() {
            interner.with(|x| {
                let name = x.intern(s.to_string());
                macros.insert(name, Box::new(ExpanderFn(e)) as Box<Expander>);
            })
        }

        MacroExpander {
            macros: macros
        }
    }

    pub fn expand_macros(session: &'a mut Session<'a>, module: &mut Module) {
        let user_macros = MacroCollector::collect(module);
        for def in user_macros.into_iter() {
            let name = def.val.name;
            session.expander.macros.insert(name, Box::new(def));
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
    format!("{:?}", nv)
}

impl<'a> MacroExpanderVisitor<'a> {
    fn expand_macro(&mut self,
                    id: &NodeId,
                    name: Name,
                    my_args: Vec<Vec<Token>>) -> Vec<SourceToken<Token>> {
        use mc::parser::Parser;

        let span = self.session.parser.span_of(id);
        let filename = self.session.parser.filename_of(id);

        let mut toks = unsafe {
            let this_macro: & &Expander = ::std::mem::transmute(
                match self.session.expander.macros.get(&name) {
                    Some(m) => m,
                    None => self.session.error_fatal(*id, format!("Macro {}! is undefined", name).as_slice()),
                }
                );
            this_macro.expand(my_args, *id, self.session)
        };

        toks.push(Token::Eof);

        let mut stream = toks.into_iter().map(
            |t| SourceToken { sp: span, tok: t })
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
                    Token::IdentBangTok(_) => true,
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
                new_toks.extend(expanded.into_iter());
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
                let stream = self.expand_macro(&expr.id, name, my_args).into_iter();
                Parser::parse_stream(self.session, filename, stream, |p| p.parse_expr())
            }
            _ => return walk_expr(self, expr),
        };

        ::std::mem::swap(&mut new_expr, expr);
    }
}
