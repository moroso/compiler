use std::io;

use session::Session;

use syntax::ast::Module;
use syntax::ast::mut_visitor::MutVisitor;
use syntax::lexer::new_mb_lexer;
use syntax::parser::Parser;

static PRELUDE: &'static str = include_str!("prelude.mb");
static STDLIB: &'static str = include_str!("std.mb");

pub struct PreludeInjector<'a> {
    session: &'a mut Session
}

pub struct StdInjector;

fn inject(src: &str, name: &str, module: &mut Module, session: &mut Session) {
    use std::mem::swap;

    let bytes = src.as_bytes().to_vec();
    let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
    let lexer = new_mb_lexer(name, buffer);
    let name = session.interner.intern(lexer.get_name());
    let mut parser = Parser::new(session, name, lexer);
    let mut temp = parser.parse_module();
    swap(&mut module.val.items, &mut temp.val.items);
    module.val.items.extend(temp.val.items.into_iter());
}

impl<'a> PreludeInjector<'a> {
    pub fn inject(session: &mut Session, module: &mut Module) {
        let mut injector = PreludeInjector { session: session };
        injector.visit_module(module);
    }
}

impl<'a> MutVisitor for PreludeInjector<'a> {
    fn visit_module(&mut self, module: &mut Module) {
        use syntax::ast::mut_visitor::walk_module;
        inject(PRELUDE, "<prelude>", module, self.session);
        walk_module(self, module);
    }
}

impl StdInjector {
    pub fn inject(session: &mut Session, module: &mut Module) {
        inject(STDLIB, "<stdlib>", module, session);
    }
}
