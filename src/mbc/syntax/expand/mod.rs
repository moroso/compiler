use session::Session;
use syntax::ast::Module;
use syntax::ast::visitor::Visitor;

use self::inject::{PreludeInjector, StdInjector};
use self::macros::MacroExpander;

mod inject;
mod macros;

pub struct Expansion(Module);

impl Expansion {
    pub fn run(session: &mut Session, mut module: Module) -> Expansion {
        PreludeInjector::inject(session, &mut module);
        StdInjector::inject(session, &mut module);
        MacroExpander::expand(session, &mut module);

        Expansion(module)
    }

    #[cfg(test)]
    pub fn bare(module: Module) -> Expansion {
        Expansion(module)
    }

    pub fn visit<T: Visitor>(&self, visitor: &mut T) {
        let Expansion(ref module) = *self;
        visitor.visit_module(module);
    }
}
