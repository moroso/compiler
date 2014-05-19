use ast::Module;
use ast::visit::Visitor;
use session::Session;
use typechecker::{Typechecker, Typemap};

pub struct Package {
    pub module:  Module,
    pub session: Session,
    pub typemap: Typemap,
}

impl Package {
    pub fn new<T: Buffer>(name: &str, buffer: T) -> Package {
        let mut session = Session::new();
        let module = session.parse_buffer(name, buffer);
        let typemap = {
            let mut typeck = Typechecker::new(&session);
            typeck.visit_module(&module);
            typeck.get_typemap()
        };

        Package {
            module:  module,
            session: session,
            typemap: typemap,
        }
    }
}
