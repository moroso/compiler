use mc::ast::Module;
use mc::ast::visitor::Visitor;
use mc::session::Session;
use typechecker::{Typechecker, Typemap};

pub struct Package {
    pub module:  Module,
    pub session: Session,
    pub typemap: Typemap,
}

struct NamedBuffer<'a, T> {
    name: &'a str,
    buffer: T,
}

trait Parsable {
    fn parse(self, session: &mut Session) -> Module;
}

impl Parsable for ::std::io::File {
    fn parse(self, session: &mut Session) -> Module {
        session.parse_file(self)
    }
}

impl<'a, T: Buffer> Parsable for NamedBuffer<'a, T> {
    fn parse(self, session: &mut Session) -> Module {
        session.parse_buffer(self.name, self.buffer)
    }
}

impl Package {
    pub fn from_buffer<T: Buffer>(name: &str, buffer: T) -> Package {
        let nb = NamedBuffer {
            name: name,
            buffer: buffer,
        };

        Package::new(nb)
    }

    pub fn from_file(file: ::std::io::File) -> Package {
        Package::new(file)
    }

    fn new<T: Parsable>(parsable: T) -> Package {
        let mut session = Session::new();
        let module = parsable.parse(&mut session);
        let typemap = {
            let mut typeck = Typechecker::new(&session);
            typeck.typecheck(&module);
            typeck.get_typemap()
        };

        Package {
            module:  module,
            session: session,
            typemap: typemap,
        }
    }
}
