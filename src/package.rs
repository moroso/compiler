use mc::ast::Module;
use mc::session::{Session, Options};
use typechecker::{Typechecker, Typemap};
use util::lexer::BufReader;

pub struct Package<'a> {
    pub module:  Module,
    pub session: Session<'a>,
    pub typemap: Typemap,
}

struct NamedBuffer<'a, T> {
    name: &'a str,
    buffer: T,
}

trait Parsable {
    fn parse(self, session: &mut Session) -> Module;
}

impl<'a> Parsable for &'a ::std::path::Path {
    fn parse(self, session: &mut Session) -> Module {
        let file = ::std::fs::File::open(&self).unwrap_or_else(|e| panic!("{}", e));
        session.parse_package_file(self, file)
    }
}

impl<'a, T: BufReader> Parsable for NamedBuffer<'a, T> {
    fn parse(self, session: &mut Session) -> Module {
        session.parse_package_buffer(self.name, self.buffer)
    }
}

impl<'a> Package<'a> {
    pub fn from_buffer<T: BufReader>(opts: Options, name: &str, buffer: T) -> Package<'a> {
        let nb = NamedBuffer {
            name: name,
            buffer: buffer,
        };

        Package::new(opts, nb)
    }

    pub fn from_path(opts: Options, path: &::std::path::Path) -> Package<'a> {
        Package::new(opts, path)
    }

    fn new<T: Parsable>(opts: Options, parsable: T) -> Package<'a> {
        let mut session = Session::new(opts);
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
