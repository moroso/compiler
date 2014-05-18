/* Holds extra information associated with each node of the AST.
 * This include spans (storing where in the source file the node came
 * from), resolver maps, definition maps, type maps, and string interning
 * maps.
 */

use std::io;

use collections::TreeMap;
use span::Span;
use ast::Module;
use ast::defmap::DefMap;
use resolver::Resolver;
use parser::Parser;
use lexer::Lexer;
use ast::visit::Visitor;

pub struct Session {
    pub defmap: DefMap,
    pub resolver: Resolver,
    pub parser: Parser,
}

impl Session {
    pub fn new() -> Session {
        Session {
            defmap: DefMap::new(),
            resolver: Resolver::new(),
            parser: Parser::new(),
        }
    }

    fn parse_common<T: Buffer>(&mut self, filename: ~str, buffer: T) -> Module {
        let lexer = Lexer::new(filename, buffer);
        let module = self.parser.parse(lexer);
        self.defmap.visit_module(&module);
        self.resolver.visit_module(&module);
        module
    }

    pub fn parse_file(&mut self, file: io::File) -> Module {
        let filename = format!("{}", file.path().display());
        self.parse_common(filename, io::BufferedReader::new(file))
    }

    pub fn parse_buffer<T: Buffer>(&mut self, buffer: T) -> Module {
        self.parse_common("<input>".to_owned(), buffer)
    }
}
