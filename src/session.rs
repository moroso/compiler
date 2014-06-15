/* Holds extra information associated with each node of the AST.
 * This include spans (storing where in the source file the node came
 * from), resolver maps, definition maps, type maps, and string interning
 * maps.
 */

use std::io;

use std::collections::{HashMap, TreeMap};
use span::Span;
use ast::Module;
use ast::defmap::DefMap;
use resolver::Resolver;
use parser::Parser;
use lexer::Lexer;
use compiler_lexer::new_mb_lexer;
use ast::visit::Visitor;
use util::Name;

pub struct Session {
    pub defmap: DefMap,
    pub resolver: Resolver,
    pub parser: Parser,
    pub interner: Interner,
}

pub struct Interner {
    strings: HashMap<String, Name>,
}

impl Interner {
    pub fn new() -> Interner {
        Interner { strings: HashMap::new() }
    }

    pub fn name_to_str<'a>(&'a self, name: &Name) -> &'a str {
        // A BiMap would be nice here
        for x in self.strings.iter() {
            if x.val1() == name {
                return x.val0().as_slice();
            }
        }

        fail!()
    }

    pub fn intern(&mut self, s: String) -> Name {
        //match self.strings.find_equiv(&s) {
        //    Some(name) => *name,
        //    None => {
        //        let name = Name(self.strings.len());
        //        self.strings.insert(s, name);
        //        name
        //    }
        //}
        let name = Name(self.strings.len());
        *self.strings.find_or_insert(s, name)
    }
}

impl Session {
    pub fn new() -> Session {
        Session {
            defmap: DefMap::new(),
            resolver: Resolver::new(),
            parser: Parser::new(),
            interner: Interner::new(),
        }
    }

    fn inject_prelude(&mut self, module: &mut Module) {
        use std::str::StrSlice;
        use std::mem::swap;

        let s = include_str!("prelude.mb");
        let bytes = Vec::from_slice(s.as_bytes());
        let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
        let lexer = new_mb_lexer("<prelude>", buffer);
        let mut temp = self.parser.parse(lexer, &mut self.interner);
        swap(&mut module.val.items, &mut temp.val.items);
        module.val.items.push_all_move(temp.val.items);
    }

    pub fn parse_buffer<S: StrAllocating, T: Buffer>(&mut self, name: S, buffer: T) -> Module {
        let lexer = new_mb_lexer(name, buffer);
        let mut module = self.parser.parse(lexer, &mut self.interner);
        self.inject_prelude(&mut module);
        self.defmap.read_module(&module);
        self.resolver.resolve_module(&self.interner, &module);
        module
    }

    pub fn parse_file(&mut self, file: io::File) -> Module {
        let filename = format!("{}", file.path().display());
        self.parse_buffer(filename, io::BufferedReader::new(file))
    }

    pub fn parse_str(&mut self, s: &str) -> Module {
        use std::str::StrSlice;
        let bytes = Vec::from_slice(s.as_bytes());
        let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
        self.parse_buffer("<input>", buffer)
    }
}
