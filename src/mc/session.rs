/* Holds extra information associated with each node of the AST.
 * This include spans (storing where in the source file the node came
 * from), resolver maps, definition maps, type maps, and string interning
 * maps.
 */

use span::Span;
use util::Name;
use mc::ast::{NodeId, DUMMY_NODEID};



use super::ast::Module;
use super::ast::defmap::DefMap;
use super::resolver::Resolver;
use super::resolver::ModuleResolver;
use super::parser::Parser;
use super::lexer::new_mb_lexer;
use super::ast::visit::Visitor;

use std::collections::{HashMap, TreeMap};

use std::io;

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

    pub fn messages<T: Str>(&self, errors: &[(NodeId, T)]) {
        let mut full_msg = String::new();
        for &(nid, ref msg) in errors.iter() {
            full_msg.push_str(msg.as_slice());
            full_msg.push_char('\n');
            if nid != DUMMY_NODEID {
                let fname = self.interner.name_to_str(&self.parser.filename_of(&nid));
                full_msg.push_str(
                    format!("   {}: {}\n", fname, self.parser.span_of(&nid)).as_slice());
            };


        }

        print!("{}", full_msg);
    }

    pub fn message<T: Str>(&self, nid: NodeId, msg: T) {
        self.messages([(nid, msg)]);
    }

    pub fn errors_fatal<T: Str>(&self, errors: &[(NodeId, T)]) -> ! {
        self.messages(errors);
        println!("");
        fail!("Aborting")
    }
    pub fn error_fatal<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        self.errors_fatal([(nid, msg)]);
    }
    pub fn error<T: Str>(&self, nid: NodeId, msg: T) {
        // For now everything is fatal.
        self.error_fatal(nid, msg);
    }

    pub fn bug_span<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        let sp = self.parser.span_of(&nid);
        fail!("\nBum bum bum budda bum bum tsch:\n\
              Internal Compiler Error{}\n\
              at: {}\n", msg.as_slice(), sp);
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
        ModuleResolver::process(self, &module);
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
