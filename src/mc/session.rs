/* Holds extra information associated with each node of the AST.
 * This include spans (storing where in the source file the node came
 * from), resolver maps, definition maps, type maps, and string interning
 * maps.
 */

use span::Span;
use util::Name;
use mc::ast::NodeId;

use super::ast::Module;
use super::ast::defmap::DefMap;
use super::resolver::Resolver;
use super::parser::Parser;
use super::lexer::new_mb_lexer;
use super::ast::visitor::Visitor;
use super::ast::macros::MacroExpander;

use std::collections::{HashMap, TreeMap};
use std::cell::RefCell;

use std::io;
use std::local_data;

local_data_key!(pub interner: Interner)
local_data_key!(pub cur_rel_path: Path)

pub fn get_cur_rel_path() -> Path {
    cur_rel_path.get().unwrap().clone()
}

pub struct Options {
    pub search_paths: HashMap<String, String>,
}

pub struct Session {
    pub options: Options,
    pub defmap: DefMap,
    pub resolver: Resolver,
    pub parser: Parser,
    pub expander: MacroExpander,
    pub interner: local_data::Ref<Interner>,
}

pub struct Interner {
    strings: RefCell<HashMap<String, Name>>,
}

impl Options {
    pub fn new() -> Options {
        Options { search_paths: HashMap::new() }
    }
}


impl Interner {
    pub fn new() -> Interner {
        Interner { strings: RefCell::new(HashMap::new()) }
    }

    pub fn name_to_str<'a>(&'a self, name: &Name) -> &'a str {
        // A BiMap would be nice here
        let strings = self.strings.borrow();
        for x in strings.iter() {
            if x.val1() == name {
                unsafe {
                    use std::mem::copy_lifetime;
                    return copy_lifetime(self, x.val0()).as_slice();
                }
            }
        }

        fail!()
    }

    pub fn intern(&self, s: String) -> Name {
        //match self.strings.find_equiv(&s) {
        //    Some(name) => *name,
        //    None => {
        //        let name = Name(self.strings.len());
        //        self.strings.insert(s, name);
        //        name
        //    }
        //}
        let mut strings = self.strings.borrow_mut();
        let name = Name(strings.len());
        *strings.find_or_insert(s, name)
    }
}

impl Session {
    pub fn new(opts: Options) -> Session {
        // XXX this is such a massive hack omg
        if interner.get().is_none() {
            interner.replace(Some(Interner::new()));
        }
        if cur_rel_path.get().is_none() {
            cur_rel_path.replace(Some(Path::new(".")));
        }

        let interner_ref = interner.get().unwrap();

        Session {
            options: opts,
            defmap: DefMap::new(),
            resolver: Resolver::new(),
            parser: Parser::new(),
            expander: MacroExpander::new(),
            interner: interner_ref,
        }
    }

    pub fn messages<T: Str>(&self, errors: &[(NodeId, T)]) {
        let mut full_msg = String::new();
        for &(nid, ref msg) in errors.iter() {
            full_msg.push_str(msg.as_slice());
            full_msg.push_char('\n');
            let fname = self.interner.name_to_str(&self.parser.filename_of(&nid));
            full_msg.push_str(
                format!("   {}: {}\n", fname, self.parser.span_of(&nid)).as_slice());
        }

        let _ = io::stderr().write_str(full_msg.as_slice());
    }

    pub fn message<T: Str>(&self, nid: NodeId, msg: T) {
        self.messages([(nid, msg)]);
    }

    pub fn errors_fatal<T: Str>(&self, errors: &[(NodeId, T)]) -> ! {
        self.messages(errors);
        let _ = io::stderr().write_str("\n");
        fail!("Aborting")
    }
    pub fn error_fatal<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        self.errors_fatal([(nid, msg)]);
    }

    // For now everything is fatal.
    pub fn error<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        self.error_fatal(nid, msg);
    }

    pub fn bug_span<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        let sp = self.parser.span_of(&nid);
        fail!("\nBum bum bum budda bum bum tsch:\n\
              Internal Compiler Error{}\n\
              at: {}\n", msg.as_slice(), sp);
    }

    fn inject_std(&mut self, module: &mut Module) {
        use std::str::StrSlice;
        use std::mem::swap;

        let s = include_str!("std.mb");
        let bytes = Vec::from_slice(s.as_bytes());
        let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
        let lexer = new_mb_lexer("<stdlib>", buffer);
        let mut temp = Parser::parse(self, lexer);
        swap(&mut module.val.items, &mut temp.val.items);
        module.val.items.push_all_move(temp.val.items);
    }

    fn inject_prelude(&mut self, module: &mut Module) {
        use std::str::StrSlice;
        use std::mem::swap;

        let s = include_str!("prelude.mb");
        let bytes = Vec::from_slice(s.as_bytes());
        let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
        let lexer = new_mb_lexer("<prelude>", buffer);
        let mut temp = Parser::parse(self, lexer);
        swap(&mut module.val.items, &mut temp.val.items);
        module.val.items.push_all_move(temp.val.items);
    }

    fn parse_buffer<S: StrAllocating, T: Buffer>(&mut self, name: S, buffer: T) -> Module {
        let lexer = new_mb_lexer(name, buffer);
        Parser::parse(self, lexer)
    }

    pub fn parse_package_buffer<S: StrAllocating, T: Buffer>(&mut self, name: S, buffer: T) -> Module {
        use super::ast::mut_visitor::MutVisitor;

        struct PreludeInjector<'a> { session: &'a mut Session }
        impl<'a> MutVisitor for PreludeInjector<'a> {
            fn visit_module(&mut self, module: &mut Module) {
                use super::ast::mut_visitor::walk_module;
                self.session.inject_prelude(module);
                walk_module(self, module);
            }
        }

        let mut module = self.parse_buffer(name, buffer);

        {
            let mut injector = PreludeInjector { session: self };
            injector.visit_module(&mut module);
        }

        self.inject_std(&mut module);

        MacroExpander::expand_macros(self, &mut module);
        DefMap::record(self, &module);
        Resolver::resolve(self, &module);
        module
    }

    pub fn parse_file_common(&mut self, file: io::File, f: |&mut Session, String, io::BufferedReader<io::File>| -> Module) -> Module {
        let filename = format!("{}", file.path().display());
        let old_wd = cur_rel_path.replace(Some(file.path().dir_path()));
        let module = f(self, filename, io::BufferedReader::new(file));
        cur_rel_path.replace(old_wd);
        module
    }

    pub fn parse_file(&mut self, file: io::File) -> Module {
        self.parse_file_common(file, |me, filename, buf| me.parse_buffer(filename, buf))
    }

    pub fn parse_package_file(&mut self, file: io::File) -> Module {
        self.parse_file_common(file, |me, filename, buf| me.parse_package_buffer(filename, buf))
    }

    pub fn parse_package_str(&mut self, s: &str) -> Module {
        use std::str::StrSlice;
        let bytes = Vec::from_slice(s.as_bytes());
        let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
        self.parse_package_buffer("<input>", buffer)
    }
}
