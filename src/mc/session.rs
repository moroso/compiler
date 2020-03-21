/* Holds extra information associated with each node of the AST.
 * This include spans (storing where in the source file the node came
 * from), resolver maps, definition maps, type maps, and string interning
 * maps.
 */

use util::Name;
use mc::ast::NodeId;
use util::lexer::BufReader;

use super::ast::Module;
use super::ast::defmap::DefMap;
use super::ast::pathmap::PathMap;
use super::resolver::Resolver;
use super::parser::Parser;
use super::lexer::new_mb_lexer;
use super::ast::macros::MacroExpander;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::cell::RefCell;

use std::io;
use std::path::{Path, PathBuf};
use std::fs;

thread_local! {
    pub static INTERNER: ::std::rc::Rc<Interner> = ::std::rc::Rc::new(Interner::new())
}

thread_local! {
    pub static CUR_REL_PATH: RefCell<PathBuf> = RefCell::new(Path::new(".").to_path_buf())
}

pub fn get_cur_rel_path() -> PathBuf {
    CUR_REL_PATH.with(|p| p.borrow().clone())
}

#[derive(Default)]
pub struct Options {
    pub search_paths: HashMap<String, PathBuf>,
    pub include_prelude: bool,
}


impl Options {
    pub fn new() -> Options {
        Options {
            search_paths: HashMap::new(),
            include_prelude: true,
        }
    }
}

pub struct Session<'a> {
    pub options: Options,
    pub defmap: DefMap,
    pub pathmap: PathMap,
    pub resolver: Resolver,
    pub parser: Parser,
    pub expander: MacroExpander<'a>,
    pub interner: ::std::rc::Rc<Interner>,
}

#[derive(Default)]
pub struct Interner {
    strings: RefCell<HashMap<String, Name>>,
}

unsafe impl Sync for Interner {}

impl Borrow<usize> for Name {
    fn borrow(&self) -> &usize {
        let Name(ref id) = *self;
        id
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
            if x.1 == name {
                unsafe {
                    use util::copy_lifetime;
                    return &copy_lifetime(self, x.0)[..];
                }
            }
        }
        panic!()
    }

    pub fn intern(&self, s: String) -> Name {
        //match self.strings.find_equiv(&s) {
        // Some(name) => *name,
        // None => {
        // let name = Name(self.strings.len());
        // self.strings.insert(s, name);
        // name
        // }
        //}
        let mut strings = self.strings.borrow_mut();
        let name = Name(strings.len());
        *strings.entry(s).or_insert(name)
    }
}

impl<'a> Session<'a> {
    pub fn new(opts: Options) -> Session<'a> {
        INTERNER.with(|x| {
            Session {
                options: opts,
                defmap: DefMap::new(),
                pathmap: PathMap::new(),
                resolver: Resolver::new(),
                parser: Parser::new(),
                expander: MacroExpander::new(),
                interner: x.clone(),
            }
        })
    }

    pub fn messages<T: AsRef<str>>(&self, errors: &[(NodeId, T)]) {
        use std::io::prelude::*;
        let mut full_msg = String::new();
        for &(nid, ref msg) in errors.iter() {
            full_msg.push_str(msg.as_ref());
            full_msg.push_str("\n");
            let fname = self.interner.name_to_str(&self.parser.filename_of(&nid));
            full_msg.push_str(
                &format!("   {}: {}\n", fname, self.parser.span_of(&nid))[..]);
        }

        let _ = writeln!(&mut io::stderr(), "{}", full_msg);
    }

    pub fn message<T: AsRef<str>>(&self, nid: NodeId, msg: T) {
        self.messages(&[(nid, msg)]);
    }

    pub fn errors_fatal<T: AsRef<str>>(&self, errors: &[(NodeId, T)]) -> ! {
        use std::io::prelude::*;
        self.messages(errors);
        let _ = writeln!(&mut io::stderr(), "");
        panic!("Aborting")
    }
    pub fn error_fatal<T: AsRef<str>>(&self, nid: NodeId, msg: T) -> ! {
        self.errors_fatal(&[(nid, msg)]);
    }

    // For now everything is fatal.
    pub fn error<T: AsRef<str>>(&self, nid: NodeId, msg: T) -> ! {
        self.error_fatal(nid, msg);
    }

    pub fn bug_span<T: AsRef<str>>(&self, nid: NodeId, msg: T) -> ! {
        let sp = self.parser.span_of(&nid);
        panic!("\nBum bum bum budda bum bum tsch:\n\
              Internal Compiler Error{}\n\
              at: {}\n", msg.as_ref(), sp);
    }

    fn inject(&mut self, src: &str, name: &str, module: &mut Module) {
        use std::mem::swap;

        let bytes = src.as_bytes();
        let buffer = io::BufReader::new(bytes);
        let lexer = new_mb_lexer(name, buffer);
        let mut temp = Parser::parse(self, lexer);
        swap(&mut module.val.items, &mut temp.val.items);
        module.val.items.extend(temp.val.items.into_iter());
    }


    fn inject_std(&mut self, module: &mut Module) {
        if self.options.include_prelude {
            let s = include_str!("std.mb");
            self.inject(s, "<stdlib>", module);
        }
    }

    fn inject_prelude(&mut self, module: &mut Module) {
        if self.options.include_prelude {
            let s = include_str!("prelude.mb");
            self.inject(s, "<prelude>", module);
        }
    }

    fn parse_buffer<S: ?Sized + ToString, T: BufReader>(&mut self, name: &S, buffer: T) -> Module {
        let lexer = new_mb_lexer(name, buffer);
        Parser::parse(self, lexer)
    }

    pub fn parse_package_buffer<S: ?Sized + ToString, T: BufReader>(
        &mut self, name: &S, buffer: T) -> Module {

        use super::ast::mut_visitor::MutVisitor;

        struct PreludeInjector<'a,'b: 'a> { session: &'a mut Session<'b> }
        impl<'a,'b> MutVisitor for PreludeInjector<'a,'b> {
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
        PathMap::record(self, &module);
        Resolver::resolve(self, &module);
        module
    }

    pub fn parse_file_common<F>(&mut self, filename: &Path, file: fs::File, f: F) -> Module
        where F: Fn(&mut Session, String,
                    io::BufReader<fs::File>) -> Module {
        use std::mem::replace;

        let filename_str = filename.to_str().unwrap().to_string();
        let old_wd = CUR_REL_PATH.with(|p| replace(&mut *p.borrow_mut(),
                                                   filename.parent().unwrap().to_path_buf()));
        let module = f(self, filename_str, io::BufReader::new(file));
        CUR_REL_PATH.with(|p| replace(&mut *p.borrow_mut(), old_wd));
        module
    }

    // FIXME: there is some silliness here with filename
    pub fn parse_file(&mut self, filename: &Path, file: fs::File) -> Module {
        self.parse_file_common(filename, file,
                               |me, filename, buf| me.parse_buffer(
                                   &filename[..], buf))
    }
    pub fn parse_package_file(&mut self, filename: &Path, file: fs::File) -> Module {
        self.parse_file_common(filename, file,
                               |me, filename, buf| me.parse_package_buffer(
                                   &filename[..], buf))
    }

    pub fn parse_package_str(&mut self, s: &str) -> Module {
        let bytes = s.as_bytes();
        let buffer = io::BufReader::new(bytes);
        self.parse_package_buffer("<input>", buffer)
    }
}
