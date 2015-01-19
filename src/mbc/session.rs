/* Holds extra information associated with each node of the AST.
 * This include spans (storing where in the source file the node came
 * from), resolver maps, definition maps, type maps, and string interning
 * maps.
 */

use std::borrow::IntoCow;
use std::cell::RefCell;
use std::collections::{HashMap, BTreeMap};
use std::io;
use std::thread_local;

use mclib::span::Span;
use mclib::intern::{Interner, Name};
use mclib::lexer::{Lexer, SourceToken};

use syntax::ast::{self, NodeId, Module};
use syntax::lexer::{Token, new_mb_lexer};
use syntax::parser::{Parser, ParseState};

pub struct Options {
    pub search_paths: HashMap<String, Path>,
}

pub struct Session {
    pub options: Options,
    pub interner: Interner,
    pub state: ParseState,
}

impl Options {
    pub fn new() -> Options {
        Options { search_paths: HashMap::new() }
    }
}

impl Session {
    pub fn new(options: Options) -> Session {
        Session {
            options: options,
            interner: Interner::new(),
            state: ParseState::new(),
        }
    }

    pub fn messages<T: Str>(&self, errors: &[(NodeId, T)]) {
        let mut full_msg = String::new();
        for error in errors.iter() {
            let nid = error.0;
            let ref msg = error.1;
            full_msg.push_str(msg.as_slice());
            full_msg.push('\n');
            let fname = self.interner.name_to_str(&self.state.filename_of(&nid));
            full_msg.push_str(
                format!("   {:?}: {:?}\n", fname, self.state.span_of(&nid)).as_slice());
        }

        let _ = io::stderr().write_str(full_msg.as_slice());
    }

    pub fn message<T: Str>(&self, nid: NodeId, msg: T) {
        self.messages(&[(nid, msg)]);
    }

    pub fn errors_fatal<T: Str>(&self, errors: &[(NodeId, T)]) -> ! {
        self.messages(errors);
        let _ = io::stderr().write_str("\n");
        panic!("Aborting")
    }
    pub fn error_fatal<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        self.errors_fatal(&[(nid, msg)]);
    }

    // For now everything is fatal.
    pub fn error<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        self.error_fatal(nid, msg);
    }

    pub fn bug_span<T: Str>(&self, nid: NodeId, msg: T) -> ! {
        let sp = self.state.span_of(&nid);
        panic!("\nBum bum bum budda bum bum tsch:\n\
              Internal Compiler Error{:?}\n\
              at: {:?}\n", msg.as_slice(), sp);
    }
}
