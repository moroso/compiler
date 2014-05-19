#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

#[phase(syntax)]
extern crate regex_macros;

extern crate collections;
extern crate getopts;
extern crate regex;

use ast::visit::Visitor;
use package::Package;
use session::Session;
use target::{Target, CTarget};
use typechecker::Typechecker;

use collections::HashMap;
use getopts::{getopts, reqopt, optopt, optflag};
use getopts::OptionMissing;
use std::ascii::StrAsciiExt;

use std::io::stdio;
use std::os;

mod util;
mod lexer;
mod parser;
mod span;
mod ast;
mod resolver;
mod session;
mod typechecker;
mod package;
mod ir;
mod target;
mod values;

fn package_from_stdin() -> Package {
    Package::new("<stdin>", stdio::stdin())
}

fn new_target<T: Target>(args: Vec<StrBuf>) -> T {
    Target::new(args)
}

macro_rules! targets {
    ($($n:expr => $t:ty),*) => (
        vec!($(($n, |args| { box new_target::<$t>(args) as Box<Target> })),*)
    );
    ($($n:expr => $t:ty),+,) => (targets!($($n => $t),+))
}

#[cfg(not(ir_tests))]
fn main() {
    let (arg0, args) : (_, Vec<StrBuf>) = {
        let mut iter = os::args().move_iter().map(|x| x.into_strbuf());
        let arg0 = iter.next().unwrap();
        (arg0, iter.collect())
    };

    let opts = [
        optopt("", "target", "Set the output target.", "<target>"),
        optflag("h", "help", "Show this help message."),
    ];

    let bail = |error: Option<~str>| {
        match error {
            Some(e) => {
                os::set_exit_status(1);
                println!("{}: fatal error: {}", arg0, e);
            }
            None => {}
        }

        let brief =
            "Usage:" +
            format!("\n    {} --help", arg0) +
            format!("\n    {} --target <target>", arg0);

        println!("");
        println!("{}", getopts::usage(brief, opts));
        println!("    Valid values for <target> are: c");
    };

    let matches = match getopts(args.as_slice(), opts) {
        Ok(m) => m,
        Err(e) => return bail(Some(e.to_err_msg().into_owned())),
    };
    
    if matches.opt_present("help") {
        return bail(None);
    }

    // Require target for now
    //TODO: 'Validate' target as default; just parse, typecheck, and exit
    let target_arg = match matches.opt_str("target") {
        Some(t) => t,
        None => {
            let msg = OptionMissing("target".to_strbuf()).to_err_msg().into_owned();
            return bail(Some(msg));
        }
    };

    let targets = targets! {
        "c" => CTarget,
    };

    let target = match targets.move_iter()
                        .filter(|&(ref t, _)| t.eq_ignore_ascii_case(target_arg.as_slice()))
                        .map(|(_, ctor)| ctor(vec!()))
                        .next() {
        Some(t) => t,
        None => {
            let msg = format!("Unrecognized target `{}'", target_arg);
            return bail(Some(msg));
        }
    };

    let package = package_from_stdin();
    target.compile(package);
}

#[cfg(ir_tests)]
fn main() {
    mod intermediate_tests;
    intermediate_tests::main();
}
