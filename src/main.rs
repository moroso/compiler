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

struct NullTarget;
impl Target for NullTarget {
    fn new(_: Vec<String>) -> NullTarget { NullTarget }
    fn compile(&self, _: Package) { }
}

fn package_from_stdin() -> Package {
    Package::new("<stdin>", stdio::stdin())
}

fn new_target<T: Target>(args: Vec<String>) -> T {
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
    let args = os::args();
    let arg0 = args.get(0);

    let opts = [
        optopt("", "target", "Set the output target.", "[c|null]"),
        optflag("h", "help", "Show this help message."),
    ];

    let bail = |error: Option<&str>| {
        match error {
            Some(e) => {
                os::set_exit_status(1);
                println!("{}: fatal error: {}", arg0, e);
            }
            None => {}
        }

        let brief = format!("Usage: {} [OPTIONS]", arg0);
        println!("{}", getopts::usage(brief.as_slice(), opts));
    };

    let matches = match getopts(args.tail(), opts) {
        Ok(m) => m,
        Err(e) => return bail(Some(e.to_err_msg().as_slice())),
    };
    
    if matches.opt_present("help") {
        return bail(None);
    }

    let targets = targets! {
        "c" => CTarget,
        "null" => NullTarget,
    };

    let target_arg = matches.opt_str("target").unwrap_or(String::from_str("null"));
    let target = match targets.move_iter()
                        .filter(|&(ref t, _)| t.eq_ignore_ascii_case(target_arg.as_slice()))
                        .map(|(_, ctor)| ctor(vec!()))
                        .next() {
        Some(t) => t,
        None => {
            let msg = format!("Unrecognized target `{}'", target_arg);
            return bail(Some(msg.as_slice()));
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
