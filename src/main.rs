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
use std::io::stdio;
use std::os;
use std::ascii::StrAsciiExt;

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

fn print_usage(arg0: &StrBuf) {
    println!(
"moroso compiler
Usage: {} --target=<target>

<target>: c",
    arg0);
}

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
    let opts = [
        reqopt("", "target", "Set the output target", "<target>"),
    ];

    let (arg0, args) : (_, Vec<StrBuf>) = {
        let mut iter = os::args().move_iter().map(|x| x.into_strbuf());
        let arg0 = iter.next().unwrap();
        (arg0, iter.collect())
    };

    let matches = match getopts(args.as_slice(), opts) {
        Ok(m) => m, 
        Err(e) => fail!(e.to_err_msg()),
    };

    let targets = targets! {
        "c" => CTarget,
    };

    let target_opt = matches.opt_str("target").unwrap();
    let target = match targets.move_iter().filter(|t| t.ref0().eq_ignore_ascii_case(target_opt.as_slice())).next() {
        Some((_, ctor)) => Ok(ctor(matches.free)),
        None => Err(target_opt),
    };

    match target {
        Ok(target) => {
            let p = package_from_stdin();
            target.compile(p);
        }
        Err(t) => {
            println!("Bad target `{}'", t);
            os::set_exit_status(1);
            return print_usage(&arg0);
        }
    }
}

#[cfg(ir_tests)]
fn main() {
    mod intermediate_tests;
    intermediate_tests::main();
}
