#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

#[phase(plugin)]
extern crate regex_macros;

extern crate collections;
extern crate getopts;
extern crate regex;

#[cfg(test)]
extern crate debug;

use getopts::{getopts, reqopt, optopt, optflag};
use getopts::OptionMissing;
use std::ascii::StrAsciiExt;

use std::io::stdio;
use std::os;

use lexer::Lexer;

mod lexer;
mod span;
// TODO: we may want to try to eliminate these dependencies.
mod ast;
mod util;
mod assembler;

fn main() {
    // TODO: option parsing.

    let lexer = Lexer::new("<stdin>", stdio::stdin());
}