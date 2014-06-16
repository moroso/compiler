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

//use util::Lexer;
use assembler::asm_lexer::{AsmToken, new_asm_lexer};

//mod lexer;
mod span;
// TODO: we may want to try to eliminate these dependencies.
mod util;
mod assembler;

fn main() {
    // TODO: option parsing.

    let lexer = new_asm_lexer("<stdin>", stdio::stdin());
    let mut peekable = lexer.peekable();
    for token in peekable {
        print!("{}\n", token);
    }
}