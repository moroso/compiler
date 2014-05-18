#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

#[phase(syntax)]
extern crate regex_macros;

extern crate collections;
extern crate regex;

use std::io::stdio::stdin;
use session::Session;

mod lexer;
mod parser;
mod span;
mod ast;
mod resolver;
mod session;
//mod typecheck;

fn main() {
    println!("moroso compiler");
    let mut session = Session::new();
    print!("{:}\n", session.parse_buffer(stdin()));
}
