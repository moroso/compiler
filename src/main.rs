#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

#[phase(syntax)]
extern crate regex_macros;

extern crate collections;
extern crate regex;

use ast::visit::Visitor;
use session::Session;
use typechecker::Typechecker;

use std::io::stdio;

mod lexer;
mod parser;
mod span;
mod ast;
mod resolver;
mod session;
mod typechecker;

fn main() {
    println!("moroso compiler");

    let stdin = stdio::stdin();

    let mut session = Session::new();
    let tree = session.parse_buffer("<stdin>", stdin);

    let mut tyck = Typechecker::new(&session);
    tyck.visit_module(&tree);

    print!("{:}\n", tree);
}
