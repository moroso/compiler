#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

#[phase(syntax)]
extern crate regexp_macros;

extern crate collections;
extern crate regexp;

use std::io::stdio::stdin;
use lexer::Lexer;
use parser::Parser;

mod lexer;
mod parser;

fn main() {
    println!("moroso compiler");
    let mut stdin = stdin();
    let lexer = Lexer::new(~stdin.lines().map(|x| x.unwrap()));
    let mut parser = Parser::new(lexer);

    print!("{:?}\n", parser.parse_expr());
}
