#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

#[phase(syntax)]
extern crate regexp_macros;

extern crate collections;
extern crate regexp;

mod lexer;
mod parser;

fn main() {
    println!("moroso compiler");
}
