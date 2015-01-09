#![feature(unboxed_closures,plugin)]

#[plugin] #[no_link]
extern crate regex_macros;
extern crate regex;
extern crate "syntax" as rust_syntax;

#[macro_use]
extern crate mclib;

pub mod syntax;

fn main() {
    let mut lexer = syntax::lexer::new_mb_lexer("test", std::io::stdio::stdin());
    for t in lexer {
        println!("{}", t);
    }
}
