#![feature(plugin)]

#[plugin] #[no_link]
extern crate regex_macros;
extern crate regex;

#[macro_use]
pub mod lexer;
pub mod span;
pub mod util;
