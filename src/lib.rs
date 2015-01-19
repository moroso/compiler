#![feature(plugin)]
#![allow(unstable)]

#[plugin] #[no_link]
extern crate regex_macros;
extern crate regex;

pub mod intern;
#[macro_use]
pub mod lexer;
pub mod span;
pub mod util;
