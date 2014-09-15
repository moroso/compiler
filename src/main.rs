#![feature(globs,phase,macro_rules,overloaded_calls,import_shadowing)]
#![allow(dead_code,unused_imports)]

#[phase(plugin)]
extern crate regex_macros;

extern crate collections;
extern crate getopts;
extern crate regex;
extern crate rustc;
extern crate "syntax" as rust_syntax;
extern crate time;

#[cfg(test)]
extern crate debug;

#[cfg(mc)]
use mc as front;

#[cfg(mas)]
use mc as front;

mod util;
mod typechecker;
mod intrinsics;
mod package;
mod ir;
mod span;
mod target;
mod values;
mod codegen;

mod mc;
mod mas;

#[cfg(not(test))]
fn main() { front::main() }
