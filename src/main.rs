#![feature(globs,phase,macro_rules,overloaded_calls)]
#![allow(dead_code,unused_imports)]

#[phase(plugin)]
extern crate regex_macros;

extern crate collections;
extern crate getopts;
extern crate regex;

#[cfg(test)]
extern crate debug;

#[cfg(mc)]
use front = mc;

#[cfg(mas)]
use front = mas;

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
