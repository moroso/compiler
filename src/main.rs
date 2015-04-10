#![feature(plugin,overloaded_calls,box_syntax,int_uint)]
// Unstable libraries
#![feature(rustc_private)] // do we really want to use this?
#![feature(old_io)] // TODO: should move away from this
#![feature(collections)] // hm
#![allow(dead_code,unused_imports)]

#![plugin(regex_macros)]
extern crate regex_macros;

extern crate collections;
extern crate getopts;
extern crate regex;
extern crate rustc;
extern crate syntax as rust_syntax;
extern crate time;

#[cfg(test)]
extern crate debug;

//#[cfg(mc)]
use mc as front;

#[cfg(mas)]
use mc as front;

#[macro_use]
mod allow_string;

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
