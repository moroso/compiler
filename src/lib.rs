#![feature(plugin,box_syntax)]

// Unstable libraries
#![feature(path_ext)]
#![feature(rustc_private)] // do we really want to use this?
#![feature(collections)] // hm
#![feature(custom_derive)]
#![allow(dead_code,unused_imports)]

#![plugin(regex_macros)]

extern crate num;

extern crate collections;
extern crate getopts;
extern crate regex;
extern crate rustc_data_structures;
extern crate syntax as rust_syntax;
extern crate time;

#[cfg(test)]
extern crate debug;

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

pub mod mc;
pub mod mas;
