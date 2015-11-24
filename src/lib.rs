#![feature(plugin,box_syntax)]

// Unstable libraries
#![feature(rustc_private)] // do we really want to use this?
#![feature(str_escape)]
#![allow(dead_code,unused_imports)]

#![plugin(regex_macros)]

extern crate num;

extern crate getopts;
extern crate regex;
extern crate rustc_data_structures;
extern crate syntax as rust_syntax;
extern crate time;

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
