#![allow(dead_code)]

extern crate num;

extern crate getopts;
extern crate regex;

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
