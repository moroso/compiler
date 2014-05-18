#![feature(globs,phase,macro_rules)]
#![allow(dead_code,unused_imports)]

/// TEMPORARY FILE.
/// This is for testing out/playing with the intermediate representation
/// stuff. It should be deleted when the IR code gets to the point where
/// this is no longer needed.

#[phase(syntax)]
extern crate regex_macros;

extern crate collections;
extern crate regex;

use std::io;

use ast::visit::Visitor;

use std::io::stdio;

use intermediate::*;
use intermediate::ast_to_intermediate::*;
use intermediate::constant_fold::*;
use intermediate::liveness::*;
use ast::{PlusOp, TimesOp};

use std::io::stdio::stdin;
use lexer::Lexer;
use parser::Parser;

mod lexer;
mod parser;
mod span;
mod ast;
mod intermediate;
mod values;

fn main() {
    let buffer = io::BufferedReader::new(io::MemReader::new(
        Vec::from_slice("{*(a*5+6*7)=4u16*(5u16+1u16)*foo; b=a+1; c=6+7<2 || a}".as_bytes())
        ));
    let mut parser = Parser::new();
    let lexer = Lexer::new("<stdin>".to_owned(), buffer);

    let ast = parser.parse_with(lexer, |p| p.parse_expr());
    let mut conv = ASTToIntermediate::new();
    let (ops, var) = conv.convert_expr(&ast);

    print!("{}\n", ast);
    print!("{}\n", (&ops, var));

    let mut folder = ConstantFolder::new(box ops);
    folder.constant_fold();
    print!("{}\n", folder.ops);

    let mut liveness = LivenessAnalyzer::new(folder.ops);
    liveness.seed();
    print!("{}\n", liveness.ops);
    print!("{}\n", liveness.opinfo);
    liveness.propagate();
    for z in liveness.opinfo.iter().zip(liveness.ops.iter()) {
        print!("{}\n", z);
    }
}
