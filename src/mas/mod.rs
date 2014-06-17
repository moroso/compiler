use getopts::{getopts, reqopt, optopt, optflag};
use getopts::OptionMissing;
use std::ascii::StrAsciiExt;

use std::io::stdio;
use std::os;

use self::lexer::{Token, new_asm_lexer};
use self::parser::AsmParser;

pub mod lexer;
pub mod parser;
pub mod ast;

pub fn main() {
    // TODO: option parsing.

    let lexer = new_asm_lexer("<stdin>", stdio::stdin());
    let peekable = lexer.peekable();
    let mut parser = AsmParser::new(peekable);
    print!("{}\n", parser.parse_inst());
}
