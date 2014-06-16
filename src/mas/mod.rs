use getopts::{getopts, reqopt, optopt, optflag};
use getopts::OptionMissing;
use std::ascii::StrAsciiExt;

use std::io::stdio;
use std::os;

use self::lexer::{Token, new_asm_lexer};

pub mod lexer;
pub mod parser;
pub mod ast;

pub fn main() {
    // TODO: option parsing.

    let lexer = new_asm_lexer("<stdin>", stdio::stdin());
    let mut peekable = lexer.peekable();
    for token in peekable {
        print!("{}\n", token);
    }
}
