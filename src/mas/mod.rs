use getopts::{getopts, reqopt, optopt, optflag};
use getopts::OptionMissing;
use std::ascii::StrAsciiExt;

use std::io::stdio;
use std::os;

use self::lexer::{Token, new_asm_lexer};
use self::parser::AsmParser;
use self::encoder::encode;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod encoder;
pub mod util;

pub fn main() {
    // TODO: option parsing.

    let lexer = new_asm_lexer("<stdin>", stdio::stdin());
    let peekable = lexer.peekable();
    let mut parser = AsmParser::new(peekable);
    loop {
        let packet = parser.parse_inst_packet();
        print!("{}\n", packet);
        print!("{:08x} {:08x} {:08x} {:08x}\n",
               encode(&packet[0]),
               encode(&packet[1]),
               encode(&packet[2]),
               encode(&packet[3]));
    }
}
