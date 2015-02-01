use std::old_io::stdio;
use std::os;

use self::lexer::{Token, new_asm_lexer};
use self::parser::AsmParser;
use self::encoder::encode;

use getopts;
use getopts::{getopts, reqopt, optopt, optflag};
use std::old_io::Writer;
use std::iter::IteratorExt;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod encoder;
pub mod util;
pub mod labels;
pub mod scheduler;

fn print_bin<T: Writer>(n: u32, stream: &mut T) {
    // Write in little-endian format.
    (stream.write(vec!(
        (n >>  0) as u8,
        (n >>  8) as u8,
        (n >> 16) as u8,
        (n >> 24) as u8,
        ).as_slice())).ok();
}

pub fn main() {
    // TODO: option parsing.
    let args = os::args();
    let arg0 = &args[0];

    let opts = [
        optopt("", "fmt", "Set the output format.", "[c|bin|internal]"),
        optflag("h", "help", "Show this help message."),
    ];

    let bail = |&: error: Option<&str>| {
        match error {
            Some(e) => {
                os::set_exit_status(1);
                println!("{}: fatal error: {}", arg0, e);
            }
            None => {}
        }

        let brief = format!("Usage: {} [OPTIONS]", arg0);
        println!("{}", getopts::usage(brief.as_slice(), &opts));
    };

    let matches = match getopts(args.tail(), &opts) {
        Ok(m) => m,
        Err(e) => return bail(Some(format!("{}", e).as_slice())),
    };

    if matches.opt_present("help") {
        return bail(None);
    }

    let format_arg = matches.opt_str("fmt").unwrap_or(
        String::from_str("internal"));

    if format_arg.as_slice() == "internal" {
        print!("Moroso assembler.\n");
    }

    let lexer = new_asm_lexer("<stdin>", stdio::stdin());
    let peekable = lexer.peekable();
    let mut parser = AsmParser::new(peekable);
    let (mut insts, labels) = parser.parse_toplevel();
    let mut stdout = stdio::stdout();

    labels::resolve_labels(&mut insts, &labels);

    for packet in insts.iter() {
        match format_arg.as_slice() {
            "internal" => print!("{}\n", packet),
            "c" => print!("0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x},\n",
                          encode(&packet[0]),
                          encode(&packet[1]),
                          encode(&packet[2]),
                          encode(&packet[3])),
            "bin" => { print_bin(encode(&packet[0]), &mut stdout);
                       print_bin(encode(&packet[1]), &mut stdout);
                       print_bin(encode(&packet[2]), &mut stdout);
                       print_bin(encode(&packet[3]), &mut stdout);
            }
            _ => panic!()
        }
    }
}
