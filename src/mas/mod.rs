use std::{process, io, env};
use std::path::Path;
use std::fs::File;
use std::collections::BTreeMap;
use std::iter::FromIterator;

use mas::labels::LabelInfo;

use self::lexer::new_asm_lexer;
use self::parser::AsmParser;
use self::encoder::encode;

use getopts::Options as Getopts;
use std::io::{Write, BufReader};


pub mod lexer;
pub mod parser;
pub mod ast;
pub mod encoder;
pub mod util;
pub mod labels;
pub mod scheduler;

fn print_bin(n: u32, stream: &mut Write) {
    // Write in little-endian format.
    (stream.write_all(vec!(
        (n >>  0) as u8,
        (n >>  8) as u8,
        (n >> 16) as u8,
        (n >> 24) as u8,
        ).as_ref())).ok();
}

pub fn main() {
    // TODO: option parsing.
    let args: Vec<String> = env::args().collect();
    let arg0 = &args[0];

    let mut argopts = Getopts::new();

    argopts.optopt("f", "fmt", "Set the output format.", "[c|bin|internal]");
    argopts.optopt("", "format", "Alias for fmt", "[c|bin|internal]");
    argopts.optopt("o", "output", "Output file", "<filename>");
    argopts.optflag("h", "help", "Show this help message.");

    let bail = |error: Option<&str>| {
        let error = match error {
            Some(e) => {
                eprintln!("{}: fatal error: {}", arg0, e);
                1
            }
            None => 0,
        };

        let brief = format!("Usage: {} [OPTIONS]", arg0);
        eprintln!("{}", argopts.usage(&brief[..]));
        process::exit(error)
    };

    let matches = match argopts.parse(&args[1..]) {
        Ok(m) => m,
        Err(e) => return bail(Some(&format!("{}", e)[..])),
    };

    if matches.opt_present("help") {
        return bail(None);
    }

    let format_arg = match matches.opt_str("fmt") {
        Some(f) => {
            if matches.opt_str("format").is_some() {
                bail(Some("Cannot supply both --fmt and --format"));
            }
            f
        },
        None => matches.opt_str("format").unwrap_or(
            "internal".to_string())
    };

    if &format_arg[..] == "internal" {
        print!("Moroso assembler.\n");
    }

    let mut f = match matches.opt_str("output") {
        None => Box::new(io::stdout()) as Box<Write>,
        Some(name) => {
            let path = Path::new(&name);
            let file = File::create(&path).unwrap_or_else(|e| panic!("{}", e));
            Box::new(file) as Box<Write>
        }
    };

    let name = if matches.free.len() == 0 {
        "-"
    } else if matches.free.len() == 1 {
        &matches.free[0][..]
    } else {
        bail(Some("too many arguments"));
        panic!()
    };

    let (mut insts, labels) = if name == "-" {
        let peekable = new_asm_lexer("<stdin>", BufReader::new(io::stdin())).peekable();
        let mut parser = AsmParser::new(peekable);
        parser.parse_toplevel()
    } else {
        let path = Path::new(name);
        let file = File::open(&path).unwrap_or_else(|e| panic!("{}", e));
        let peekable = new_asm_lexer(name, BufReader::new(file)).peekable();
        let mut parser = AsmParser::new(peekable);
        parser.parse_toplevel()
    };

    let new_labels: BTreeMap<String, LabelInfo> = FromIterator::from_iter(
        labels.into_iter().map(|(k, v)| (k, LabelInfo::InstLabel(v))));

    labels::resolve_labels(&mut insts, &new_labels, 0);

    for packet in insts.iter() {
        match &format_arg[..] {
            "internal" => { write!(&mut *f, "{:?}\n", packet).ok(); },
            "c" => { write!(&mut *f, "0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x},\n",
                            encode(&packet[0]),
                            encode(&packet[1]),
                            encode(&packet[2]),
                            encode(&packet[3])).ok(); },
            "bin" => { print_bin(encode(&packet[0]), &mut *f);
                       print_bin(encode(&packet[1]), &mut *f);
                       print_bin(encode(&packet[2]), &mut *f);
                       print_bin(encode(&packet[3]), &mut *f);
            }
            _ => panic!()
        }
    }
}
