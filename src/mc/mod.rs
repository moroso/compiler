use package::Package;
use target::{Target, CTarget, IRTarget, AsmTarget};
use typechecker::Typechecker;

use self::ast::visit::Visitor;
use self::session::Session;

use getopts;
use getopts::{getopts, reqopt, optopt, optflag};
use getopts::OptionMissing;
use std::ascii::StrAsciiExt;
use std::io::{BufferedReader, File, stdio};

use std::os;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod session;
pub mod resolver;

struct NullTarget;
impl Target for NullTarget {
    fn new(_: Vec<String>) -> NullTarget { NullTarget }
    fn compile(&self, _: Package) { }
}

fn package_from_stdin() -> Package {
    Package::new("<stdin>", stdio::stdin())
}

fn new_target<T: Target>(args: Vec<String>) -> T {
    Target::new(args)
}

macro_rules! targets {
    ($($n:expr => $t:ty),*) => (
        vec!($(($n, |args| { box new_target::<$t>(args) as Box<Target> })),*)
    );
    ($($n:expr => $t:ty),+,) => (targets!($($n => $t),+))
}

pub fn main() {
    let args = os::args();
    let arg0 = args.get(0);

    let opts = [
        optopt("", "target", "Set the output target.", "[c|null]"),
        optflag("h", "help", "Show this help message."),
    ];

    let bail = |error: Option<&str>| {
        match error {
            Some(e) => {
                os::set_exit_status(1);
                println!("{}: fatal error: {}", arg0, e);
            }
            None => {}
        }

        let brief = format!("Usage: {} [OPTIONS]", arg0);
        println!("{}", getopts::usage(brief.as_slice(), opts));
    };

    let matches = match getopts(args.tail(), opts) {
        Ok(m) => m,
        Err(e) => return bail(Some(format!("{}", e).as_slice())),
    };

    if matches.opt_present("help") {
        return bail(None);
    }

    let targets = targets! {
        "c" => CTarget,
        "ir" => IRTarget,
        "null" => NullTarget,
        "asm" => AsmTarget,
    };

    let target_arg = matches.opt_str("target").unwrap_or(String::from_str("null"));
    let target = match targets.move_iter()
                        .filter(|&(ref t, _)| t.eq_ignore_ascii_case(target_arg.as_slice()))
                        .map(|(_, ctor)| ctor(vec!()))
                        .next() {
        Some(t) => t,
        None => {
            let msg = format!("Unrecognized target `{}'", target_arg);
            return bail(Some(msg.as_slice()));
        }
    };

    let name = if matches.free.len() == 0 {
        "-"
    } else if matches.free.len() == 1 {
        matches.free.get(0).as_slice()
    } else {
        return bail(Some("too many arguments"))
    };

    let package = if name == "-" {
        package_from_stdin()
    } else {
        let path = Path::new(name);
        let file = File::open(&path).unwrap_or_else(|e| fail!("{}", e));
        let buffer = BufferedReader::new(file);
        Package::new(name, buffer)
    };

    target.compile(package);
}

#[cfg(test)]
mod tests {
    use package::Package;
    use super::NullTarget;
    use target::Target;

    fn package_from_str(s: &str) -> Package {
        use std::str::StrSlice;
        use std::io;
        let bytes = Vec::from_slice(s.as_bytes());
        let buffer = io::BufferedReader::new(io::MemReader::new(bytes));
        Package::new("<input>", buffer)
    }

    #[test]
    fn exercise() {
        let src = r"
mod prelude {
    enum Option<T> {
        Some(T),
        None,
    }
}

struct Foo<T> {
    foo: T,
}

fn main() {
    let wot = Foo { foo: 42 };
    let more_wot = prelude::Some(wot);
    let foo: u32 = match more_wot {
        ::prelude::Some(wot) => {
            match wot {
                Foo{ foo: foo } => foo,
            }
        }
    };
}
";
        let package = package_from_str(src);
        NullTarget.compile(package);
    }
}