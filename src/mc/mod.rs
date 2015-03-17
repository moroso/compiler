use package::Package;
use target::{Target, CTarget, IRTarget, AsmTarget};

use self::ast::visitor::Visitor;
use self::session::{Session, Options};

use getopts;
use getopts::{getopts, reqopt, optopt, optflag, optmulti};
use std::old_io::{BufferedReader, File, Writer, stdio};
use std::ascii::AsciiExt;

use std::os;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod session;
pub mod resolver;
pub mod deps;

struct NullTarget;
impl Target for NullTarget {
    fn new(_: Vec<String>) -> Box<NullTarget> { Box::new(NullTarget) }
    fn compile(&self, _: Package, _: &mut Writer) { }
}

fn package_from_stdin<'a>(opts: Options) -> Package<'a> {
    Package::from_buffer(opts, "<stdin>", stdio::stdin())
}

fn new_target<T: Target>(args: Vec<String>) -> Box<T> {
    Target::new(args)
}

macro_rules! targets {
    ($($n:expr => $t:ty),*) => (
        vec!($(($n, Box::new(|&:args| { new_target::<$t>(args) as Box<Target> }) as Box<Fn(Vec<String>) -> Box<Target>> )),*)
    );
    ($($n:expr => $t:ty),+,) => (targets!($($n => $t),+))
}


pub fn setup_builtin_search_paths(opts: &mut Options) {
    // Unless it gets overridden, pull out a prelude based on the
    // install location of the binary. This is kind of dubious.
    match os::self_exe_path() {
        None => {}, /* whatever? */
        Some(exe_path) => {
            let prelude_location = exe_path.join(Path::new("lib/prelude.mb"));
            opts.search_paths.insert("prelude".to_string(), prelude_location);
        }
    }
}

fn parse_search_paths(opts: &mut Options, matches: &getopts::Matches) -> bool {
    // Pull libraries out of the command line
    for string in matches.opt_strs("lib").into_iter() {
        let parts: Vec<&str> = string.as_slice().split_str(":").collect();
        let (module, file) = match parts.as_slice() {
            [ module, file ] => (module, file),
            _ => { return false; }
        };
        opts.search_paths.insert(String::from_str(module), Path::new(file));
    }

    true
}

pub fn main() {
    let args = os::args();
    let arg0 = &args[0];

    let opts = [
        optopt("", "target", "Set the output target format.", "[c|null|asm|ir]"),
        optopt("o", "output", "Output file", "<filename>"),
        optflag("d", "dep-files", "Generate dependency files"),
        optflag("v", "verbose", "Enable verbose output."),
        optflag("h", "help", "Show this help message."),
        optmulti("l", "lib", "Specify a library location", "<foo:/path/to/foo.mb>"),
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
        println!("{}", getopts::usage(brief.as_slice(), &opts));
    };

    let matches = match getopts(args.tail(), &opts) {
        Ok(m) => m,
        Err(e) => return bail(Some(format!("{}", e).as_slice())),
    };

    if matches.opt_present("help") {
        return bail(None);
    }

    // TODO: make this work again.
    /*
    let targets = targets! {
        "c" => CTarget,
        "ir" => IRTarget,
        "null" => NullTarget,
        "asm" => AsmTarget,
    };

    let verbose = matches.opt_present("verbose");

    let target_arg = matches.opt_str("target").unwrap_or("null".to_string());
    let target = match targets.into_iter()
                        .filter(|&(ref t, _)| t.eq_ignore_ascii_case(target_arg.as_slice()))
                        .map(|(_, ctor)| ctor(
                            if verbose {
                                vec!("verbose".to_string())
                            } else {
                                vec!()
                            }))
                        .next() {
        Some(t) => t,
        None => {
            let msg = format!("Unrecognized target `{}'", target_arg);
            return bail(Some(msg.as_slice()));
        }
                        };*/
    // TODO!!!!
    let target = new_target::<CTarget>(vec!());

    let mut options = Options::new();
    setup_builtin_search_paths(&mut options);
    if !parse_search_paths(&mut options, &matches) {
        bail(Some("Bogus library specification"));
    }

    let name = if matches.free.len() == 0 {
        "-"
    } else if matches.free.len() == 1 {
        matches.free[0].as_slice()
    } else {
        return bail(Some("too many arguments"))
    };

    let package = if name == "-" {
        package_from_stdin(options)
    } else {
        let path = Path::new(name);
        let file = File::open(&path).unwrap_or_else(|e| panic!("{}", e));
        Package::from_file(options, file)
    };

    // FIXME: Hm, maybe should use a MemWriter, gather the buffer,
    // then print it all at once and properly error check. Then we
    // don't truncate the file if it fails, either. (We *shouldn't*
    // fail during compile, but...)
    let mut writer = match matches.opt_str("output") {
        None => Box::new(stdio::stdout()) as Box<Writer>,
        Some(name) => {
            let path = Path::new(name);
            let file = File::create(&path).unwrap_or_else(|e| panic!("{}", e));
            Box::new(file) as Box<Writer>
        }
    };

    if matches.opt_present("dep-files") {
        let target = matches.opt_str("output").unwrap_or(format!("-"));
        deps::output_deps(&package, &target);
    }

    target.compile(package, &mut *writer);
}

#[cfg(test)]
mod tests {
    use package::Package;
    use super::{NullTarget, setup_builtin_search_paths};
    use target::Target;
    use std::old_io::stdio;

    fn package_from_str(s: &str) -> Package {
        use std::str::StrSlice;
        use std::old_io;
        let bytes = s.as_bytes().to_vec();
        let buffer = old_io::BufferedReader::new(old_io::MemReader::new(bytes));
        let mut opts = super::session::Options::new();
        setup_builtin_search_paths(&mut opts);
        Package::from_buffer(opts, "<input>", buffer)
    }

    #[test]
    fn exercise() {
        let src = r"
mod option {
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
    let more_wot = option::Some(wot);
    let foo: u32 = match more_wot {
        ::option::Some(wot) => {
            let Foo { foo: foo } = wot;
            foo
        },
        _ => 1337
    };
}
";
        let package = package_from_str(src);
        NullTarget.compile(package, &mut stdio::stdout() as &mut Writer);
    }
}
