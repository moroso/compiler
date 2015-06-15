use package::Package;
use target::{MkTarget, Target, CTarget, IRTarget, AsmTarget};

use self::ast::visitor::Visitor;
use self::session::{Session, Options};

use getopts;
use getopts::{getopts, reqopt, optopt, optflag, optmulti};
use std::io::{BufReader, Write};
use std::fs::File;
use std::ascii::AsciiExt;

use std::io;
use std::os;
use std::env;
use std::path::Path;
use std::process;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod session;
pub mod resolver;
pub mod deps;

struct NullTarget;
impl MkTarget for NullTarget {
    fn new(_: Vec<String>) -> Box<NullTarget> { Box::new(NullTarget) }
}
impl Target for NullTarget {
    fn compile(&self, _: Package, _: &mut Write) { }
}

fn package_from_stdin<'a>(opts: Options) -> Package<'a> {
    Package::from_buffer(opts, "<stdin>", BufReader::new(io::stdin()))
}

fn new_target<T: MkTarget>(args: Vec<String>) -> Box<T> {
    MkTarget::new(args)
}

macro_rules! targets {
    ($($n:expr => $t:ty),*) => (
        vec!($(($n, Box::new(|args| { new_target::<$t>(args) as Box<Target> }) as Box<Fn(Vec<String>) -> Box<Target>> )),*)
    );
    ($($n:expr => $t:ty),+,) => (targets!($($n => $t),+))
}


pub fn setup_builtin_search_paths(opts: &mut Options) {
    // Unless it gets overridden, pull out a prelude based on the
    // install location of the binary. This is kind of dubious.
    match env::current_exe() {
        Err(_) => {}, /* whatever? */
        Ok(exe_path) => {
            let mut install_path = exe_path.parent().unwrap().to_path_buf();
            if install_path.parent().unwrap().ends_with("target") {
                // If we're installed somewhere like target/release or
                // target/debug, remove those components from the
                // path.
                install_path.pop();
                install_path.pop();
            }
            let prelude_location = install_path.join(Path::new("lib/prelude.mb"));
            opts.search_paths.insert("prelude".to_string(), prelude_location);
        }
    }
}

fn parse_search_paths(opts: &mut Options, matches: &getopts::Matches) -> bool {
    // Pull libraries out of the command line
    for string in matches.opt_strs("lib").into_iter() {
        let parts: Vec<&str> = string.split(":").collect();
        if parts.len() != 2 { return false }
        let (module, file) = (parts[0], parts[1]);
        opts.search_paths.insert(module.to_string(), Path::new(file).to_path_buf());
    }

    true
}

pub fn main() {
    let args: Vec<String> = env::args().collect();
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
        let error = match error {
            Some(e) => {
                println!("{}: fatal error: {}", arg0, e);
                1
            }
            None => 0,
        };

        let brief = format!("Usage: {} [OPTIONS]", arg0);
        println!("{}", getopts::usage(&brief[..], &opts));
        process::exit(error)
    };

    let matches = match getopts(args.tail(), &opts) {
        Ok(m) => m,
        Err(e) => bail(Some(&format!("{}", e)[..])),
    };

    if matches.opt_present("help") {
        bail(None);
    }

    let targets = targets! {
        "c" => CTarget,
        "ir" => IRTarget,
        "null" => NullTarget,
        "asm" => AsmTarget,
    };

    let verbose = matches.opt_present("verbose");

    let target_arg = matches.opt_str("target").unwrap_or("null".to_string());
    let target = match targets.into_iter()
                        .filter(|&(ref t, _)| t.eq_ignore_ascii_case(&target_arg[..]))
                        .map(|(_, ctor)| ctor(
                            if verbose {
                                vec!("verbose".to_string())
                            } else {
                                vec!()
                            }))
                        .next()
    {
        Some(t) => t,
        None => {
            let msg = format!("Unrecognized target `{}'", target_arg);
            bail(Some(&msg[..]));
            panic!()
        }
    };

    let mut options = Options::new();
    setup_builtin_search_paths(&mut options);
    if !parse_search_paths(&mut options, &matches) {
        bail(Some("Bogus library specification"));
    }

    let name = if matches.free.len() == 0 {
        "-"
    } else if matches.free.len() == 1 {
        &matches.free[0][..]
    } else {
        bail(Some("too many arguments"));
        panic!()
    };

    let package = if name == "-" {
        package_from_stdin(options)
    } else {
        let path = Path::new(name);
        Package::from_path(options, path)
    };

    // FIXME: Hm, maybe should use a MemWriter, gather the buffer,
    // then print it all at once and properly error check. Then we
    // don't truncate the file if it fails, either. (We *shouldn't*
    // fail during compile, but...)
    let mut writer = match matches.opt_str("output") {
        None => Box::new(io::stdout()) as Box<Write>,
        Some(name) => {
            let path = Path::new(&name);
            let file = File::create(&path).unwrap_or_else(|e| panic!("{}", e));
            Box::new(file) as Box<Write>
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
    use target::MkTarget;
    use std::old_io::stdio;

    fn package_from_str(s: &str) -> Package {
        use std::str::StrSlice;
        use std::old_io;
        let bytes = s.as_bytes().to_vec();
        let buffer = io::BufReader::new(old_io::MemReader::new(bytes));
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
        NullTarget.compile(package, &mut stdio::stdout() as &mut Write);
    }
}
