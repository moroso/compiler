use package::Package;
use target::{MkTarget, Target, CTarget, IRTarget, AsmTarget};

use self::session::Options;

use getopts;
use getopts::Options as Getopts;
use std::io::{BufReader, Write};
use std::fs::File;

use std::io;
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
    fn new(_: &[(String, Option<String>)]) -> Box<NullTarget> { Box::new(NullTarget) }
}
impl Target for NullTarget {
    fn compile(&self, _: Package, _: &mut dyn Write) { }
}

fn package_from_stdin<'a>(opts: Options) -> Package<'a> {
    Package::from_buffer(opts, "<stdin>", BufReader::new(io::stdin()))
}

fn new_target<T: MkTarget>(args: &[(String, Option<String>)]) -> Box<T> {
    MkTarget::new(args)
}

macro_rules! targets {
    ($($n:expr => $t:ty),*) => (
        vec!($(($n, Box::new(|args: &Vec<(String, Option<String>)>| {
            new_target::<$t>(args) as Box<dyn Target>
        }) as Box<dyn Fn(&Vec<(String, Option<String>)>) -> Box<dyn Target>> )),*)
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
            let prelude_ma_location = install_path.join(Path::new("lib/prelude.ma"));
            opts.search_paths.insert("prelude.ma".to_string(), prelude_ma_location);
            let prelude_bsld_ma_location = install_path.join(Path::new("lib/prelude_bsld.ma"));
            opts.search_paths.insert("prelude_bsld.ma".to_string(), prelude_bsld_ma_location);
        }
    }
}

fn parse_search_paths(opts: &mut Options, matches: &getopts::Matches) -> bool {
    // Pull libraries out of the command line
    for string in matches.opt_strs("lib") {
        let parts: Vec<&str> = string.split(':').collect();
        if parts.len() != 2 { return false }
        let (module, file) = (parts[0], parts[1]);
        opts.search_paths.insert(module.to_string(), Path::new(file).to_path_buf());
    }

    true
}

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let arg0 = &args[0];

    let mut argopts = Getopts::new();

    argopts.optopt("", "target", "Set the output target format.", "[c|null|asm|ir]");
    argopts.optopt("o", "output", "Output file", "<filename>");
    argopts.optopt("", "list", "List file (asm target only)", "<filename>");
    argopts.optopt("", "debug", "Debug file (asm target only)", "<filename>");
    argopts.optopt("f", "format", "Output file format (asm target only)", "[flat|bsld]");
    argopts.optopt("", "stack_start", "Address in hex of the start of the stack (asm target only)",
                          "<hex value, no 0x>");
    argopts.optopt("", "global_start", "Address in hex of the start of global vars (asm target only)",
                          "<hex value, no 0x>");
    argopts.optopt("", "code_start", "Address in hex of the start of the code (asm target only)",
                          "<hex value, no 0x>");
    argopts.optopt("", "mul_func", "Name of function to use for software multiplication (asm target only)",
                          "<mangled function name>");
    argopts.optopt("", "div_func", "Name of function to use for software division (asm target only)",
                          "<mangled function name>");
    argopts.optopt("", "mod_func", "Name of function to use for software modulo (asm target only)",
                          "<mangled function name>");
    argopts.optflag("d", "dep-files", "Generate dependency files");
    argopts.optflag("v", "verbose", "Enable verbose output.");
    argopts.optflag("", "disable_scheduler", "Disable instruction scheduler (asm target only)");
    argopts.optflag("", "disable_inliner", "Disable function inlining (asm target only)");
    argopts.optflag("", "no_prelude", "Omit the prelude.");
    argopts.optflag("h", "help", "Show this help message.");
    argopts.optmulti("l", "lib", "Specify a library location", "<foo:/path/to/foo.mb>");

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

    let mut opts = vec!();

    for opt in &["verbose", "disable_scheduler", "disable_inliner"] {
        let val = matches.opt_present(opt);
        if val {
            opts.push((opt.to_string(), None));
        }
    }

    for opt in &["list", "format", "code_start", "stack_start", "global_start",
                    "debug", "mul_func", "div_func", "mod_func"] {
        let val = matches.opt_str(opt);
        if val.is_some() {
            opts.push((opt.to_string(), val));
        }
    }

    let target_arg = matches.opt_str("target").unwrap_or_else(|| "null".to_string());
    let target = match targets.into_iter()
                        .filter(|&(t, _)| t.eq_ignore_ascii_case(&target_arg[..]))
                        .map(|(_, ctor)| ctor(&opts))
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
    if matches.opt_present("no_prelude") {
        options.include_prelude = false;
    }

    let name = if matches.free.is_empty() {
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
        None => Box::new(io::stdout()) as Box<dyn Write>,
        Some(name) => {
            let path = Path::new(&name);
            let file = File::create(&path).unwrap_or_else(|e| panic!("{}", e));
            Box::new(file) as Box<dyn Write>
        }
    };

    if matches.opt_present("dep-files") {
        let target = matches.opt_str("output").unwrap_or_else(|| "-".to_string());
        deps::output_deps(&package, &target);
    }

    target.compile(package, &mut *writer);
}

#[cfg(test)]
mod tests {
    use package::Package;
    use super::{NullTarget};
    use target::{Target, MkTarget};

    use std::io;
    use std::path::PathBuf;

    fn package_from_str(s: &str) -> Package {
        let bytes = s.as_bytes();
        let buffer = io::BufReader::new(bytes);
        let mut opts = super::session::Options::new();
        let prelude_location = PathBuf::from("lib/prelude.mb");
        opts.search_paths.insert("prelude".to_string(), prelude_location);
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
        NullTarget.compile(package, &mut io::stdout() as &mut dyn io::Write);
    }
}
