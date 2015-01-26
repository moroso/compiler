use std::collections::HashMap;
use std::io;

use analysis::Analysis;
use session::Session;
use syntax::parser;
use syntax::expand::Expansion;

pub struct Options {
    pub search_paths: HashMap<String, Path>,
}

impl Options {
    pub fn new() -> Options {
        Options {
            search_paths: HashMap::new()
        }
    }
}

pub fn analyze(source: Path, options: Options) -> Analysis {
    let mut session = Session::new(options);
    let file = io::File::open(&source).unwrap();
    let module = parser::parse_file(&mut session, file);
    let expn = Expansion::run(&mut session, module);
    let analysis = Analysis::run(session, expn);

    analysis
}
