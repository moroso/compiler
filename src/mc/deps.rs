use std::path::Path;
use std::fs::File;
use std::io::prelude::*;

use package::Package;

// Should live somewhere else.
pub fn output_deps(package: &Package, target: &String) {
    let set = package.session.parser.get_all_filenames();
    let vec: Vec<String> =
        set.iter().
          map(|name| format!("{}", package.session.interner.name_to_str(name))).
          filter(|name| (&name[..]).chars().nth(0).unwrap() != '<').collect();

    let mut dep_path = Path::new(target).to_path_buf();
    dep_path.set_extension("dep");
    let mut file = match File::create(&dep_path) {
        Ok(f) => f,
        Err(e) => panic!("Failed to open dependency file: {}", e),
    };

    match writeln!(&mut file, "{}: {}\n{}", target, vec.connect(" "), vec.connect(":\n") + ":") {
        Ok(()) => (), // succeeded
        Err(e) => panic!("Failed to generate dependency file: {}", e),
    }
}
