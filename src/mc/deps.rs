use std::path::Path;
use std::io::File;

use package::Package;

// Should live somewhere else.
pub fn output_deps(package: &Package, target: &String) {
    let set = package.session.parser.get_all_filenames();
    let vec: Vec<String> =
        set.iter().
          map(|name| format!("{}", package.session.interner.name_to_str(name))).
          filter(|name| name.as_slice().char_at(0) != '<').collect();

    let mut dep_path = Path::new(target.clone());
    dep_path.set_extension("dep");
    let mut file = match File::create(&dep_path) {
        Ok(f) => f,
        Err(e) => panic!("Failed to open dependency file: {}", e),
    };

    match writeln!(&file, "{}: {}\n{}", target, vec.connect(" "), vec.connect(":\n") + ":") {
        Ok(()) => (), // succeeded
        Err(e) => panic!("Failed to generate dependency file: {}", e),
    }
}
