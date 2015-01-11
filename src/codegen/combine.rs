/// Take a bunch of functions, already as machine code, and the corresponding
/// labels, and "link" them.

use mas::ast::*;
use collections::BTreeMap;
use mas::labels::resolve_labels;

pub fn link(parts: Vec<(Vec<[InstNode; 4]>, BTreeMap<String, uint>)>
            ) -> (Vec<[InstNode; 4]>, BTreeMap<String, uint>) {
    let mut result = vec!();
    let mut pos = 0;
    let mut all_labels = BTreeMap::new();

    for (insts, labels) in parts.move_iter() {
        let this_len = insts.len();
        result.push_all_move(insts);
        for (label, label_pos) in labels.move_iter() {
            // For the assert; up here because "label" is moved.
            let error_str = format!("Duplicate label {}", label);
            let res = all_labels.insert(label, pos + label_pos);
            assert!(res, error_str);
        }
        pos += this_len;
    }

    (result, all_labels)
}
