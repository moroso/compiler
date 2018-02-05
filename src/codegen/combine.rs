/// Take a bunch of functions, already as machine code, and the corresponding
/// labels, and "link" them.

use mas::labels::LabelInfo;
use mas::ast::*;
use std::collections::BTreeMap;

pub fn link(parts: Vec<(Vec<[InstNode; 4]>, BTreeMap<String, usize>)>
            ) -> (Vec<[InstNode; 4]>, BTreeMap<String, LabelInfo>) {
    let mut result = vec!();
    let mut pos = 0;
    let mut all_labels = BTreeMap::new();

    for (insts, labels) in parts {
        let this_len = insts.len();
        result.extend(insts.into_iter());
        for (label, label_pos) in labels {
            // For the assert; up here because "label" is moved.
            let error_str = format!("Duplicate label {}", label);
            let res = all_labels.insert(label, LabelInfo::InstLabel(pos + label_pos));
            assert!(res.is_none(), error_str);
        }
        pos += this_len;
    }

    (result, all_labels)
}
