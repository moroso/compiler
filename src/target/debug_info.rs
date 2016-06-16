use span::Span;
use util::Name;
use mc::ast::NodeId;
use std::collections::{BTreeSet, BTreeMap, BinaryHeap};
use std::collections::btree_map;
use codegen::RegisterColor;
use ir::{IrNodeId, Op, OpInfo, Var};
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use target::util::print_bin;
use mas::labels::LabelInfo;

// TODO: remove this; we won't need it later.
use mas::ast::InstNode;

#[allow(unused_must_use)]
pub fn write_debug_file(
    f: &mut File,
    all_labels: &BTreeMap<String, LabelInfo>,
    spanmap: &BTreeMap<NodeId, Span>, // Maps *AST* node IDs to spans in Mb source
    filemap: &BTreeMap<NodeId, Name>, // Maps *AST* node IDs to Mb source files
    sourcemap: &BTreeMap<IrNodeId, NodeId>, // Maps IR node IDs to the AST node ID they came from
    func_debug_info: &BTreeMap<Name, // Maps function names to:
                               (BTreeMap<Var, RegisterColor>, // Variable -> register correspondence
                                Vec<Op>, // The entire IR of the function
                                Vec<OpInfo>, // Liveness information for each IR instruction
                                Vec<usize>, // Map from IR locations to ASM locations
                                Vec<InstNode>, // The ASM instructions themselves (for debug)
                               )>
) {
    let mut func_labels = BinaryHeap::<(isize, String, usize)>::new();
    for (name, label) in all_labels.iter() {
        // TODO: this is a hacky way of checking for internal labels.
        if !name.starts_with("LABEL") {
            match *label {
                LabelInfo::InstLabel(pos) => func_labels.push((-(pos as isize), name.clone(), pos)),
                _ => {}
            }
        }
    }

    let mut number_to_file = vec!();
    let mut file_to_number = BTreeMap::new();

    for filename in filemap.values() {
        match file_to_number.entry(filename) {
            btree_map::Entry::Vacant(v) => {
                v.insert(number_to_file.len());
                number_to_file.push(filename);
            },
            _ => {}
        }
    }

    for (idx, fname) in number_to_file.iter().enumerate() {
        print!("{}, {}\n", idx, fname);
    }

    // Dump some info to stdout for now.
    for (name, vals) in func_debug_info {
        print!("{}\n", name);

        let &(ref color_map, ref ops, ref opinfo, ref ir_to_asm_map, ref asm_insts) = vals;
        print!("{:?}\n", color_map);
        print!("{:?}\n", opinfo);

        let mut asm_pos = 0;
        let mut old_row = 0;
        for (ir_pos, op) in ops.iter().enumerate() {
            let orig_node_id = sourcemap.get(&op.id);
            let orig_span = orig_node_id.map(|x| spanmap.get(x).unwrap());
            let orig_file = orig_node_id.map(|x| filemap.get(x).unwrap());
            let orig_row = orig_span.map(|x| x.get_begin().row);
            if orig_file.is_some() {
                let orig_file = orig_file.unwrap();
                let orig_row = orig_row.unwrap();
                if orig_row != old_row {
                    let f = File::open(format!("{:?}", orig_file)).ok().unwrap();
                    let mut reader = BufReader::new(f);
                    let mut s = String::new();
                    for _ in 0..orig_row + 1 {
                        s.clear();
                        reader.read_line(&mut s);
                    }
                    print!("{}", s);
                }
                old_row = orig_row;
            }
            while asm_pos < ir_to_asm_map[ir_pos+1] && asm_pos < asm_insts.len() {
                print!("{}\n", asm_insts[asm_pos]);
                asm_pos += 1;
            }
        }
        while asm_pos < asm_insts.len() {
            print!("{}\n", asm_insts[asm_pos]);
            asm_pos += 1;
        }
    }

    // Magic
    write!(f, "MROD");

    // Label section
    write!(f, "LBEL");
    // Number of entries
    print_bin(func_labels.len() as u32, f);
    while !func_labels.is_empty() {
        let (_, name, pos) = func_labels.pop().unwrap();
        print_bin(pos as u32, f);
        print_bin(name.len() as u32 + 1, f);
        write!(f, "{}", name);
        f.write(&[0]);
    }
}
