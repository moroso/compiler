use mas::ast::*;
use std::collections::BTreeMap;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum LabelInfo {
    InstLabel(usize), // Offset from code start, in packets.
    ByteLabel(usize), // Absolute offset, in bytes.
}

fn addr_of(label_info: LabelInfo, offset: usize) -> u32 {
    match label_info {
        LabelInfo::InstLabel(pos) => (pos * 16 + offset) as u32,
        LabelInfo::ByteLabel(pos) => pos as u32,
    }
}

fn subst_label(target: &mut JumpTarget, idx: usize,
               labels: &BTreeMap<String, LabelInfo>) {
    let new_target = match *target {
        JumpOffs(..) => target.clone(),
        JumpLabel(ref name) => {
            let label_info = match labels.get(name) {
                Some(label_info) => *label_info,
                _ => panic!("Unresolved label {}", name),
            };
            let addr = addr_of(label_info, 0);
            if addr & 0xf != 0 {
                panic!("Unaligned label {}", name);
            }
            JumpOffs(((addr / 16) as i32) - (idx as i32))
        }
    };
    *target = new_target;
}

// TODO: eliminate this code duplication.
fn subst_label_long(target: &mut LongValue,
                    labels: &BTreeMap<String, LabelInfo>,
                    offset: usize) {
    let new_target = match *target {
        Immediate(..) => target.clone(),
        LabelOffs(ref name) => {
            let label_info = match labels.get(name) {
                Some(label_info) => *label_info,
                _ => panic!("Unresolved label {}", name),
            };
            Immediate(addr_of(label_info, offset))
        }
    };
    *target = new_target;
}

pub fn resolve_labels(insts: &mut Vec<InstPacket>,
                      labels: &BTreeMap<String, LabelInfo>,
                      offset: usize) {
    for (count, ref mut packet) in insts.iter_mut().enumerate() {
        for inst in packet.iter_mut() {
            match *inst {
                BranchImmInst(_, _, ref mut target) => {
                    subst_label(target, count, labels);
                },
                LongInst(ref mut target) => {
                    subst_label_long(target, labels, offset);
                },
                _ => {}
            }
        }
    }
}
