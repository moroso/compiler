use mas::ast::*;
use std::collections::TreeMap;

fn subst_label(target: &mut JumpTarget, idx: uint,
               labels: &TreeMap<String, uint>) {
    let new_target = match *target {
        JumpOffs(..) => target.clone(),
        JumpLabel(ref name) => {
            let label_idx = match labels.find(name) {
                Some(pos) => *pos,
                _ => fail!("Unresolved label {}", name),
            };
            JumpOffs((label_idx as i32) - (idx as i32))
        }
    };
    *target = new_target;
}

// TODO: eliminate this code duplication.
fn subst_label_long(target: &mut LongValue,
               labels: &TreeMap<String, uint>) {
    let new_target = match *target {
        Immediate(..) => target.clone(),
        LabelOffs(ref name) => {
            let label_idx = match labels.find(name) {
                Some(pos) => *pos,
                _ => fail!("Unresolved label {}", name),
            };
            // The offset here is in bytes, not packets, so we multiply by 16.
            Immediate((label_idx * 16) as u32)
        }
    };
    *target = new_target;
}

pub fn resolve_labels(insts: &mut Vec<InstPacket>,
                      labels: &TreeMap<String, uint>) {
    for (count, ref mut packet) in insts.mut_iter().enumerate() {
        for inst in packet.mut_iter() {
            match *inst {
                BranchImmInst(_, _, ref mut target) => {
                    subst_label(target, count, labels);
                },
                LongInst(ref mut target) => {
                    subst_label_long(target, labels);
                },
                _ => {}
            }
        }
    }
}