use mas::ast::*;
use mas::parser::{classify_inst, InstType};
use std::collections::{BTreeSet, BTreeMap};
use std::iter::FromIterator;

// Return Rd.
fn destreg(inst: &InstNode) -> Option<Reg> {
    match *inst {
        ALU1ShortInst(_, _, r, _, _) |
        ALU2ShortInst(_, _, r, _, _, _) |
        ALU1RegInst(_, _, r, _, _, _) |
        ALU2RegInst(_, _, r, _, _, _, _) |
        ALU2LongInst(_, _, r, _) |
        ALU1LongInst(_, _, r) |
        ALU1RegShInst(_, r, _, _, _, _) |
        LoadInst(_, _, r, _, _) |
        MfcInst(_, r, _) |
        MfhiInst(_, r) |
        MultInst(_, _, r, _, _) |
        DivInst(_, _, _, r, _, _) => Some(r),
        _ => None,
    }
}

fn pred(inst: &InstNode) -> Option<Pred> {
    match *inst {
        ALU1ShortInst(p, _, _, _, _) |
        ALU2ShortInst(p, _, _, _, _, _) |
        ALU1RegInst(p, _, _, _, _, _) |
        ALU2RegInst(p, _, _, _, _, _, _) |
        ALU2LongInst(p, _, _, _) |
        ALU1LongInst(p, _, _) |
        ALU1RegShInst(p, _, _, _, _, _) |
        LoadInst(p, _, _, _, _) |
        MfcInst(p, _, _) |
        MfhiInst(p, _) |
        MultInst(p, _, _, _, _) |
        DivInst(p, _, _, _, _, _) |
        StoreInst(p, _, _, _, _) |
        CompareShortInst(p, _, _, _, _, _) |
        CompareRegInst(p, _, _, _, _, _, _) |
        CompareLongInst(p, _, _, _) |
        BranchImmInst(p, _, _) |
        BranchRegInst(p, _, _, _) |
        BreakInst(p, _) |
        SyscallInst(p, _) |
        MtcInst(p, _, _) |
        EretInst(p) |
        FenceInst(p) |
        MthiInst(p, _) |
        FlushInst(p, _, _) => Some(p),
        NopInst |
        LongInst(..)  => None,
        PacketsInst(..) => panic!("Attempting to determine predicate of Packets pseudo-instruction"),
    }
}

fn destpred(inst: &InstNode) -> Option<Pred> {
    match *inst {
        CompareShortInst(_, p, _, _, _, _) |
        CompareRegInst(_, p, _, _, _, _, _) |
        CompareLongInst(_, p, _, _) => Some(p),
        _ => None
    }
}

// Return Rs.
fn srcreg1(inst: &InstNode) -> Option<Reg> {
    match *inst {
        ALU2ShortInst(_, _, _, r, _, _) |
        ALU2RegInst(_, _, _, r, _, _, _) |
        ALU2LongInst(_, _, _, r) |
        ALU1RegShInst(_, _, _, r, _, _) |
        LoadInst(_, _, _, r, _) |
        StoreInst(_, _, r, _, _) |
        CompareShortInst(_, _, r, _, _, _) |
        CompareRegInst(_, _, r, _, _, _, _) |
        CompareLongInst(_, _, r, _) |
        BranchRegInst(_, _, r, _) |
        MtcInst(_, _, r) |
        MthiInst(_, r) |
        MultInst(_, _, _, r, _) |
        DivInst(_, _, _, _, r, _) |
        FlushInst(_, _, r) => Some(r),
        _ => None
    }
}

// Return Rt
fn srcreg2(inst: &InstNode) -> Option<Reg> {
    match *inst {
        ALU1RegInst(_, _, _, r, _, _) |
        ALU2RegInst(_, _, _, _, r, _, _) |
        ALU1RegShInst(_, _, _, _, _, r) |
        StoreInst(_, _, _, _, r) |
        CompareRegInst(_, _, _, _, r, _, _) |
        MultInst(_, _, _, _, r) |
        DivInst(_, _, _, _, _, r) => Some(r),
        _ => None
    }
}

// Return which coprocessor register we write to.
fn destcoreg(inst: &InstNode) -> Option<CoReg> {
    match *inst {
        MtcInst(_, r, _) => Some(r),
        _ => None
    }
}

// Return which coprocessor register we read from.
fn srccoreg(inst: &InstNode) -> Option<CoReg> {
    match *inst {
        MfcInst(_, _, r) => Some(r),
        _ => None
    }
}

// Returns whether or not the order of inst1 and inst2 can be swapped.
fn commutes(inst1: &InstNode, inst2: &InstNode) -> bool {
    commutes_(inst1, inst2) && commutes_(inst2, inst1)
}

fn commutes_(inst1: &InstNode, inst2: &InstNode) -> bool {
    // Nop always commutes.
    if *inst1 == NopInst {
        return true;
    }

    // Fence never commutes; neither does break (for now).
    // Also treat the Packets pseudo-instruction as never commuting,
    // because we should never commute past inline ASM.
    match *inst1 {
        BreakInst(..) |
        FenceInst(..) |
        PacketsInst(..) => return false,
        // Never commute a load with a store, because we aren't smart enough
        // to know about aliasing.
        // Also, for now, never commute store with store.
        // TODO: these rules can be relaxed a bit.
        LoadInst(..) => match *inst2 {
            StoreInst(..) => return false,
            _ => {},
        },
        StoreInst(..) => match *inst2 {
            StoreInst(..) |
            LoadInst(..) => return false,
            _ => {},
        },
        _ => {},
    }
    match *inst2 {
        // TODO: this is more conservative than it needs to be, with LongInst.
        // We really only need to prevent it from commuting with its immediate
        // predecessor.
        LongInst(..) |
        BreakInst(..) |
        FenceInst(..) |
        PacketsInst(..) => return false,
        _ => {},
    }

    let pred1 = pred(inst1);
    let pred2 = pred(inst2);
    // If two instructions have opposite predicates, we can commute them.
    match pred1 {
        Some(ref p1) => match pred2 {
            Some(ref p2) => {
                if p1.reg == p2.reg &&
                    p1.inverted != p2.inverted {
                        return true;
                    }
            },
            _ => {},
        },
        _ => {},
    }

    let destreg1 = destreg(inst1);
    // inst1 writes a variable that inst2 uses.
    if destreg1.is_some() &&
        (destreg1 == srcreg1(inst2) ||
         destreg1 == srcreg2(inst2) ||
         destreg1 == destreg(inst2)) {
            return false;
        }

    // inst1 writes to a predicate that inst2 uses.
    let destpred1 = destpred(inst1);
    match destpred1 {
        Some(ref p1) => match pred(inst2) {
            Some(ref p2) => {
                if p1.reg == p2.reg {
                    return false;
                }
            },
            _ => {},
        },
        _ => {},
    }

    // coregs conflict
    if destcoreg(inst1).is_some() &&
        destcoreg(inst1) == srccoreg(inst2) {
            return false;
        }

    // Unless one of the exceptions above applies, jumps can't commute.
    match *inst1 {
        BranchImmInst(..) |
        BranchRegInst(..)  => return false,
        _ => {},
    }

    // TODO: a few more cases.

    return true;
}

// Returns whether inst1 and inst2 (appearing in that order in the original
// instruction list) can be placed in the same packet.
fn compatible_insts(inst1: &InstNode, inst2: &InstNode) -> bool {
    commutes_(inst1, inst2)
}

fn compatible(packet: &[InstNode; 4], inst: &InstNode) -> bool {
    packet.iter().all(|x| compatible_insts(x, inst))
}

fn update_labels(label_map: &BTreeMap<usize, Vec<&String>>,
                 new_label_list: &mut BTreeMap<String, usize>,
                 orig_pos: usize,
                 new_pos: usize) {
    let labels = label_map.get(&orig_pos);
    match labels {
        // Update the labels that pointed here.
        Some(labels) => {
            for label in labels.iter() {
                new_label_list.insert((*label).clone(), new_pos);
            }
        },
        None => {},
    }

}

/// Dummy scheduler: one instruction per packet.
pub fn schedule_dummy(insts: &Vec<InstNode>,
                      labels: &BTreeMap<String, usize>,
                      _: bool) -> (Vec<[InstNode; 4]>,
                                   BTreeMap<String, usize>) {
    let mut packets = vec!();

    let mut modified_labels: BTreeMap<String, usize> = BTreeMap::new();
    let mut jump_target_dict: BTreeMap<usize, Vec<&String>> = BTreeMap::new();
    for (label, pos) in labels.iter() {
        if jump_target_dict.contains_key(pos) {
            jump_target_dict.get_mut(pos).unwrap().push(label);
        } else {
            jump_target_dict.insert(*pos, vec!(label));
        }
    }

    for i in 0 .. insts.len() {
        match insts[i] {
            LongInst(..) => continue,
            PacketsInst(ref inline_packets) => {
                update_labels(&jump_target_dict,
                              &mut modified_labels,
                              i,
                              packets.len());
                // TODO label support in inline asm.
                for packet in inline_packets.iter() {
                    packets.push([packet[0].clone(),
                                  packet[1].clone(),
                                  packet[2].clone(),
                                  packet[3].clone()]);
                }
                continue;
            }
            _ => {},
        }
        let is_long = match insts[i] {
            ALU2LongInst(..) |
            ALU1LongInst(..) |
            CompareLongInst(..) => true,
            _ => false,
        };
        update_labels(&jump_target_dict,
                      &mut modified_labels,
                      i,
                      packets.len());

        if is_long {
            packets.push([insts[i].clone(), insts[i+1].clone(), NopInst,
                          NopInst]);
        } else {
            packets.push([insts[i].clone(), NopInst, NopInst, NopInst]);
        }
    }

    (packets, modified_labels)
}

pub fn schedule(insts: &Vec<InstNode>,
                labels: &BTreeMap<String, usize>,
                debug: bool) -> (Vec<[InstNode; 4]>,
                                 BTreeMap<String, usize>) {
    let mut packets: Vec<[InstNode; 4]> = vec!();

    let mut modified_labels: BTreeMap<String, usize> = BTreeMap::new();
    let mut jump_target_dict: BTreeMap<usize, Vec<&String>> = BTreeMap::new();
    for (label, pos) in labels.iter() {
        if jump_target_dict.contains_key(pos) {
            jump_target_dict.get_mut(pos).unwrap().push(label);
        } else {
            jump_target_dict.insert(*pos, vec!(label));
        }
    }

    // Include all jump targets...
    let mut jump_targets: Vec<usize> =
        FromIterator::from_iter(labels.iter().map(|(_, &b)| b));
    // .. and all jumps.
    for (idx, inst) in insts.iter().enumerate() {
        match *inst {
            BranchImmInst(..) |
            BranchRegInst(..) => jump_targets.push(idx+1),
            _ => {},
        }
    }

    jump_targets.sort();

    jump_targets.push(insts.len());

    if debug {
        for target in jump_targets.iter() {
            print!("targets:{}\n", target);
        }
    }

    let mut start = 0;
    for end in jump_targets.into_iter() {
        if debug {
            print!("Scheduling ({}, {})\n", start, end);
        }

        // Start by building the instruction DAG.
        let mut edges: BTreeSet<(usize, usize)> = BTreeSet::new();
        let mut all: BTreeSet<usize> =
            FromIterator::from_iter(start .. end);
        let mut this_packet: [InstNode; 4] =
            [NopInst, NopInst, NopInst, NopInst];
        let mut packet_added = false;
        for idx in start .. end {
            let inst = &insts[idx];

            for prev_idx in start .. idx {
                let prev_inst = &insts[prev_idx];
                if !commutes(prev_inst, inst) {
                    edges.insert((prev_idx, idx));
                }
            }
        }

        if debug {
            print!("edges: {:?}\n", edges);
        }

        // We now have a DAG, corresponding to dependencies among instructions.
        // Scheduling involves essentially a topological sort of this DAG.

        while !all.is_empty() {
            let non_schedulables: BTreeSet<usize> = FromIterator::from_iter(
                edges.iter().map(|&(_, x)| x));
            let leaves: BTreeSet<usize> = FromIterator::from_iter(
                all.iter()
                    .map(|&x| x)
                    .filter(|x| !non_schedulables.contains(x)));
            let mut added = false;
            for leaf in leaves.iter() {
                let inst = &insts[*leaf];
                match *inst {
                    PacketsInst(ref inline_packets) => {
                        // Flush the current packet.
                        if packet_added {
                            packets.push(this_packet);
                            this_packet = [NopInst, NopInst, NopInst, NopInst];
                            packet_added = false;
                            added = false;
                        }

                        update_labels(&jump_target_dict,
                                      &mut modified_labels,
                                      *leaf,
                                      packets.len());
                        // TODO label support in inline asm.
                        for packet in inline_packets.iter() {
                            packets.push([packet[0].clone(),
                                          packet[1].clone(),
                                          packet[2].clone(),
                                          packet[3].clone()]);
                        }
                        // The packet being built at this point should be empty.
                        assert!(this_packet.iter().all(|x| *x == NopInst));
                        edges = FromIterator::from_iter(
                            edges.iter().map(|&x|x)
                                .filter(|&(x, _)| x!=*leaf));
                        all.remove(leaf);
                        break;
                    },
                    _ => {}
                }

                if compatible(&this_packet, inst) {
                    for idx in (
                        0 ..
                        (match classify_inst(inst) {
                            InstType::ControlType => 0,
                            InstType::MemoryType => 1,
                            _ => 3
                        })+1).rev() {
                        // Does this expect a long after it?
                        let is_long = match *inst {
                            ALU2LongInst(..) |
                            ALU1LongInst(..) |
                            CompareLongInst(..) => true,
                            _ => false,
                        };
                        if this_packet[idx] == NopInst && (
                            !is_long ||
                                (idx < 3 && this_packet[idx+1] == NopInst)) {
                            // Usually we just have to move one instruction
                            // into the packet, but for instructions expecting
                            // a long we have to move two.
                            for offs in 0 .. if is_long { 2 } else { 1 } {
                                let inst = &insts[*leaf+offs];
                                all.remove(&(*leaf+offs));
                                update_labels(&jump_target_dict,
                                              &mut modified_labels,
                                              *leaf+offs,
                                              packets.len());
                                this_packet[idx+offs] = inst.clone();
                                added = true;
                                packet_added = true;
                                edges = FromIterator::from_iter(
                                    edges.iter().map(|&x|x)
                                        .filter(|&(x, _)| x!=*leaf+offs));
                            }
                            break;
                        }
                    }
                    if added {
                        break;
                    }
                }
            }

            // This packet is as full as it can get. Move on!
            if !this_packet.iter().any(|x| *x == NopInst) || !added {
                packets.push(this_packet);
                this_packet = [NopInst, NopInst, NopInst, NopInst];
                packet_added = false;
            }
        }

        if packet_added {
            packets.push(this_packet);
        }

        start = end;
    }

    // If there's any label at the very end, we have to update it too.
    update_labels(&jump_target_dict,
                  &mut modified_labels,
                  insts.len(),
                  packets.len());

    if debug {
        print!("packets: {:?}\n", packets);
        print!("Efficiency: {} instructions in {} packets = {}\n",
               insts.len(), packets.len(),
               insts.len() as f32 / packets.len() as f32);
    }
    (packets, modified_labels)
}
