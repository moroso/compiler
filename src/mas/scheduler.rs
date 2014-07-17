use mas::ast::*;
use mas::parser::{classify_inst,
                  ALUInstType,
                  ControlType,
                  MemoryType,
                  LongType};
use collections::TreeSet;

// Return Rd.
fn destreg(inst: &InstNode) -> Option<Reg> {
    match *inst {
        ALU1ShortInst(_, _, r, _, _) |
        ALU2ShortInst(_, _, r, _, _, _) |
        ALU1RegInst(_, _, r, _, _, _) |
        ALU2RegInst(_, _, r, _, _, _, _) |
        ALU2LongInst(_, _, r, _) |
        ALU1LongInst(_, _, r) |
        ALU1RegShInst(_, r, _, _, _) |
        LoadInst(_, _, r, _, _) |
        MfcInst(_, r, _) |
        MfhiInst(_, r) |
        MultInst(_, _, r, _, _) |
        DivInst(_, _, r, _, _) => Some(r),
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
        ALU1RegShInst(p, _, _, _, _) |
        LoadInst(p, _, _, _, _) |
        MfcInst(p, _, _) |
        MfhiInst(p, _) |
        MultInst(p, _, _, _, _) |
        DivInst(p, _, _, _, _) |
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
        ALU1RegShInst(_, _, r, _, _) |
        LoadInst(_, _, _, r, _) |
        StoreInst(_, _, r, _, _) |
        CompareShortInst(_, _, r, _, _, _) |
        CompareRegInst(_, _, r, _, _, _, _) |
        CompareLongInst(_, _, r, _) |
        BranchRegInst(_, _, r, _) |
        MtcInst(_, _, r) |
        MthiInst(_, r) |
        MultInst(_, _, _, r, _) |
        DivInst(_, _, _, r, _) |
        FlushInst(_, _, r) => Some(r),
        _ => None
    }
}

// Return Rt
fn srcreg2(inst: &InstNode) -> Option<Reg> {
    match *inst {
        ALU1RegInst(_, _, _, r, _, _) |
        ALU2RegInst(_, _, _, _, r, _, _) |
        ALU1RegShInst(_, _, _, _, r) |
        StoreInst(_, _, _, _, r) |
        CompareRegInst(_, _, _, _, r, _, _) |
        MultInst(_, _, _, _, r) |
        DivInst(_, _, _, _, r) => Some(r),
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

    // Fence never commutes.
    match *inst1 {
        FenceInst(..) => return false,
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
        destreg1 == srcreg1(inst2) ||
        destreg1 == srcreg2(inst2) {
            return false;
        }

    // inst1 writes to a predicate that inst2 uses.
    let destpred1 = destpred(inst1);
    match destpred1 {
        Some(ref p1) => match pred(inst2) {
            Some(ref p2) => {
                if p1.reg != p2.reg {
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

    // TODO: a few more cases.

    return true;
}

// Returns whether inst1 and inst2 (appearing in that order in the original
// instruction list) can commute.
fn compatible_insts(inst1: &InstNode, inst2: &InstNode) -> bool {
    commutes_(inst1, inst2)
}

fn compatible(packet: &[InstNode, ..4], inst: &InstNode) -> bool {
    match classify_inst(inst) {
        ControlType => { if packet[0] != NopInst { return false; } },
        MemoryType => {
            if packet[0] != NopInst && packet[1] != NopInst {
                return false;
            }
        },
        ALUInstType => {
            if !packet.iter().all(|x| *x == NopInst) {
                return false;
            }
        },
        LongType => {
            fail!("Should never be trying to directly schedule a long inst.")
        }
    }

    packet.iter().all(|x| compatible_insts(x, inst))
}

pub fn schedule(insts: &Vec<InstNode>) -> Vec<[InstNode, ..4]> {
    // Start by building the instruction DAG.
    let mut edges: TreeSet<(uint, uint)> = TreeSet::new();
    let mut all: TreeSet<uint> = FromIterator::from_iter(range(0, insts.len()));
    let mut this_packet: [InstNode, ..4] = [NopInst, NopInst, NopInst, NopInst];

    for idx in range(0, insts.len()) {
        let inst = insts.get(idx);

        for prev_idx in range(0, idx) {
            let prev_inst = insts.get(prev_idx);
            if !commutes(prev_inst, inst) {
                edges.insert((prev_idx, idx));
            }
        }
    }

    print!("{}\n", edges);

    // We now have a DAG, corresponding to dependencies among instructions.
    // Scheduling involves essentially a topological sort of this DAG.

    while !all.is_empty() {
        let non_schedulables: TreeSet<uint> = FromIterator::from_iter(
            edges.iter().map(|&(_, x)| x));
        let leaves: TreeSet<uint> = FromIterator::from_iter(
            all.iter().map(|&x| x).filter(|x| !non_schedulables.contains(x)));
        for leaf in leaves.iter() {
            all.remove(leaf);
            edges = FromIterator::from_iter(
                edges.iter().map(|&x|x).filter(|&(x, _)| x!=*leaf));
        }
        print!("{}\n", leaves);
    }

    unimplemented!()
}
