// Liveness analysis
use time::precise_time_ns;

use std::collections::BTreeSet;
use std::iter::FromIterator;
use std::mem::swap;

use ir::*;


pub struct LivenessAnalyzer;

fn seed_rve(opinfo: &mut OpInfo, rve: &RValueElem) {
    match *rve {
        Variable(v) => { opinfo.used.insert(v); },
        _ => {}
    }
}

fn seed(ops: &Vec<Op>, opinfo: &mut Vec<OpInfo>) {
    let len = ops.len();
    if len == 1 { return; } // No instructions; probably extern.
                            // TODO: this is somewhat fragile...

    // Common code between ordinary goto and conditional goto.
    // Handles everything needed by the unconditional goto, which
    // is a subset of what's needed for conditional.
    fn handle_goto(len: usize, ops: &Vec<Op>, opinfo: &mut OpInfo,
                   l: usize, vars: &BTreeSet<Var>) {
        for u2 in 0..len {
            match ops[u2].val {
                OpNode::Label { label_idx: l2, .. } if l == l2 => {
                    opinfo.succ.insert(u2);
                    break;
                },
                _ => {},
            }
        }
        for var in vars.iter() {
            opinfo.used.insert(*var);
        }
    }


    for u in 0..len {
        let opinfo = opinfo.get_mut(u).unwrap();
        match ops[u].val {
            OpNode::BinOp { target: lv, lhs: ref rve1, rhs: ref rve2, .. } => {
                opinfo.def.insert(lv);
                seed_rve(opinfo, rve1);
                seed_rve(opinfo, rve2);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::UnOp { target: lv, operand: ref rve, .. } => {
                opinfo.def.insert(lv);
                seed_rve(opinfo, rve);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Load { target: lv, addr: rv, .. } => {
                opinfo.def.insert(lv);
                opinfo.used.insert(rv);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Store { addr: v1, value: v2, .. } => {
                opinfo.used.insert(v1);
                opinfo.used.insert(v2);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Call { target: ref lv_opt, func: ref f, ref args } => {
                match *lv_opt {
                    Some(lv) => { opinfo.def.insert(lv); },
                    None => {},
                }
                seed_rve(opinfo, f);
                for arg in args.iter() {
                    opinfo.used.insert(arg.clone());
                }

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Alloca { var: lv, .. } => {
                opinfo.def.insert(lv);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Nop {} => {
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            }
            OpNode::Label { ref vars, .. } => {
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
                for var in vars.iter() {
                    opinfo.def.insert(*var);
                }
            },
            OpNode::Goto { label_idx: l, ref vars } => {
                handle_goto(len, ops, opinfo, l, vars);
            },
            OpNode::CondGoto { cond: ref rve, label_idx: l, ref vars, .. } => {
                handle_goto(len, ops, opinfo, l, vars);
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
                match *rve {
                    Variable(v) => { opinfo.used.insert(v); },
                    _ => {},
                }
            },
            OpNode::Return { retval: ref v } => {
                match *v {
                    Some(Variable(w1)) => { opinfo.used.insert(w1); },
                    _ => {},
                }
            },
            OpNode::Func { args: ref vars, .. } => {
                for v in vars.iter() {
                    opinfo.def.insert(*v);
                }
                opinfo.succ.insert(u + 1);
            },
            // TODO: fill this in, when ready.
            OpNode::AsmOp { .. } => {},
        }
    }
}

fn propagate_once(opinfo: &mut Vec<OpInfo>,
                  active_ops: BTreeSet<usize>,
                  predecessors: &Vec<BTreeSet<usize>>) -> BTreeSet<usize> {
    let mut new_active_ops = BTreeSet::<usize>::new();
    for u in active_ops.into_iter() {
        // Making this dummy structure is sort of unfortunate.
        // There are ways around it, but it is better than the old
        // clone!
        let mut this_opinfo = OpInfo::new();
        // This working relies on instructions not being able to be their own
        // successor. If they were, then we'd be emptying information we need.
        swap(&mut this_opinfo, opinfo.get_mut(u).unwrap());

        for usedvar in this_opinfo.used.iter() {
            if this_opinfo.live.insert(*usedvar) {
                new_active_ops.insert(u);
                for &predecessor in predecessors[u].iter() {
                    new_active_ops.insert(predecessor);
                }
            }
        }
        for next_idx in this_opinfo.succ.iter() {
            let next_opinfo = opinfo.get_mut(*next_idx).unwrap();
            for livevar in next_opinfo.live.iter() {
                if !this_opinfo.def.contains(livevar) {
                    if this_opinfo.live.insert(*livevar) {
                        for &predecessor in predecessors[u].iter() {
                            new_active_ops.insert(predecessor);
                        }
                    }
                }
            }
        }
        *opinfo.get_mut(u).unwrap() = this_opinfo;
    }

    new_active_ops
}

#[allow(non_upper_case_globals)]
static mut seed_time: u64 = 0;
#[allow(non_upper_case_globals)]
static mut propagate_time: u64 = 0;

pub fn get_liveness_times() -> (u64, u64) {
    unsafe {
        (seed_time, propagate_time)
    }
}

fn propagate(ops: &Vec<Op>, opinfo: &mut Vec<OpInfo>) {
    let start = precise_time_ns();

    let mut active_ops: BTreeSet<usize> = FromIterator::from_iter(0..ops.len());
    let mut predecessors: Vec<BTreeSet<usize>> = (0..ops.len()).map(|_| BTreeSet::new()).collect();
    for idx in 0..ops.len() {
        for &successor in opinfo.get(idx).unwrap().succ.iter() {
            predecessors[successor].insert(idx);
        }
    }

    while active_ops.len() > 0 {
        active_ops = propagate_once(opinfo, active_ops, &predecessors);
    }
    let end = precise_time_ns();
    unsafe {
        propagate_time += end-start;
    }
}

impl LivenessAnalyzer {
    // Gives back the seeded data. This is mostly useful for getting
    // defs.
    pub fn unanalyzed_opinfo(ops: &Vec<Op>) -> Vec<OpInfo> {
        let len = ops.len();
        let mut opinfo = (0..len).map(|_| OpInfo::new()).collect();
        let start = precise_time_ns();
        seed(ops, &mut opinfo);
        let end = precise_time_ns();
        unsafe {
            seed_time += end-start;
        }

        opinfo
    }

    pub fn analyze(ops: &Vec<Op>) -> Vec<OpInfo> {
        let mut opinfo = LivenessAnalyzer::unanalyzed_opinfo(ops);
        propagate(ops, &mut opinfo);

        opinfo
    }

}
