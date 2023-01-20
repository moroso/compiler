// Liveness analysis
use std::time::Instant;

use std::collections::BTreeSet;
use std::iter::FromIterator;
use std::mem::swap;

use ir::*;


pub struct LivenessAnalyzer;

fn seed_rve(opinfo: &mut OpInfo, rve: &RValueElem) {
    if let Variable(v) = *rve { opinfo.used.insert(v); }
}

fn seed(ops: &[Op], opinfo: &mut Vec<OpInfo>) {
    let len = ops.len();
    if len == 1 { return; } // No instructions; probably extern.
                            // TODO: this is somewhat fragile...

    // Common code between ordinary goto and conditional goto.
    // Handles everything needed by the unconditional goto, which
    // is a subset of what's needed for conditional.
    fn handle_goto(len: usize, ops: &[Op], opinfo: &mut OpInfo,
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
        let opinfo = &mut opinfo[u];
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
                if let Some(lv) = *lv_opt {
                    opinfo.def.insert(lv);
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
                if let Variable(v) = *rve { opinfo.used.insert(v); };
            },
            OpNode::Return { retval: ref v } => {
                if let Some(Variable(w1)) = *v { opinfo.used.insert(w1); };
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
                  predecessors: &[BTreeSet<usize>]) -> BTreeSet<usize> {
    let mut new_active_ops = BTreeSet::<usize>::new();
    for u in active_ops {
        // Making this dummy structure is sort of unfortunate.
        // There are ways around it, but it is better than the old
        // clone!
        let mut this_opinfo = OpInfo::new();
        // This working relies on instructions not being able to be their own
        // successor. If they were, then we'd be emptying information we need.
        swap(&mut this_opinfo, &mut opinfo[u]);

        for usedvar in &this_opinfo.used {
            if this_opinfo.live.insert(*usedvar) {
                new_active_ops.insert(u);
                for &predecessor in &predecessors[u] {
                    new_active_ops.insert(predecessor);
                }
            }
        }
        for next_idx in &this_opinfo.succ {
            let next_opinfo = &mut opinfo[*next_idx];
            for livevar in &next_opinfo.live {
                if !this_opinfo.def.contains(livevar) {
                    if this_opinfo.live.insert(*livevar) {
                        for &predecessor in &predecessors[u] {
                            new_active_ops.insert(predecessor);
                        }
                    }
                }
            }
        }
        opinfo[u] = this_opinfo;
    }

    new_active_ops
}

#[allow(non_upper_case_globals)]
static mut seed_time: u128 = 0;
#[allow(non_upper_case_globals)]
static mut propagate_time: u128 = 0;

pub fn get_liveness_times() -> (u128, u128) {
    unsafe {
        (seed_time, propagate_time)
    }
}

fn propagate(ops: &[Op], opinfo: &mut Vec<OpInfo>) {
    let start = Instant::now();

    let mut active_ops: BTreeSet<usize> = FromIterator::from_iter(0..ops.len());
    let mut predecessors: Vec<BTreeSet<usize>> = (0..ops.len()).map(|_| BTreeSet::new()).collect();
    for idx in 0..ops.len() {
        for &successor in &opinfo[idx].succ {
            predecessors[successor].insert(idx);
        }
    }

    while !active_ops.is_empty() {
        active_ops = propagate_once(opinfo, active_ops, &predecessors);
    }
    let end = Instant::now();
    unsafe {
        propagate_time += (end-start).as_nanos();
    }
}

impl LivenessAnalyzer {
    // Gives back the seeded data. This is mostly useful for getting
    // defs.
    pub fn unanalyzed_opinfo(ops: &[Op]) -> Vec<OpInfo> {
        let len = ops.len();
        let mut opinfo = (0..len).map(|_| OpInfo::new()).collect();
        let start = Instant::now();
        seed(ops, &mut opinfo);
        let end = Instant::now();
        unsafe {
            seed_time += (end-start).as_nanos();
        }

        opinfo
    }

    pub fn analyze(ops: &[Op]) -> Vec<OpInfo> {
        let mut opinfo = LivenessAnalyzer::unanalyzed_opinfo(ops);
        propagate(ops, &mut opinfo);

        opinfo
    }

}
