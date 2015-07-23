// Liveness analysis
use time::precise_time_ns;

use std::collections::BTreeSet;
use std::cmp::Eq;
use std::iter::FromIterator;
use std::mem::swap;

use mc::ast::*;

use ir::*;
use values::*;


pub struct LivenessAnalyzer;

fn seed_rve(opinfo: &mut OpInfo, rve: &RValueElem) {
    match *rve {
        Variable(ref v) => { opinfo.used.insert(v.clone()); },
        _ => {}
    }
}

fn seed(ops: &Vec<Op>, opinfo: &mut Vec<OpInfo>) {
    let len = ops.len();

    for u in 0..len {
        let opinfo = opinfo.get_mut(u).unwrap();
        match ops[u].val {
            OpNode::BinOp(ref lv, _, ref rve1, ref rve2, _) => {
                opinfo.def.insert(lv.clone());
                seed_rve(opinfo, rve1);
                seed_rve(opinfo, rve2);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::UnOp(ref lv, _, ref rve) => {
                opinfo.def.insert(lv.clone());
                seed_rve(opinfo, rve);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Load(ref lv, ref rv, _) => {
                opinfo.def.insert(lv.clone());
                opinfo.used.insert(rv.clone());

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Store(ref v1, ref v2, _) => {
                opinfo.used.insert(v1.clone());
                opinfo.used.insert(v2.clone());

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Call(ref lv, ref f, ref args) => {
                opinfo.def.insert(lv.clone());
                seed_rve(opinfo, f);
                for arg in args.iter() {
                    opinfo.used.insert(arg.clone());
                }

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Alloca(lv, _) => {
                opinfo.def.insert(lv.clone());

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            OpNode::Nop => {
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            }
            OpNode::Label(_, ref vars) => {
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
                for var in vars.iter() {
                    opinfo.def.insert(var.clone());
                }
            },
            OpNode::Goto(ref l, ref vars) => {
                for u2 in 0..len {
                    match ops[u2].val {
                        OpNode::Label(l2, _) if *l == l2 => {
                            opinfo.succ.insert(u2);
                            break;
                        },
                        _ => {},
                    }
                }
                for var in vars.iter() {
                    opinfo.used.insert(var.clone());
                }
            },
            // TODO: get rid of redundant code.
            OpNode::CondGoto(_, ref rve, ref l, ref vars) => {
                for u2 in 0..len {
                    match ops[u2].val {
                        OpNode::Label(l2, _) if *l == l2 => {
                            opinfo.succ.insert(u2);
                            break;
                        },
                        _ => {},
                    }
                }
                for var in vars.iter() {
                    opinfo.used.insert(var.clone());
                }
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
                match *rve {
                    Variable(ref v) => { opinfo.used.insert(v.clone()); },
                    _ => {},
                }
            },
            OpNode::Return(ref v) => {
                match *v {
                    Variable(ref w1) => { opinfo.used.insert(w1.clone()); },
                    _ => {},
                }
            },
            OpNode::Func(_, ref vars, ref abi) => {
                if abi.is_some() { return; }
                for v in vars.iter() {
                    opinfo.def.insert(v.clone());
                }
                opinfo.succ.insert(u + 1);
            },
            // TODO: fill this in, when ready.
            OpNode::AsmOp(..) => {},
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
            if this_opinfo.live.insert(usedvar.clone()) {
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
                    if this_opinfo.live.insert(livevar.clone()) {
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

static mut seed_time: u64 = 0;
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
