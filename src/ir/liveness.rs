// Liveness analysis

use std::collections::BTreeSet;
use std::cmp::Eq;
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

    for u in range(0, len) {
        let opinfo = opinfo.get_mut(u);
        match ops[u] {
            BinOp(ref lv, _, ref rve1, ref rve2, _) => {
                opinfo.def.insert(lv.clone());
                seed_rve(opinfo, rve1);
                seed_rve(opinfo, rve2);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            UnOp(ref lv, _, ref rve) => {
                opinfo.def.insert(lv.clone());
                seed_rve(opinfo, rve);

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            Load(ref lv, ref rv, _) => {
                opinfo.def.insert(lv.clone());
                opinfo.used.insert(rv.clone());

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            Store(ref v1, ref v2, _) => {
                opinfo.used.insert(v1.clone());
                opinfo.used.insert(v2.clone());

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            Call(ref lv, ref f, ref args) => {
                opinfo.def.insert(lv.clone());
                seed_rve(opinfo, f);
                for arg in args.iter() {
                    opinfo.used.insert(arg.clone());
                }

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            Alloca(lv, _) => {
                opinfo.def.insert(lv.clone());

                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            Nop => {
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            }
            Label(_, ref vars) => {
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
                for var in vars.iter() {
                    opinfo.def.insert(var.clone());
                }
            },
            Goto(ref l, ref vars) => {
                for u2 in range(0, len) {
                    match ops[u2] {
                        Label(l2, _) if *l == l2 => {
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
            CondGoto(_, ref rve, ref l, ref vars) => {
                for u2 in range(0, len) {
                    match ops[u2] {
                        Label(l2, _) if *l == l2 => {
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
            Return(ref v) => {
                match *v {
                    Variable(ref w1) => { opinfo.used.insert(w1.clone()); },
                    _ => {},
                }
            },
            Func(_, ref vars, is_extern) => {
                if is_extern { return; }
                for v in vars.iter() {
                    opinfo.def.insert(v.clone());
                }
                opinfo.succ.insert(u + 1);
            }
        }
    }
}

fn propagate_once(ops: &Vec<Op>, opinfo: &mut Vec<OpInfo>) -> bool {
    let mut modified = false;
    let len = ops.len();

    for u in range(0, len) {
        // Making this dummy structure is sort of unfortunate.
        // There are ways around it, but it is better than the old
        // clone!
        let mut this_opinfo = OpInfo::new();
        // This working relies on instructions not being able to be their own
        // successor. If they were, then we'd be emptying information we need.
        swap(&mut this_opinfo, opinfo.get_mut(u));

        for usedvar in this_opinfo.used.iter() {
            modified = modified || this_opinfo.live.insert(usedvar.clone());
        }
        for next_idx in this_opinfo.succ.iter() {
            let next_opinfo = opinfo.get_mut(*next_idx);
            for livevar in next_opinfo.live.iter() {
                if !this_opinfo.def.contains(livevar) {
                    modified = modified
                        || this_opinfo.live.insert(livevar.clone());
                }
            }
        }
        *opinfo.get_mut(u) = this_opinfo;
    }
    modified
}

fn propagate(ops: &Vec<Op>, opinfo: &mut Vec<OpInfo>) {
    while propagate_once(ops, opinfo) {};
}

impl LivenessAnalyzer {
    // Gives back the seeded data. This is mostly useful for getting
    // defs.
    pub fn unanalyzed_opinfo(ops: &Vec<Op>) -> Vec<OpInfo> {
        let len = ops.len();
        let mut opinfo = Vec::from_fn(len, |_| OpInfo::new());
        seed(ops, &mut opinfo);

        opinfo
    }

    pub fn analyze(ops: &Vec<Op>) -> Vec<OpInfo> {
        let mut opinfo = LivenessAnalyzer::unanalyzed_opinfo(ops);
        propagate(ops, &mut opinfo);

        opinfo
    }

}
