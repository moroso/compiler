// Liveness analysis

use std::collections::TreeSet;
use std::cmp::Eq;
use ir::*;
use ast::*;
use values::*;

pub struct LivenessAnalyzer;

fn seed(ops: &Vec<Op>, opinfo: &mut Vec<OpInfo>) {
    let len = ops.len();

    for u in range(0, len) {
        match *ops.get(u) {
            Assign(ref lv, ref rv) => {
                let opinfo = opinfo.get_mut(u);
                match *lv {
                    VarLValue(ref v) =>
                    { opinfo.def.insert(v.clone()); },
                    _ => {},
                };
                match *rv {
                    BinOpRValue(_, ref v1, ref v2) => {
                        match *v1 {
                            Variable(ref w1) =>
                            { opinfo.used.insert(w1.clone()); },
                            _ => {},
                        };
                        match *v2 {
                            Variable(ref w2) =>
                            { opinfo.used.insert(w2.clone()); },
                            _ => {},
                        };
                    },
                    UnOpRValue(_, ref v1) => {
                        match *v1 {
                            Variable(ref w1) =>
                            { opinfo.used.insert(w1.clone()); },
                            _ => {},
                        };
                    },
                    DirectRValue(ref v) =>
                        match *v {
                            Variable(ref v2) =>
                            { opinfo.used.insert(v2.clone()); },
                            _ => {},
                        },
                    CallRValue(ref v, ref args) => {
                        match *v {
                            Variable(ref v2) =>
                            { opinfo.used.insert(v2.clone()); },
                            _ => {},
                        };
                        for arg in args.iter() {
                            match *arg {
                                Variable(ref v2) =>
                                { opinfo.used.insert(v2.clone()); },
                                _ => {},
                            }
                        }
                    },
                    AllocaRValue(..) => { }
                };
                // TODO: this needs to handle jumps.
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
            },
            Nop => {
                if u + 1 < len {
                    let opinfo = opinfo.get_mut(u);
                    opinfo.succ.insert(u + 1);
                }
            }
            Label(_, ref vars) => {
                let opinfo = opinfo.get_mut(u);
                if u + 1 < len {
                    opinfo.succ.insert(u + 1);
                }
                for var in vars.iter() {
                    opinfo.def.insert(var.clone());
                }
            },
            Goto(ref l, ref vars) => {
                let opinfo = opinfo.get_mut(u);
                for u2 in range(0, len) {
                    match *ops.get(u2) {
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
            CondGoto(_, ref l, ref vars) => {
                let opinfo = opinfo.get_mut(u);
                for u2 in range(0, len) {
                    match *ops.get(u2) {
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
            },
            Return(ref v) => {
                let opinfo = opinfo.get_mut(u);
                match *v {
                    Variable(ref w1) => { opinfo.used.insert(w1.clone()); },
                    _ => {},
                }
            },
            Func(_, ref vars) => {
                let opinfo = opinfo.get_mut(u);
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
        let mut this_opinfo = opinfo.get_mut(u).clone();

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
    pub fn analyze(ops: &Vec<Op>) -> Vec<OpInfo> {
        let len = ops.len();
        let mut opinfo = Vec::from_fn(len,
                                  |_| OpInfo {
                                      live: TreeSet::new(),
                                      used: TreeSet::new(),
                                      def: TreeSet::new(),
                                      succ: TreeSet::new(),
                                  }
                                  );
        seed(ops, &mut opinfo);
        propagate(ops, &mut opinfo);

        opinfo
    }

}
