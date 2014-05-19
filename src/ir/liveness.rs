// Liveness analysis

use ir::*;
use ast::*;
use values::*;

pub struct LivenessAnalyzer {
    pub ops: Box<Vec<Op>>,
    pub opinfo: Vec<OpInfo>,
}

fn add_to<T: Eq>(ref mut v: &mut Vec<T>, x: T) -> bool {
    for e in v.iter() {
        if *e == x { return false; }
    }
    v.push(x);
    true
}

impl LivenessAnalyzer {
    pub fn new(ops: Box<Vec<Op>>) -> LivenessAnalyzer {
        let len = ops.len();
        LivenessAnalyzer {
            ops: ops,
            opinfo: Vec::from_fn(len,
                                 |_| OpInfo {
                                     live: vec!(),
                                     used: vec!(),
                                     def: vec!(),
                                     succ: vec!(),
                                 }
                                 )
        }
    }

    pub fn seed(&mut self) {
        let len = self.ops.len();

        for u in range(0, len) {
            match *self.ops.get(u) {
                Assign(ref lv, ref rv) => {
                    let opinfo = self.opinfo.get_mut(u);
                    match *lv {
                        VarLValue(ref v) =>
                            opinfo.def.push(v.clone()),
                        _ => {},
                    };
                    match *rv {
                        BinOpRValue(_, ref v1, ref v2) => {
                            match *v1 {
                                Variable(ref w1) =>
                                    opinfo.used.push(w1.clone()),
                                _ => {},
                            };
                            match *v2 {
                                Variable(ref w2) =>
                                    opinfo.used.push(w2.clone()),
                                _ => {},
                            };
                        },
                        DirectRValue(ref v) =>
                            match *v {
                                Variable(ref v2) =>
                                    opinfo.used.push(v2.clone()),
                                _ => {},
                            },
                    };
                    // TODO: this needs to handle jumps.
                    if u + 1 < len {
                        opinfo.succ.push(u + 1);
                    }
                },
                _ => {}
            }
        }
    }

    pub fn propagate_once(&mut self) -> bool {
        let mut modified = false;
        let len = self.ops.len();

        for u in range(0, len) {
            let mut opinfo = self.opinfo.get_mut(u).clone();

            for usedvar in opinfo.used.iter() {
                modified = modified || add_to(&mut opinfo.live,
                                              usedvar.clone());
            }
            for next_idx in opinfo.succ.iter() {
                let mut next_opinfo = self.opinfo.get_mut(*next_idx);
                for livevar in next_opinfo.used.iter() {
                    if !opinfo.def.contains(livevar) {
                        modified = modified || add_to(&mut opinfo.live,
                                                      livevar.clone());
                    }
                }
            }
            *self.opinfo.get_mut(u) = opinfo;
        }
        modified
    }

    pub fn propagate(&mut self) {
        while self.propagate_once() {};
    }

}
