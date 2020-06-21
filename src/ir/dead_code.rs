use ir::{Op, OpNode};
use ir::liveness::LivenessAnalyzer;

use std::collections::{BTreeSet, BTreeMap};

// For debugging
use mas::ast::*;


pub struct DeadCodeEliminator;

impl DeadCodeEliminator {
    pub fn eliminate(insts: &mut Vec<Op>, verbose: bool, debug_mode: bool) {
        let funcname = if let OpNode::Func { ref name, .. } = insts[0].val {
            format!("{}", name)
        } else {
            unreachable!()
        };

        for op in insts.iter() {
            // DCE fails pretty badly with asm operations. We won't even attempt to do anything.
            if let OpNode::AsmOp { .. } = op.val { return; }
        }

        if verbose {
            print!("Dead code elim: {}\n", funcname);
        }

        let mut successors: Vec<BTreeSet<usize>> =
            LivenessAnalyzer::analyze(insts).into_iter().map(|x| x.succ).collect();
        let mut predecessors: Vec<BTreeSet<usize>> = (0..insts.len()).map(|_| BTreeSet::new()).collect();

        for (pos, succs) in successors.iter().enumerate() {
            for successor in succs {
                predecessors[*successor].insert(pos);
            }
        }

        let mut changed: bool = true;
        while changed {
            changed = false;
            for (pos, op) in insts.iter_mut().enumerate().skip(1) {
                if predecessors[pos].is_empty() {
                    match op.val {
                        OpNode::Nop {} => {},
                        _ => {
                            if verbose {
                                print!("  {}\n", op.val);
                            }

                            if debug_mode {
                                // In debug mode, replace all dead code with debug breaks, so we can easily
                                // tell in the sim if we end up hitting it.
                                op.val = OpNode::AsmOp {
                                    insts: vec!(
                                        vec!(
                                            InstNode::alu1short(
                                                TRUE_PRED,
                                                MovAluOp,
                                                Reg { index: 30 },
                                                3,
                                                0
                                            ),
                                            InstNode::nop(), InstNode::nop(), InstNode::nop(),
                                        ),
                                        vec!(
                                            InstNode::breaknum(
                                                TRUE_PRED,
                                                0x1f
                                            ),
                                            InstNode::nop(), InstNode::nop(), InstNode::nop(),
                                        )
                                    ),
                                    labels: BTreeMap::new(),
                                };
                            } else {
                                op.val = OpNode::Nop {};
                            }
                        }
                    }
                    for successor in &successors[pos] {
                        let res = predecessors[*successor].remove(&pos);
                        assert!(res);
                        changed = true;
                    }
                    successors[pos].clear();
                }
            }
        }
    }
}
