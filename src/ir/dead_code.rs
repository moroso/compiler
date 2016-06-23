use ir::{Op, OpNode};
use ir::liveness::LivenessAnalyzer;

use std::collections::BTreeSet;

// For debugging
use mas::ast::*;


pub struct DeadCodeEliminator;

impl DeadCodeEliminator {
    pub fn eliminate(insts: &mut Vec<Op>, verbose: bool, debug_mode: bool) {
        let funcname = if let OpNode::Func(ref name, _, _) = insts[0].val {
            format!("{}", name)
        } else {
            unreachable!()
        };

        for op in insts.iter() {
            // DCE fails pretty badly with asm operations. We won't even attempt to do anything.
            match op.val {
                OpNode::AsmOp(..) => { return; }
                _ => {}
            }
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
                if predecessors[pos].len() == 0 {
                    match op.val {
                        OpNode::Nop => {},
                        _ => {
                            if verbose {
                                print!("  {}\n", op.val);
                            }

                            if debug_mode {
                                // In debug mode, replace all dead code with debug breaks, so we can easily
                                // tell in the sim if we end up hitting it.
                                op.val = OpNode::AsmOp(
                                    vec!(
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
                                    )
                                );
                            } else {
                                op.val = OpNode::Nop;
                            }
                        }
                    }
                    for successor in successors[pos].iter() {
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
