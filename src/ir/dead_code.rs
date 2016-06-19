use ir::{Op, OpNode};
use ir::liveness::LivenessAnalyzer;

use std::collections::BTreeSet;

pub struct DeadCodeEliminator;

impl DeadCodeEliminator {
    pub fn eliminate(insts: &mut Vec<Op>, verbose: bool) {
        if verbose {
            print!("Dead code elim...\n");
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
                    op.val = OpNode::Nop;
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
