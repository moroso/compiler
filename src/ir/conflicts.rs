use ir::*;
use ir::liveness::LivenessAnalyzer;
use std::collections::{TreeMap, TreeSet};

pub struct ConflictAnalyzer;

impl ConflictAnalyzer {
    /// Analyze a block of IR code, and return a map of conflicting registers.
    /// Assumes the IR is already in SSA form.
    pub fn conflicts(ops: &Vec<Op>) -> (TreeMap<Var, TreeSet<Var>>,
                                        TreeMap<Var, u32>) {
        let opinfo = LivenessAnalyzer::analyze(ops);
        let mut conflict_map = TreeMap::<Var, TreeSet<Var>>::new();
        let mut counts = TreeMap::<Var, u32>::new();

        for info in opinfo.iter() {
            for var1 in info.live.iter().chain(info.def.iter()) {
                for var2 in info.live.iter().chain(info.def.iter()) {
                    if var1 != var2 {
                        if !conflict_map.contains_key(var1) {
                            conflict_map.insert(*var1, TreeSet::<Var>::new());
                        }
                        if !conflict_map.contains_key(var2) {
                            conflict_map.insert(*var2, TreeSet::<Var>::new());
                        }

                        conflict_map.find_mut(var1).unwrap().insert(*var2);
                        conflict_map.find_mut(var2).unwrap().insert(*var1);
                    }
                }
            }

            for used_var in info.used.iter().chain(info.def.iter()) {
                let new_count = counts.find(used_var).unwrap_or(&0) + 1;
                counts.insert(*used_var, new_count);
            }
        }

        (conflict_map, counts)
    }
}
