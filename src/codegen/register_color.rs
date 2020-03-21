/* Register coloring. */

use mas::ast::Reg;
use ir::*;
use std::collections::{BTreeMap, BTreeSet};
use std::iter::FromIterator;
use codegen::*;

pub struct RegisterColorer;

impl RegisterColorer {
    /// Greedy register allocator, favoring variables that are used more often.
    pub fn color(conflicts: BTreeMap<Var, BTreeSet<Var>>,
                 frequencies: BTreeMap<Var, u32>,
                 must_colors: BTreeMap<Var, RegisterColor>,
                 mem_vars: BTreeSet<VarName>,
                 global_map: &BTreeMap<VarName, StaticIRItem>,
                 num_colors: usize
                 ) -> BTreeMap<Var, RegisterColor> {
        let mut coloring: BTreeMap<Var, RegisterColor> =
            FromIterator::from_iter(must_colors.into_iter());
        // Make a list of all variables that have to go on the stack.
        // (mem_vars contains all variables that need to go in memory,
        // but some of these are global).
        let new_mem_vars = mem_vars.into_iter().filter(
            |name| global_map.get(name).is_none());
        let mem_locs: BTreeMap<VarName, usize> = FromIterator::from_iter(
            new_mem_vars.enumerate().map(|(x,y)| (y,x)));

        let mut freq_vec: Vec<(&Var, &u32)> = frequencies.iter().collect();
        freq_vec.sort_by(|&(_, a), &(_, b)| b.cmp(a));

        // First, decide what must go on the stack..
        for &(var, _) in &freq_vec {
            let maybe_pos = mem_locs.get(&var.name);
            match maybe_pos {
                // We don't want to override any "must colors", but otherwise want to put every generation
                // of a given variable in the same place on the stack.
                Some(i) if coloring.get(var).is_none() => { coloring.insert(*var,
                                                                             StackColor(*i as isize)); },
                _ => {},
            }
        }

        for (var, _) in freq_vec {
            let global_info = global_map.get(&var.name);
            if global_info.is_some() {
                // It's a global variable. No work to do!
                assert!(
                    coloring.get(var).is_none(),
                    format!("Already colored a global variable {} as {}",
                            var, coloring[var]));
                coloring.insert(*var, GlobalColor);
                continue;
            }
            let empty_treeset = BTreeSet::<Var>::new();
            let adjacent_vars =
                conflicts
                .get(var)
                .unwrap_or(&empty_treeset);
            let adjacent_colors: BTreeSet<Option<RegisterColor>>
                = FromIterator::from_iter(adjacent_vars.iter()
                                          .map(|var|
                                               coloring
                                               .get(var)
                                               .cloned()
                                               ));

            // If we already have a coloring, make sure that it hasn't
            // created any inconsistencies.
            let cur_color = coloring.get(var).cloned();
            if cur_color.is_some() {
                assert!(!adjacent_colors.contains(&cur_color),
                        "var {} has an adjacent color {}", var, cur_color.unwrap());
                continue;
            }

            let mut i = 0;
            loop {
                if !adjacent_colors.contains(&Some(StackColor(i))) {
                    break;
                }
                i+=1;
            }

            let mut color = StackColor(i);

            for n in 0 .. num_colors {
                let n = n as u8;
                // No matter what, we're not allowed to assign these registers.
                if n == SPILL_REG_BASE ||
                    n == SPILL_REG_BASE+1 ||
                    n == SPILL_REG_BASE+2 ||
                    n == LINK_REGISTER.index ||
                    n == STACK_POINTER.index ||
                    n == GLOBAL_REG.index
                {
                    continue;
                }
                if !adjacent_colors.contains(
                    &Some(RegColor(Reg { index: n } ))) {
                    color = RegColor(Reg { index: n } );
                    break;
                }
            }

            coloring.insert(*var, color);
        }

        coloring
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    use std::collections::{BTreeMap, BTreeSet};
    
    use mas::ast::Reg;
    

    fn var(n: u32) -> Var {
        Var { name: VarName::IRTempVariable(n as usize), generation: Some(1) }
    }

    #[test]
    fn test_color_with_no_conflicts() {
        let conflicts: BTreeMap<Var, BTreeSet<Var>> = BTreeMap::new();
        let mut frequencies: BTreeMap<Var, u32> = BTreeMap::new();

        for n in 0u32 .. 10 {
            frequencies.insert(var(n), 1);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies,
                                              BTreeMap::new(),
                                              BTreeSet::new(),
                                              &BTreeMap::<VarName,
                                                         StaticIRItem>::new(),
                                              10);
        for (_, &color) in coloring.iter() {
            assert_eq!(color, RegColor(Reg { index: 0 as u8 } ));
        }
    }

    #[test]
    fn test_color_with_all_conflicts() {
        let mut conflicts: BTreeMap<Var, BTreeSet<Var>> = BTreeMap::new();
        let mut frequencies: BTreeMap<Var, u32> = BTreeMap::new();

        for n in 0u32 .. 20 {
            frequencies.insert(var(n), (25 - n) as u32);

            let mut conflict_set = BTreeSet::<Var>::new();
            for m in 0u32 .. 20 {
                if n != m {
                    conflict_set.insert(var(m));
                }
            }
            conflicts.insert(var(n), conflict_set);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies,
                                              BTreeMap::new(),
                                              BTreeSet::new(),
                                              &BTreeMap::<VarName,
                                                         StaticIRItem>::new(),
                                              10);
        for i in 0u32 .. 8 {
            let color = *coloring.get(&var(i)).unwrap();
            assert_eq!(color, RegColor(Reg { index: i as u8 } ));
        }

        for i in 8u32 .. 20 {
            let color = *coloring.get(&var(i)).unwrap();
            match color {
                StackColor(_) => {},
                _ => assert!(false),
            }
        }
    }

    #[test]
    fn test_color_with_some_conflicts() {
        let mut conflicts: BTreeMap<Var, BTreeSet<Var>> = BTreeMap::new();
        let mut frequencies: BTreeMap<Var, u32> = BTreeMap::new();

        for n in 0u32 .. 20 {
            frequencies.insert(var(n), (25 - n) as u32);

            let mut conflict_set = BTreeSet::<Var>::new();
            // Each variable conflicts with the adjacent ones.
            for m in 0u32 .. 20 {
                if n == 1 + m || m == 1 + n {
                    conflict_set.insert(var(m));
                }
            }
            conflicts.insert(var(n), conflict_set);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies,
                                              BTreeMap::new(),
                                              BTreeSet::new(),
                                              &BTreeMap::<VarName,
                                                         StaticIRItem>::new(),
                                              10);
        for i in 0u32 .. 20 {
            let color = *coloring.get(&var(i)).unwrap();
            assert_eq!(color, RegColor(Reg { index: (i%2) as u8 } ));
        }
    }
}
