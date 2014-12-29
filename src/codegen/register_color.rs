/* Register coloring. */

use mas::ast::Reg;
use ir::*;
use std::collections::{BTreeMap, BTreeSet};
use util::Name;
use codegen::*;

pub struct RegisterColorer;

impl RegisterColorer {
    /// Greedy register allocator, favoring variables that are used more often.
    pub fn color(conflicts: BTreeMap<Var, BTreeSet<Var>>,
                 frequencies: BTreeMap<Var, u32>,
                 must_colors: BTreeMap<Var, RegisterColor>,
                 mem_vars: BTreeSet<Name>,
                 global_map: &BTreeMap<Name, StaticIRItem>,
                 num_colors: uint
                 ) -> BTreeMap<Var, RegisterColor> {
        let mut coloring: BTreeMap<Var, RegisterColor> =
            FromIterator::from_iter(must_colors.move_iter());
        // Make a list of all variables that have to go on the stack.
        // (mem_vars contains all variables that need to go in memory,
        // but some of these are global).
        let new_mem_vars = mem_vars.move_iter().filter(
            |name| global_map.find(name).is_none());
        let mem_locs: BTreeMap<Name, uint> = FromIterator::from_iter(
            new_mem_vars.enumerate().map(|(x,y)| (y,x)));

        let mut freq_vec: Vec<(&Var, &u32)> = frequencies.iter().collect();
        freq_vec.sort_by(|&(_, a), &(_, b)| b.cmp(a));

        // First, decide what must go on the stack..
        for &(var, _) in freq_vec.iter() {
            let maybe_pos = mem_locs.find(&var.name);
            match maybe_pos {
                // We don't want to override any "must colors", but otherwise want to put every generation
                // of a given variable in the same place on the stack.
                Some(i) if coloring.find(var).is_none() => { coloring.insert(var.clone(),
                                                                             StackColor(*i as int)); },
                _ => {},
            }
        }

        for (var, _) in freq_vec.move_iter() {
            let global_info = global_map.find(&var.name);
            match global_info {
                Some(ref info) => {
                    // It's a global variable. No work to do!
                    assert!(
                        coloring.find(var).is_none(),
                        format!("Already colored a global variable {} as {}",
                                var, coloring.find(var)));
                    // Global functions get registers.
                    if !info.is_func {
                        // Anything else gets the global color.
                        coloring.insert(*var, GlobalColor);
                        continue;
                    }
                },
                _ => {},
            }
            let empty_treeset = BTreeSet::<Var>::new();
            let ref adjacent_vars =
                conflicts
                .find(var)
                .unwrap_or(&empty_treeset);
            let adjacent_colors: BTreeSet<Option<RegisterColor>>
                = FromIterator::from_iter(adjacent_vars.iter()
                                          .map(|var|
                                               coloring
                                               .find(var)
                                               .map(|&x|x)
                                               ));

            // If we already have a coloring, make sure that it hasn't
            // created any inconsistencies.
            let cur_color = coloring.find(var).map(|&x|x);
            if cur_color.is_some() {
                assert!(!adjacent_colors.contains(&cur_color),
                        "var {} has an adjacent color {}", var, cur_color);
                continue;
            }

            let mut i = 0i;
            loop {
                if !adjacent_colors.contains(&Some(StackColor(i))) {
                    break;
                }
                i+=1;
            }

            let mut color = StackColor(i);

            for n in range(0, num_colors) {
                let n = n as u8;
                // No matter what, we're not allowed to assign these registers.
                if n == spill_reg_base ||
                    n == spill_reg_base+1 ||
                    n == spill_reg_base+2 ||
                    n == link_register.index ||
                    n == stack_pointer.index ||
                    n == global_reg.index
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
    use ir::*;
    use std::collections::{BTreeMap, BTreeSet};
    use util::Name;
    use mas::ast::Reg;
    use codegen::*;

    fn var(n: u32) -> Var {
        Var { name: Name(n as uint), generation: Some(1) }
    }

    #[test]
    fn test_color_with_no_conflicts() {
        let conflicts: BTreeMap<Var, BTreeSet<Var>> = BTreeMap::new();
        let mut frequencies: BTreeMap<Var, u32> = BTreeMap::new();

        for n in range(0u32, 10) {
            frequencies.insert(var(n), 1);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies,
                                              BTreeMap::new(),
                                              BTreeSet::new(),
                                              &BTreeMap::<Name,
                                                         StaticIRItem>::new(),
                                              10);
        for (idx, (_, &color)) in coloring.iter().enumerate() {
            assert_eq!(color, RegColor(Reg { index: 0 as u8 } ));
        }
    }

    #[test]
    fn test_color_with_all_conflicts() {
        let mut conflicts: BTreeMap<Var, BTreeSet<Var>> = BTreeMap::new();
        let mut frequencies: BTreeMap<Var, u32> = BTreeMap::new();

        for n in range(0u32, 20) {
            frequencies.insert(var(n), (25 - n) as u32);

            let mut conflict_set = BTreeSet::<Var>::new();
            for m in range(0u32, 20) {
                if n != m {
                    conflict_set.insert(var(m));
                }
            }
            conflicts.insert(var(n), conflict_set);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies,
                                              BTreeMap::new(),
                                              BTreeSet::new(),
                                              &BTreeMap::<Name,
                                                         StaticIRItem>::new(),
                                              10);
        for i in range(0u32, 8) {
            let color = *coloring.find(&var(i)).unwrap();
            assert_eq!(color, RegColor(Reg { index: i as u8 } ));
        }

        for i in range(8u32, 20) {
            let color = *coloring.find(&var(i)).unwrap();
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

        for n in range(0u32, 20) {
            frequencies.insert(var(n), (25 - n) as u32);

            let mut conflict_set = BTreeSet::<Var>::new();
            // Each variable conflicts with the adjacent ones.
            for m in range(0u32, 20) {
                if n - m == 1 || m - n == 1 {
                    conflict_set.insert(var(m));
                }
            }
            conflicts.insert(var(n), conflict_set);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies,
                                              BTreeMap::new(),
                                              BTreeSet::new(),
                                              &BTreeMap::<Name,
                                                         StaticIRItem>::new(),
                                              10);
        for i in range(0u32, 20) {
            let color = *coloring.find(&var(i)).unwrap();
            assert_eq!(color, RegColor(Reg { index: (i%2) as u8 } ));
        }
    }
}
