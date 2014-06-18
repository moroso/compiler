/* Register coloring. */

use mas::ast::Reg;
use ir::*;
use std::collections::{TreeMap, TreeSet};

#[deriving(Ord, PartialOrd, PartialEq, Eq, Show)]
pub enum RegisterColor {
    RegColor(Reg),
    Spilled,
}

pub struct RegisterColorer;

impl RegisterColorer {
    /// Greedy register allocator, favoring variables that are used more often.
    pub fn color(conflicts: TreeMap<Var, TreeSet<Var>>,
                 frequencies: TreeMap<Var, u32>,
                 num_colors: uint
                 ) -> TreeMap<Var, RegisterColor> {
        let mut coloring = TreeMap::<Var, RegisterColor>::new();

        let mut freq_vec: Vec<(&Var, &u32)> = frequencies.iter().collect();
        freq_vec.sort_by(|&(_, a), &(_, b)| b.cmp(a));

        for (var, _) in freq_vec.move_iter() {
            let empty_treeset = TreeSet::<Var>::new();
            let ref adjacent_vars =
                conflicts
                .find(var)
                .unwrap_or(&empty_treeset);
            let adjacent_colors: TreeSet<RegisterColor>
                = FromIterator::from_iter(adjacent_vars.iter()
                                          .map(|var|
                                               *coloring
                                               .find(var)
                                               .unwrap_or(
                                                   &Spilled)
                                               ));

            let mut color = Spilled;

            for n in range(0, num_colors) {
                if !adjacent_colors.contains(
                    &RegColor(Reg { index: n as u8 } )
                        ) {
                    color = RegColor(Reg { index: n as u8 } );
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
    use std::collections::{TreeMap, TreeSet};
    use util::Name;
    use mas::ast::Reg;

    fn var(n: int) -> Var {
        Var { name: Name(n as uint), generation: Some(1) }
    }

    #[test]
    fn test_color_with_no_conflicts() {
        let conflicts: TreeMap<Var, TreeSet<Var>> = TreeMap::new();
        let mut frequencies: TreeMap<Var, u32> = TreeMap::new();

        for n in range(0, 10) {
            frequencies.insert(var(n), 1);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies, 10);
        for (_, &color) in coloring.iter() {
            assert_eq!(color, RegColor(Reg { index: 0 } ));
        }
    }

    #[test]
    fn test_color_with_all_conflicts() {
        let mut conflicts: TreeMap<Var, TreeSet<Var>> = TreeMap::new();
        let mut frequencies: TreeMap<Var, u32> = TreeMap::new();

        for n in range(0, 20) {
            frequencies.insert(var(n), (25 - n) as u32);

            let mut conflict_set = TreeSet::<Var>::new();
            for m in range(0, 20) {
                if n != m {
                    conflict_set.insert(var(m));
                }
            }
            conflicts.insert(var(n), conflict_set);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies, 10);
        for i in range(0, 10) {
            let color = *coloring.find(&var(i)).unwrap();
            assert_eq!(color, RegColor(Reg { index: i as u8 } ));
        }

        for i in range(10, 20) {
            let color = *coloring.find(&var(i)).unwrap();
            assert_eq!(color, Spilled);
        }
    }

    #[test]
    fn test_color_with_some_conflicts() {
        let mut conflicts: TreeMap<Var, TreeSet<Var>> = TreeMap::new();
        let mut frequencies: TreeMap<Var, u32> = TreeMap::new();

        for n in range(0, 20) {
            frequencies.insert(var(n), (25 - n) as u32);

            let mut conflict_set = TreeSet::<Var>::new();
            // Each variable conflicts with the adjacent ones.
            for m in range(0, 20) {
                if n - m == 1 || m - n == 1 {
                    conflict_set.insert(var(m));
                }
            }
            conflicts.insert(var(n), conflict_set);
        }

        let coloring = RegisterColorer::color(conflicts, frequencies, 10);
        for i in range(0, 20) {
            let color = *coloring.find(&var(i)).unwrap();
            assert_eq!(color, RegColor(Reg { index: (i%2) as u8 } ));
        }
    }
}
