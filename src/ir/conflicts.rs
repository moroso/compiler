use ir::*;
use std::collections::{BTreeMap, BTreeSet};
use util::Name;
use mc::ast::AddrOf;
use codegen::*;
use mas::ast::Reg;

pub struct ConflictAnalyzer;

impl ConflictAnalyzer {
    /// Analyze a block of IR code, and return a map of conflicting registers
    /// as well as how many times each variable is used.
    /// Also return a list of variables that must be in memory rather than
    /// in registers, because they are referenced.
    /// Assumes the IR is already in SSA form.
    pub fn conflicts(ops: &Vec<Op>,
                     opinfo: &Vec<OpInfo>
                     ) -> (BTreeMap<Var, BTreeSet<Var>>,
                           BTreeMap<Var, u32>,
                           BTreeMap<Var, RegisterColor>,
                           // This is a set of Names, because
                           // we spill *every* generation of
                           // the variable.
                           BTreeSet<Name>) {
        let mut conflict_map = BTreeMap::<Var, BTreeSet<Var>>::new();
        let mut counts = BTreeMap::<Var, u32>::new();
        let mut referenced_vars = BTreeSet::<Name>::new();
        let mut must_colors = BTreeMap::new();

        for op in ops.iter() {
            match *op {
                Op::UnOp(_, AddrOf, ref rve) => {
                    match *rve {
                        Variable(ref v) => { referenced_vars.insert(v.name); },
                        _ => panic!("Should have a variable here."),
                    }
                },
                Op::Call(ref v, _, ref args) => {
                    for (i, arg) in args.iter().enumerate()
                        .take(num_param_regs)
                    {
                        must_colors.insert(*arg,
                                           RegColor(Reg { index: i as u8 }));
                    }
                    must_colors.insert(*v, RegColor(Reg { index: 0 as u8 }));
                },
                Op::Func(_, ref args, is_extern) => {
                    if is_extern { break; }
                    for (i, arg) in args.iter().enumerate()
                        .take(num_param_regs)
                    {
                        must_colors.insert(*arg,
                                           RegColor(Reg { index: i as u8 }));
                    }
                    for i in num_param_regs .. args.len() {
                        must_colors.insert(args[i],
                                           StackColor((i as isize) - 1 -
                                                      (args.len() as isize)));
                    }
                }
                _ => {}
            }
        }

        for info in opinfo.iter() {
            for var1 in info.live.iter() {
                for var2 in info.live.iter() {
                    if var1 != var2 {
                        if !conflict_map.contains_key(var1) {
                            conflict_map.insert(*var1, BTreeSet::<Var>::new());
                        }
                        if !conflict_map.contains_key(var2) {
                            conflict_map.insert(*var2, BTreeSet::<Var>::new());
                        }

                        conflict_map.get_mut(var1).unwrap().insert(*var2);
                        conflict_map.get_mut(var2).unwrap().insert(*var1);
                    }
                }
            }

            // If we define a variable somewhere, it conflicts with
            // all variables that are live in instructions immediately
            // following it.
            for succ in info.succ.iter() {
                let ref succinfo = opinfo[*succ];
                for var1 in succinfo.live.iter() {
                    for var2 in info.def.iter() {
                        if var1 != var2 {
                            if !conflict_map.contains_key(var1) {
                                conflict_map.insert(*var1, BTreeSet::<Var>::new());
                            }
                            if !conflict_map.contains_key(var2) {
                                conflict_map.insert(*var2, BTreeSet::<Var>::new());
                            }

                            conflict_map.get_mut(var1).unwrap().insert(*var2);
                            conflict_map.get_mut(var2).unwrap().insert(*var1);
                        }
                    }
                }
            }

            for used_var in info.used.iter().chain(info.def.iter()) {
                let new_count = *counts.get(used_var).unwrap_or(&0) + 1;
                counts.insert(*used_var, new_count);
            }
        }

        (conflict_map, counts, must_colors, referenced_vars)
    }
}
