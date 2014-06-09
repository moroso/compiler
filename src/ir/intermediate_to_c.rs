use ast::*;
use ir::*;
use util::Name;
use session::Interner;
use std::collections::{TreeMap, SmallIntMap, TreeSet};

pub struct IRToC;

fn print_var(interner: &Interner, v: &Var) -> String {
    format!("{}_{}",
            interner.name_to_str(&v.name),
            match v.generation {
                Some(ref g) => format!("{}", g),
                None => format!(""),
            }
            )
}

fn print_lvalue(interner: &Interner, lv: &LValue) -> String {
    match *lv {
        VarLValue(ref v) => print_var(interner, v),
        PtrLValue(ref v) => format!("*{}", print_var(interner, v)),
    }
}

fn print_rvalelem(interner: &Interner, rve: &RValueElem) -> String {
    match *rve {
        Variable(ref v) => print_var(interner, v),
        Constant(ref l) => format!("{}", l),
    }
}

fn print_rvalue(interner: &Interner, rv: &RValue) -> String {
    match *rv {
        BinOpRValue(ref op, ref v1, ref v2) => {
            format!("{} {} {}",
                    print_rvalelem(interner, v1),
                    op,
                    print_rvalelem(interner, v2)
                    )
        },
        UnOpRValue(ref op, ref v) => {
            format!("{} {}",
                    op,
                    print_rvalelem(interner, v)
                    )
        },
        DirectRValue(ref v) => print_rvalelem(interner, v)
    }
}

fn assign_vars(interner: &Interner,
               label: &TreeMap<Name, uint>,
               vars: &TreeSet<Var>) -> String {
    let mut s = "".to_string();
    for var in vars.iter() {
        let new_var = Var {
            name: var.name.clone(),
            generation: Some(*label.find(&var.name).unwrap())
        };
        s = s.append(format!("  {} = {};\n",
                             print_var(interner, &new_var),
                             print_var(interner, var)).as_slice());
    }
    s
}

impl IRToC {
    pub fn convert_function(interner: &Interner, ops: &Vec<Op>) -> String {
        let mut s = "".to_string();
        let mut vars = TreeSet::new();
        let mut labels: SmallIntMap<TreeMap<Name, uint>> = SmallIntMap::new();
        for op in ops.iter() {
            match *op {
                Assign(ref lv, _) => {
                    match *lv {
                        VarLValue(ref v) => {
                            vars.insert(v);
                        },
                        _ => {},
                    }
                },
                Label(ref idx, ref vars) => {
                    let mut varmap: TreeMap<Name, uint> = TreeMap::new();
                    for var in vars.iter() {
                        varmap.insert(var.name.clone(),
                                      var.generation.unwrap());
                    }
                    labels.insert(*idx,
                                  varmap);
                }
                _ => {},
            }
        }

        for var in vars.iter() {
            s = s.append(format!("  int {};\n",
                                 print_var(interner, *var)).as_slice());
        }

        for op in ops.iter() {
            s = s.append(match *op {
                Assign(ref lv, ref rv) => {
                    format!("  {} = {};\n",
                            print_lvalue(interner, lv),
                            print_rvalue(interner, rv))
                },
                Label(ref l, _) => {
                    // TODO: correct assignments of variables in labels and
                    // gotos.
                    format!("LABEL{}:\n", l)
                },
                Goto(ref l, ref vars) => {
                    format!("{}  goto LABEL{};\n",
                            assign_vars(interner,
                                        labels.get(l),
                                        vars),
                            l)
                },
                CondGoto(ref rve, ref l, _) => {
                    format!("  if ({}) goto LABEL{};\n",
                            print_rvalelem(interner, rve),
                            l)
                }
                _ => format!(""),
            }.as_slice());
        }
        s.append("  0;\n")
    }
}