use package::Package;

use mc::lexer::Lexer;
use mc::parser::Parser;
use mc::session::Interner;

use util::Name;

use ir::liveness::LivenessAnalyzer;
use ir::ast_to_intermediate::ASTToIntermediate;
use ir::constant_fold::ConstantFolder;
use ir::ssa::ToSSA;

use super::Target;

use std::collections::{TreeMap, SmallIntMap, TreeSet};
use std::io::stdio;

use std::io;

use mc::ast::*;
use ir::*;

pub struct IRTarget;

fn print_var(interner: &Interner, v: &Var) -> String {
    format!("{}{}",
            interner.name_to_str(&v.name),
            match v.generation {
                Some(ref g) if *g > 0 => format!("_{}", g),
                _ => format!(""),
            }
            )
}

fn print_lvalue(interner: &Interner, lv: &LValue) -> String {
    match *lv {
        VarLValue(ref v) => print_var(interner, v),
        PtrLValue(ref v) => format!("*(int*){}", print_var(interner, v)),
    }
}

fn print_rvalelem(interner: &Interner, rve: &RValueElem) -> String {
    match *rve {
        Variable(ref v) => print_var(interner, v),
        Constant(ref l) => {
            match *l {
                NumLit(n, _) => format!("{}", n),
                _ => fail!("Non-integer literals not yet supported."),
            }
        }
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
            match *op {
                Deref => {
                    format!("*((int*){})",
                            print_rvalelem(interner, v))
                },
                _ => {
                    format!("{} {}",
                            op,
                            print_rvalelem(interner, v)
                            )
                }
            }
        },
        DirectRValue(ref v) => print_rvalelem(interner, v),
        CallRValue(ref v, ref args) => {
            let mut s = format!("((int (*)()){})(", print_rvalelem(interner, v));
            let list: Vec<String> = args.iter()
                .map(|arg| print_rvalelem(interner, arg)).collect();
            s = s.append(list.connect(", ").as_slice());
            s = s.append(")");
            s
        }
        AllocaRValue(ref size) => {
            format!("alloca({})", size)
        }
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

impl IRTarget {
    fn convert_function(&self, interner: &Interner, ops: &Vec<Op>) -> String {
        let mut s = "".to_string();
        let mut vars = TreeSet::new();
        let mut labels: SmallIntMap<TreeMap<Name, uint>> = SmallIntMap::new();
        let opinfo = LivenessAnalyzer::analyze(ops);
        // Find all variables we need to declare. This is all variables
        // that are defined anywhere, except in the very first instruction
        // (which must be a function definition instruction).
        for op in opinfo.iter().skip(1) {
            for var in op.def.iter() {
                vars.insert(var);
            }
        }

        // Find out which variables are used at each label.
        for op in ops.iter() {
            match *op {
                Label(ref idx, ref vars) => {
                    let mut varmap: TreeMap<Name, uint> = TreeMap::new();
                    for var in vars.iter() {
                        varmap.insert(var.name.clone(),
                                      var.generation.unwrap());
                    }
                    labels.insert(*idx, varmap);
                }
                _ => {}
            }
        }

        // Do the actual conversion.
        for op in ops.iter() {
            s = s.append(match *op {
                Assign(ref lv, ref rv) => {
                    format!("  {} = (long)({});\n",
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
                },
                Return(ref rve) => {
                    format!("  return {};\n", print_rvalelem(interner, rve))
                },
                Func(ref name, ref args) => {
                    let mapped_args: Vec<String> = args.iter()
                        .map(|arg| format!("long {}", print_var(interner, arg)))
                        .collect();
                    let mut s = format!("");
                    for var in vars.iter() {
                        s = s.append(format!("  long {};\n",
                                             print_var(interner, *var))
                                     .as_slice());
                    }


                    format!("int {}({}) {{\n{}",
                            interner.name_to_str(name),
                            mapped_args.connect(", "),
                            s)
                }
                _ => format!(""),
            }.as_slice());
        }
        s.append("}\n")
    }
}

impl Target for IRTarget {
    fn new(_args: Vec<String>) -> IRTarget {
        IRTarget
    }

    fn compile(&self, p: Package) {
        let Package {
            module:  module,
            session: mut session,
            typemap: mut typemap,
        } = p;

        let mut result =
        {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap);

            let mut result = vec!();

            for item in module.val.items.iter() {
                print!("{}\n", item);
                let (insts, _) = converter.convert_item(item);
                print!("{}\n\n", insts);
                result.push(insts);
            }
            result
        };

        for insts in result.mut_iter() {
            ToSSA::to_ssa(insts);
            ConstantFolder::fold(insts);
            print!("{}\n", insts);
            for a in LivenessAnalyzer::analyze(insts).iter() {
                print!("{}\n", a);
            }
            print!("{}\n", self.convert_function(&session.interner, insts));
        }
    }
}
