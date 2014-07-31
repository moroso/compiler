use package::Package;

use mc::lexer::Lexer;
use mc::parser::Parser;
use mc::session::Interner;

use util::{Name, Width32, Width16, Width8, AnyWidth};

use ir::liveness::LivenessAnalyzer;
use ir::ast_to_intermediate::ASTToIntermediate;
use ir::constant_fold::ConstantFolder;
use ir::ssa::ToSSA;

use super::Target;

use target::util::NameMangler;

use std::collections::{TreeMap, SmallIntMap, TreeSet};
use std::io::stdio;
use std::local_data::Ref;

use std::io;

use mc::ast::*;
use ir::*;

pub struct IRTarget {
    verbose: bool,
}

fn print_var(interner: &Ref<Interner>,
             global_map: &TreeMap<Name, StaticIRItem>,
             v: &Var) -> String {
    format!("{}{}",
            interner.name_to_str(&v.name),
            match v.generation {
                Some(ref g) if *g > 0 =>
                    if global_map.find(&v.name).is_none() {
                        format!("_{}", g)
                    } else {
                        format!("")
                    },
                _ => format!(""),
            }
            )
}

fn print_lvalue(interner: &Ref<Interner>,
                global_map: &TreeMap<Name, StaticIRItem>,
                lv: &LValue) -> String {
    match *lv {
        VarLValue(ref v) => print_var(interner, global_map, v),
        PtrLValue(ref v) => format!("*(int*){}", print_var(interner,
                                                           global_map, v)),
    }
}

fn print_rvalelem(interner: &Ref<Interner>,
                  global_map: &TreeMap<Name, StaticIRItem>,
                  rve: &RValueElem) -> String {
    match *rve {
        Variable(ref v) => print_var(interner, global_map, v),
        Constant(ref l) => {
            match *l {
                NumLit(n, _) => format!("{}", n),
                NullLit => format!("NULL"),
                BoolLit(true) => format!("1"),
                BoolLit(false) => format!("0"),
                StringLit(ref s) => format!("\"{}\"", s),
            }
        }
    }
}

fn assign_vars(interner: &Ref<Interner>,
               global_map: &TreeMap<Name, StaticIRItem>,
               label: &TreeMap<Name, uint>,
               vars: &TreeSet<Var>) -> String {
    let mut s = "".to_string();
    for var in vars.iter() {
        let new_var = Var {
            name: var.name.clone(),
            generation: Some(*label.find(&var.name).unwrap())
        };
        s = s.append(format!("  {} = {};\n",
                             print_var(interner, global_map, &new_var),
                             print_var(interner, global_map, var)).as_slice());
    }
    s
}

impl IRTarget {
    fn convert_function(&self, interner: &Ref<Interner>,
                        ops: &Vec<Op>,
                        global_map: &TreeMap<Name, StaticIRItem>) -> String {
        if ops.len() == 0 { return String::from_str("") }

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
        // TODO: This is a hack, until "return" takes an option.
        for op in ops.iter() {
            match *op {
                Return(Variable(ref v)) => { vars.insert(v); },
                _ => {}
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

        let mut in_func = false;

        // Do the actual conversion.
        for op in ops.iter() {
            s = s.append(match *op {
                BinOp(ref v, ref op, ref rv1, ref rv2) => {
                    format!("  {} = (long)(({}) {} ({}));\n",
                            print_var(interner, global_map, v),
                            print_rvalelem(interner, global_map, rv1),
                            op,
                            print_rvalelem(interner, global_map, rv2))
                },
                UnOp(ref v, ref op, ref rv) => {
                    match *op {
                        Deref =>
                            format!("  {} = (long)({} (long*)({}));\n",
                                    print_var(interner, global_map, v),
                                    op,
                                    print_rvalelem(interner, global_map, rv)),
                        _ =>
                            format!("  {} = (long)({} ({}));\n",
                                    print_var(interner, global_map, v),
                                    op,
                                    print_rvalelem(interner, global_map, rv))
                    }
                },
                Alloca(ref v, ref size) => {
                    format!("  {} = (long)(alloca({}));\n",
                            print_var(interner, global_map, v), size)
                },
                Call(ref v, ref fname, ref args) => {
                    let mut s = match *fname {
                        Variable(ref fnv) =>
                            format!("  {} = ((long (*)()){})(",
                                    print_var(interner, global_map, v),
                                    print_var(interner, global_map, fnv),
                                    ),
                        _=> unimplemented!(),
                    };
                    let list: Vec<String> = args.iter()
                        .map(|arg| print_var(interner, global_map, arg)
                             ).collect();
                    s = s.append(list.connect(", ").as_slice());
                    s = s.append(");\n");
                    s
                },
                Load(ref l, ref r, ref size) => {
                    format!("  {} = (long)*({}*)({});\n",
                            print_var(interner, global_map, l),
                            match *size {
                                AnyWidth |
                                Width32 => "uint32_t",
                                Width16 => "uint16_t",
                                Width8 => "uint8_t",
                            },
                            print_var(interner, global_map, r))
                },
                Store(ref l, ref r, ref size) => {
                    format!("  *({}*)({}) = {};\n",
                            match *size {
                                AnyWidth |
                                Width32 => "uint32_t",
                                Width16 => "uint16_t",
                                Width8 => "uint8_t",
                            },
                            print_var(interner, global_map, l),
                            print_var(interner, global_map, r))
                },
                Nop => format!(""),
                Label(ref l, _) => {
                    // TODO: correct assignments of variables in labels and
                    // gotos.
                    format!("LABEL{}:\n", l)
                },
                Goto(ref l, ref vars) => {
                    format!("{}  goto LABEL{};\n",
                            assign_vars(interner,
                                        global_map,
                                        labels.get(l),
                                        vars),
                            l)
                },
                CondGoto(ref negated, ref rve, ref l, _) => {
                    format!("  if ({}({})) goto LABEL{};\n",
                            if *negated { "!" } else { "" },
                            print_rvalelem(interner, global_map, rve),
                            l)
                },
                Return(ref rve) => {
                    format!("  return {};\n", print_rvalelem(interner,
                                                             global_map,
                                                             rve))
                },
                Func(ref name, ref args) => {
                    let mapped_args: Vec<String> = args.iter()
                        .map(|arg| format!("long {}", print_var(interner,
                                                                global_map,
                                                                arg)))
                        .collect();
                    let mut s = format!("");
                    for var in vars.iter() {
                        if global_map.find(&var.name).is_none() {
                            s = s.append(format!("  long {};\n",
                                                 print_var(interner,
                                                           global_map,
                                                           *var))
                                         .as_slice());
                        }
                    }

                    let closer = if in_func { "}" } else { "" };

                    in_func = true;

                    format!("{}\nlong {}({}) {{\n{}",
                            closer,
                            interner.name_to_str(name),
                            mapped_args.connect(", "),
                            s)
                }
                //_ => format!(""),
            }.as_slice());
        }
        s.append("}\n")
    }
}

impl Target for IRTarget {
    fn new(args: Vec<String>) -> IRTarget {
        let mut verbose = false;
        for arg in args.iter() {
            if *arg == String::from_str("verbose") {
                print!("Enabling verbose mode.\n");
                verbose = true;
            }
        }
        IRTarget { verbose: verbose }
    }

    #[allow(unused_must_use)]
    fn compile(&self, p: Package, f: &mut io::Writer) {
        let Package {
            module:  module,
            session: session,
            typemap: mut typemap,
        } = p;

        let mangler = NameMangler::new(session, &module, true, false);
        let mut session = mangler.session;

        let (mut result, mut staticitems) = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap,
                                                       &mangler.names);

            if self.verbose {
                print!("{}\n", module);
            }

            converter.convert_module(&module)
        };

        if self.verbose {
            for res in result.iter() {
                write!(f, "{}\n\n", res);
            }
        }

        println!("{}", "#include <stdio.h>");
        println!("{}", "#include <stdlib.h>");
        println!("{}", "#include <stdint.h>");
        println!("{}", "#include <assert.h>");
        println!("{}", "#include <string.h>");
        println!("{}", "typedef unsigned int uint_t;");
        println!("{}", "typedef int int_t;");

        println!("{}", "long printf0_(uint8_t *s) { return printf(\"%s\", (char *)s); }");
        println!("{}", "long printf1_(uint8_t *s, ulong a) { return printf((char *)s, a); }");
        println!("{}", "long printf2_(uint8_t *s, ulong a, ulong b) { return printf((char *)s, a, b); }");
        println!("{}", "long printf3_(uint8_t *s, ulong a, ulong b, ulong c) { return printf((char *)s, a, b, c); }");
        println!("{}", "long print_int(long x) { printf(\"%d\\n\", (int)x); return x; }");
        println!("{}", "long print_char(long x) { printf(\"%c\", (int)x); return x; }");

        // Print function prototypes.
        for insts in result.iter() {
            for inst in insts.iter() {
                match *inst {
                    Func(ref name, _) => {
                        write!(f, "long {}();\n",
                               session.interner.name_to_str(name));
                    },
                    _ => {}
                }
            }
        }

        // TODO: this is a hack. Eventually we should extract names from labels
        // in any included asm files.
        let builtin_staticitems = vec!("print_char",
                                       "print_int",
                                       "printf0_",
                                       "printf1_",
                                       "printf2_",
                                       "printf3_",
                                       "assert")
            .iter()
            .map(|x|
                 StaticIRItem {
                     name: session.interner.intern(
                         x.to_string()),
                     size: 0,
                     offset: None,
                     is_ref: false,
                     is_func: true,
                     expr: None,
                 }).collect();
        staticitems.push_all_move(builtin_staticitems);

        let global_map = ASTToIntermediate::allocate_globals(staticitems);
        let global_initializer = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap,
                                                       &mangler.names);

            converter.convert_globals(&global_map)
        };
        result.push(global_initializer);

        // Print global definitions.
        for (_, item) in global_map.iter() {
            if !item.is_func {
                if item.is_ref {
                    println!("char {}[{}];", item.name, item.size);
                } else {
                    println!("long {};", item.name);
                }
            }
        }

        for insts in result.mut_iter() {
            ToSSA::to_ssa(insts, self.verbose);
            if self.verbose {
                write!(f, "{}\n", insts);
            }
            ConstantFolder::fold(insts, &global_map, self.verbose);
            if self.verbose {
                write!(f, "{}\n", insts);
                for a in LivenessAnalyzer::analyze(insts).iter() {
                    write!(f, "{}\n", a);
                }
            }
            write!(f, "{}\n", self.convert_function(&session.interner, insts,
                                                    &global_map));
        }

        println!("{}", "void main() { __INIT_GLOBALS(); MANGLEDmain(); }");
    }
}
