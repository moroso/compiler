use package::Package;

use mc::lexer::Lexer;
use mc::parser::Parser;
use mc::session::Interner;
use mc::ast::NodeId;

use super::{MkTarget,Target};

use ir::liveness::LivenessAnalyzer;
use ir::ast_to_intermediate::ASTToIntermediate;
use ir::constant_fold::ConstantFolder;
use ir::multiply_optimizer::MultiplyOptimizer;
use ir::ssa::ToSSA;
use ir::conflicts::ConflictAnalyzer;
use ir::{IrNodeId, Op, OpInfo, OpNode, StaticIRItem, Var, VarName};
use ir::dead_code::DeadCodeEliminator;
use ir::inliner::Inliner;

use target::NameMangler;
use target::debug_info::write_debug_file;
use target::util::print_bin;

use codegen::RegisterColor;
use codegen::register_color::RegisterColorer;
use codegen::NUM_USABLE_VARS;
use codegen::IrToAsm;
use codegen::combine::link;

use mas::labels::{LabelInfo, resolve_labels};
use mas::encoder::encode;
use mas::ast::NopInst;
use mas::scheduler::{schedule, schedule_dummy};
use mas::lexer::new_asm_lexer;
use mas::parser::AsmParser;

use util::{Name, align};

use std::io::{Write, BufReader};
use std::fs::File;
use std::path::Path;
use std::collections::{BTreeSet, BTreeMap, BinaryHeap};

// TODO: remove this; we won't need it later.
use mas::ast::InstNode;

#[derive(Eq, PartialEq)]
enum BinaryFormat {
    FlatFormat,
    BSLDFormat,
}

pub struct AsmTarget {
    verbose: bool,
    disable_scheduler: bool,
    disable_inliner: bool,
    list_file: Option<String>,
    debug_file: Option<String>,
    format: BinaryFormat,
    code_start: u32,
    global_start: Option<u32>,
    stack_start: Option<u32>,
    mul_func: Option<String>,
    div_func: Option<String>,
    mod_func: Option<String>,
    const_mul_bit_limit: u8,
}

impl MkTarget for AsmTarget {
    fn new(args: &Vec<(String, Option<String>)>) -> Box<AsmTarget> {
        let mut verbose = false;
        let mut disable_scheduler = false;
        let mut disable_inliner = false;
        let mut list_file = None;
        let mut format = BinaryFormat::FlatFormat;
        let mut code_start = 0;
        let mut stack_start = None;
        let mut global_start = None;
        let mut debug_file = None;
        let mut mul_func = None;
        let mut div_func = None;
        let mut mod_func = None;
        let mut const_mul_bit_limit = 1;

        // TODO: get rid of the unnecessary clones in this function.
        for arg in args.iter() {
            if arg.0 == "verbose".to_string() {
                print!("Enabling verbose mode.\n");
                verbose = true;
            } else if arg.0 == "list" {
                list_file = arg.1.clone();
            } else if arg.0 == "debug" {
                debug_file = arg.1.clone();
            } else if arg.0 == "format" {
                if arg.1 == Some("flat".to_string()) {
                    format = BinaryFormat::FlatFormat;
                } else if arg.1 == Some("bsld".to_string()) {
                    format = BinaryFormat::BSLDFormat;
                } else {
                    panic!("Invalid format! Consider specifying a valid one instead.")
                }
            } else if arg.0 == "code_start" {
                code_start = u32::from_str_radix(&arg.1.clone().unwrap()[..], 16).unwrap();
                if code_start & 0xf != 0 {
                    panic!("Code start is not aligned");
                }
            } else if arg.0 == "stack_start" {
                stack_start = Some(u32::from_str_radix(&arg.1.clone().unwrap()[..], 16).unwrap());
            } else if arg.0 == "global_start" {
                global_start = Some(u32::from_str_radix(&arg.1.clone().unwrap()[..], 16).unwrap());
            } else if arg.0 == "disable_scheduler" {
                disable_scheduler = true;
            } else if arg.0 == "disable_inliner" {
                disable_inliner = true;
            } else if arg.0 == "mul_func" {
                mul_func = arg.1.clone();
            } else if arg.0 == "div_func" {
                div_func = arg.1.clone();
            } else if arg.0 == "mod_func" {
                mod_func = arg.1.clone();
            } else if arg.0 == "const_mul_bit_limit" {
                // TODO: actually implement this functionality.
                const_mul_bit_limit = u8::from_str_radix(&arg.1.clone().unwrap()[..], 10).unwrap();
            }
        }
        Box::new(AsmTarget {
            verbose: verbose,
            list_file: list_file,
            format: format,
            code_start: code_start,
            stack_start: stack_start,
            global_start: global_start,
            disable_scheduler: disable_scheduler,
            disable_inliner: disable_inliner,
            debug_file: debug_file,
            mul_func: mul_func,
            div_func: div_func,
            mod_func: mod_func,
            const_mul_bit_limit: const_mul_bit_limit,
        })
    }
}

impl Target for AsmTarget {
    #[allow(unused_must_use)]
    fn compile(&self, p: Package, f: &mut Write) {
        let Package {
            module,
            mut session,
            mut typemap,
        } = p;

        let mangle_map = NameMangler::get_mangle_map(&mut session, &module, true, false);
        let mut sourcemap = BTreeMap::<IrNodeId, NodeId>::new();

        if self.verbose {
            print!("Mangler: {:?}\n", mangle_map);
        }

        let ((mut result, mut staticitems), max_label) = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap,
                                                       &mangle_map,
                                                       &mut sourcemap);

            (converter.convert_module(&module), converter.next_label())
        };

        // TODO: this is a hack. Eventually we should extract names from labels
        // in any included asm files.
        let asm_staticitems: Vec<StaticIRItem> = vec!("rt_memcpy")
            .iter()
            .map(|x|
                 StaticIRItem {
                     name: VarName::MangledVariable(session.interner.intern(
                         x.to_string())),
                     label: None,
                     size: 0,
                     offset: None,
                     is_ref: false,
                     is_func: true,
                     is_extern: true,
                     expr: None,
                 }).collect();
        staticitems.extend(asm_staticitems.into_iter());

        let global_map = ASTToIntermediate::allocate_globals(&mut session, staticitems);
        if self.verbose {
            print!("Global map: {:?}\n", global_map);
        }
        let global_initializer = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap,
                                                       &mangle_map,
                                                       &mut sourcemap);

            converter.convert_globals(&global_map)
        };
        result.push(global_initializer);

        let prelude_name = match self.format {
            BinaryFormat::BSLDFormat => "prelude_bsld.ma",
            BinaryFormat::FlatFormat => "prelude.ma",
        };
        let prelude_file = {
            let prelude_path = session.options.search_paths.get(&prelude_name.to_string()).unwrap();
            File::open(&prelude_path).unwrap_or_else(|e| panic!("{}", e))
        };

        let prelude_reader = BufReader::new(prelude_file);
        let asm_lexer = new_asm_lexer(prelude_name, prelude_reader);
        let asm_peekable = asm_lexer.peekable();
        let mut asm_parser = AsmParser::new(asm_peekable);
        let (insts, labels) = asm_parser.parse_toplevel();

        let mut items = vec!((insts, labels));

        let strings: BTreeSet<Name> = BTreeSet::new();

        let mut irtoasm = IrToAsm::new(&global_map,
                                       strings);

        let mul_func_name = self.mul_func.clone().map(|x| VarName::MangledVariable(session.interner.intern(x)));
        let div_func_name = self.div_func.clone().map(|x| VarName::MangledVariable(session.interner.intern(x)));
        let mod_func_name = self.mod_func.clone().map(|x| VarName::MangledVariable(session.interner.intern(x)));

        let mut debug_info: BTreeMap<VarName,
                                     (BTreeMap<Var, RegisterColor>,
                                      Vec<Op>,
                                      Vec<OpInfo>,
                                      Vec<usize>,
                                      Vec<InstNode>)> = BTreeMap::new();

        if !self.disable_inliner {
            Inliner::inline(&mut result, max_label, self.verbose);
        }

        for mut insts in result.into_iter() {
            if self.verbose {
                print!("Start conversion!\n");
                print!("{:?}\n", insts);
            }
            let func_name = match insts[0].val {
                OpNode::Func(funcname, _, _) => funcname,
                _ => panic!("Function doesn't start with a 'Func' op!")
            };
            ToSSA::to_ssa(&mut insts, self.verbose);
            ConstantFolder::fold(&mut insts, &global_map, self.verbose);
            MultiplyOptimizer::process(&mut insts, self.verbose,
                                       mul_func_name, div_func_name, mod_func_name,
                                       self.const_mul_bit_limit);
            // Set the last argument to true for debugging issues with the DCE.
            // See the "eliminate" function for details.
            DeadCodeEliminator::eliminate(&mut insts, self.verbose, false);
            if self.verbose {
                print!("Post-optimization:\n");
                for op in insts.iter() {
                    let new_id = sourcemap.get(&op.id);
                    match new_id {
                        Some(id) =>
                            print!("{:?} {:?}\n",
                                   session.parser.spanmap.get(&id),
                                   session.parser.filemap.get(&id)),
                        _ => {}
                    }
                    print!("{}", op.id);
                    print!("{}", op);
                }
            }
            let opinfo = LivenessAnalyzer::analyze(&mut insts);
            if self.verbose {
                for a in opinfo.iter() {
                    print!("{:?}\n", a);
                }
                print!("{:?}\n", insts);
                let (conflict_map, counts, must_colors, mem_vars) =
                    ConflictAnalyzer::conflicts(&mut insts, &opinfo);
                print!("conflicts: {:?}\ncounts: {:?}\nmust: {:?}\nin mem: {:?}\n",
                       conflict_map, counts, must_colors, mem_vars);
                print!("{:?}\n",
                       RegisterColorer::color(conflict_map, counts,
                                              must_colors, mem_vars,
                                              &global_map,
                                              NUM_USABLE_VARS as usize));
            }
            let (asm_insts, labels, coloring, opinfo, correspondence) = irtoasm.ir_to_asm(
                &mut insts, &mut session);

            if self.verbose {
                for (pos, inst) in asm_insts.iter().enumerate() {
                    for (k, v) in labels.iter() {
                        if *v == pos {
                            print!("{}:\n", k);
                        }
                    }
                    print!("   {}\n", inst);
                }
                for (k, v) in labels.iter() {
                    if *v == asm_insts.len() {
                        print!("{}:\n", k);
                    }
                }
            }

            let (packets, new_labels) = if self.disable_scheduler {
                schedule_dummy(&asm_insts, &labels, self.verbose)
            } else {
                schedule(&asm_insts, &labels, self.verbose)
            };
            items.push((packets, new_labels));
            debug_info.insert(func_name,
                              (coloring, insts, opinfo, correspondence,
                               asm_insts.clone() // TODO: remove this when we're confident stuff works
                              ));
        }

        items.push(irtoasm.strings_to_asm(&mut session));

        let (mut all_packets, mut all_labels) = link(items);

        let mut list_file = self.list_file.clone().map(|ref name| {
            let path = Path::new(name);
            File::create(&path).unwrap_or_else(|e| panic!("{}", e))
        });

        let mut debug_file = self.debug_file.clone().map(|ref name| {
            let path = Path::new(name);
            File::create(&path).unwrap_or_else(|e| panic!("{}", e))
        });

        match debug_file {
            Some(ref mut f) => {
                write_debug_file(f,
                                 &all_labels,
                                 &session.parser.spanmap,
                                 &session.parser.filemap,
                                 &sourcemap,
                                 &debug_info);
            },
            None => {}
        }

        match list_file {
            Some(ref mut f) => {
                for (pos, packet) in all_packets.iter().enumerate() {
                    for (k, v) in all_labels.iter() {
                        if *v == LabelInfo::InstLabel(pos) {
                            write!(f, "    {}:\n", k);
                        }
                    }

                    write!(f, "{:04x}        {{ {}; {}; {}; {} }}\n",
                           pos * 16 + self.code_start as usize,
                           packet[0],
                           packet[1],
                           packet[2],
                           packet[3]);
                }
                for (k, v) in all_labels.iter() {
                    if *v == LabelInfo::InstLabel(all_packets.len()) {
                        write!(f, "{}:\n", k);
                    }
                }
            },
            None => {}
        }

        // Determine size of globals.
        let global_size = 1 + global_map.iter()
            .map(|(_, x)| x.offset.unwrap_or(0) + x.size).max().unwrap_or(0) as u32;

        // Determine position of globals. By default, put them at the end of the code.
        let global_start = self.global_start.unwrap_or((all_packets.len() * 0x10) as u32);

        // Determine position of the stack. By default, put it right after the globals.
        let stack_start = self.stack_start.unwrap_or(align(global_start + global_size, 4));

        // Add a special end label.
        all_labels.insert("__END__".to_string(), LabelInfo::InstLabel(all_packets.len()));

        // Add a label for the start of the stack.
        all_labels.insert("__STACK_START__".to_string(), LabelInfo::ByteLabel(stack_start as usize));

        // TODO: once Rust has lexical scoping, use into_iter() here.
        for global_info in global_map.values() {
            if !global_info.is_extern {
                all_labels.insert(
                    format!("{}", global_info.label.expect("Global has no label.")),
                    LabelInfo::ByteLabel(global_info.offset.expect("Global has no offset.")
                                         + global_start as usize)
                );
            }
        }

        if self.verbose {
            for (ref label, _) in all_labels.iter() {
                print!("Label: {}\n", label);
            }
        }
        resolve_labels(&mut all_packets, &all_labels, self.code_start as usize);

        if self.verbose {
            for packet in all_packets.iter() {
                print!("0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x},\n",
                       encode(&packet[0]),
                       encode(&packet[1]),
                       encode(&packet[2]),
                       encode(&packet[3]))
            }
        }

        if self.format == BinaryFormat::BSLDFormat {
            // Print the bs-ld header, if necessary.
            write!(f, "MROE");
            // Binary size
            print_bin(all_packets.len() as u32 * 0x10, f);
            // Image size
            print_bin(global_start - self.code_start + global_size, f);
            // Binary start
            print_bin(self.code_start, f);
            // First writable
            print_bin(global_start, f);
            // Entry
            print_bin(self.code_start, f);
        }

        for packet in all_packets.iter() {
            print_bin(encode(&packet[0]), f);
            print_bin(encode(&packet[1]), f);
            print_bin(encode(&packet[2]), f);
            print_bin(encode(&packet[3]), f);
        }
    }

}
