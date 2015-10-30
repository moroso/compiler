use package::Package;

use mc::lexer::Lexer;
use mc::parser::Parser;
use mc::session::Interner;
use mc::ast::NodeId;

use super::{MkTarget,Target};

use ir::liveness::LivenessAnalyzer;
use ir::ast_to_intermediate::ASTToIntermediate;
use ir::constant_fold::ConstantFolder;
use ir::ssa::ToSSA;
use ir::conflicts::ConflictAnalyzer;
use ir::StaticIRItem;

use target::NameMangler;

use codegen::register_color::RegisterColorer;
use codegen::{NUM_USABLE_VARS, GLOBAL_MEM_START, STACK_START};
use codegen::IrToAsm;
use codegen::combine::link;

use mas::labels;
use mas::encoder::encode;
use mas::ast::NopInst;
use mas::scheduler::{schedule, schedule_dummy};
use mas::lexer::new_asm_lexer;
use mas::parser::AsmParser;

use util::Name;

use std::io::{Write, BufReader};
use std::fs::File;
use std::path::Path;
use std::collections::{BTreeSet, BTreeMap, BinaryHeap};

#[derive(Eq, PartialEq)]
enum BinaryFormat {
    FlatFormat,
    BSLDFormat,
}

pub struct AsmTarget {
    verbose: bool,
    disable_scheduler: bool,
    list_file: Option<String>,
    debug_file: Option<String>,
    format: BinaryFormat,
    code_start: u32,
    global_start: u32,
    stack_start: u32,
}

// TODO: move this somewhere common.
fn print_bin(n: u32, stream: &mut Write) {
    // Write in little-endian format.
    (stream.write(vec!(
        (n >>  0) as u8,
        (n >>  8) as u8,
        (n >> 16) as u8,
        (n >> 24) as u8,
        ).as_ref())).ok();
}

impl MkTarget for AsmTarget {
    fn new(args: &Vec<(String, Option<String>)>) -> Box<AsmTarget> {
        let mut verbose = false;
        let mut disable_scheduler = false;
        let mut list_file = None;
        let mut format = BinaryFormat::FlatFormat;
        let mut code_start = 0;
        let mut stack_start = STACK_START;
        let mut global_start = GLOBAL_MEM_START;
        let mut debug_file = None;

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
                stack_start = u32::from_str_radix(&arg.1.clone().unwrap()[..], 16).unwrap();
            } else if arg.0 == "global_start" {
                global_start = u32::from_str_radix(&arg.1.clone().unwrap()[..], 16).unwrap();
            } else if arg.0 == "disable_scheduler" {
                disable_scheduler = true;
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
            debug_file: debug_file,
        })
    }
}

impl Target for AsmTarget {
    #[allow(unused_must_use)]
    fn compile(&self, p: Package, f: &mut Write) {
        let Package {
            module,
            session,
            mut typemap,
        } = p;

        let mangler = NameMangler::new(session, &module, true, false);
        let mut sourcemap = BTreeMap::<NodeId, NodeId>::new();
        let mut session = mangler.session;

        if self.verbose {
            print!("Mangler: {:?}\n", mangler.names);
        }

        let (mut result, mut staticitems) = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap,
                                                       &mangler.names,
                                                       &mut sourcemap);

            converter.convert_module(&module)
        };

        // TODO: this is a hack. Eventually we should extract names from labels
        // in any included asm files.
        let asm_staticitems: Vec<StaticIRItem> = vec!("MANGLEDprelude_print_uint",
                                                      "MANGLEDprelude_print_int",
                                                      "rt_memcpy")
            .iter()
            .map(|x|
                 StaticIRItem {
                     name: session.interner.intern(
                         x.to_string()),
                     size: 0,
                     offset: None,
                     is_ref: false,
                     is_func: true,
                     is_extern: true,
                     expr: None,
                 }).collect();
        staticitems.extend(asm_staticitems.into_iter());

        let global_map = ASTToIntermediate::allocate_globals(staticitems);
        if self.verbose {
            print!("Global map: {:?}\n", global_map);
        }
        let global_initializer = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap,
                                                       &mangler.names,
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
                                       session,
                                       strings,
                                       self.global_start);

        for insts in result.iter_mut() {
            if self.verbose {
                print!("Start conversion!\n");
                print!("{:?}\n", insts);
            }
            ToSSA::to_ssa(insts, self.verbose);
            ConstantFolder::fold(insts, &global_map, self.verbose);
            let opinfo = LivenessAnalyzer::analyze(insts);
            if self.verbose {
                for a in opinfo.iter() {
                    print!("{:?}\n", a);
                }
                print!("{:?}\n", insts);
                let (conflict_map, counts, must_colors, mem_vars) =
                    ConflictAnalyzer::conflicts(insts, &opinfo);
                print!("conflicts: {:?}\ncounts: {:?}\nmust: {:?}\nin mem: {:?}\n",
                       conflict_map, counts, must_colors, mem_vars);
                print!("{:?}\n",
                       RegisterColorer::color(conflict_map, counts,
                                              must_colors, mem_vars,
                                              &global_map,
                                              NUM_USABLE_VARS as usize));
            }
            let (asm_insts, labels) = irtoasm.ir_to_asm(insts);

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
        }

        items.push(irtoasm.strings_to_asm());

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
                let mut func_labels = BinaryHeap::<(isize, String, usize)>::new();
                for (name, &pos) in all_labels.iter() {
                    // TODO: this is a hacky way of checking for internal labels.
                    if !name.starts_with("LABEL") {
                        func_labels.push((-(pos as isize), name.clone(), pos));
                    }
                }
                // Magic
                write!(f, "MROD");

                // Label section
                write!(f, "LBEL");
                // Number of entries
                print_bin(func_labels.len() as u32, f);
                while !func_labels.is_empty() {
                    let (_, name, pos) = func_labels.pop().unwrap();
                    print_bin(pos as u32, f);
                    print_bin(name.len() as u32 + 1, f);
                    write!(f, "{}", name);
                    f.write(&[0]);
                }
            },
            None => {}
        }

        match list_file {
            Some(ref mut f) => {
                for (pos, packet) in all_packets.iter().enumerate() {
                    for (k, v) in all_labels.iter() {
                        if *v == pos {
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
                    if *v == all_packets.len() {
                        write!(f, "{}:\n", k);
                    }
                }
            },
            None => {}
        }

        // Add a special end label.
        all_labels.insert("__END__".to_string(), all_packets.len());

        // Add a label for the start of the stack.
        all_labels.insert("__STACK_START__".to_string(), (self.stack_start / 0x10) as usize);

        labels::resolve_labels(&mut all_packets, &all_labels, self.code_start as usize);

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
            // Determine size of globals.
            let global_size = 1 + global_map.iter()
                .map(|(_, x)| x.offset.unwrap_or(0) + x.size).max().unwrap_or(0) as u32;

            // Print the bs-ld header, if necessary.
            write!(f, "MROE");
            // Binary size
            print_bin(all_packets.len() as u32 * 0x10, f);
            // Image size
            print_bin(self.global_start - self.code_start + global_size, f);
            // Binary start
            print_bin(self.code_start, f);
            // First writable
            print_bin(self.global_start, f);
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
