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
use codegen::NUM_USABLE_VARS;
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
use std::collections::{BTreeSet, BTreeMap};

pub struct AsmTarget {
    verbose: bool,
    list_file: Option<String>,
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
        let mut list_file = None;

        for arg in args.iter() {
            if arg.0 == "verbose".to_string() {
                print!("Enabling verbose mode.\n");
                verbose = true;
            } else if arg.0 == "list" {
                list_file = arg.1.clone();
            }
        }
        Box::new(AsmTarget { verbose: verbose, list_file: list_file })
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

        let fname = "src/mc/prelude.ma";
        let path = Path::new(fname);
        let file = File::open(&path).unwrap_or_else(|e| panic!("{}", e));

        let reader = BufReader::new(file);
        let asm_lexer = new_asm_lexer(fname, reader);
        let asm_peekable = asm_lexer.peekable();
        let mut asm_parser = AsmParser::new(asm_peekable);
        let (insts, labels) = asm_parser.parse_toplevel();

        let mut items = vec!((insts, labels));

        let mut strings: BTreeSet<Name> = BTreeSet::new();

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
            let (asm_insts, labels) = IrToAsm::ir_to_asm(insts,
                                                         &global_map,
                                                         &mut session,
                                                         &mut strings);

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

            let (packets, new_labels) = schedule_dummy(&asm_insts,
                                                 &labels,
                                                 self.verbose);

            items.push((packets, new_labels));
        }

        items.push(IrToAsm::strings_to_asm(&session, &strings));

        let (mut all_packets, mut all_labels) = link(items);

        let mut list_file = self.list_file.clone().map(|ref name| {
            let path = Path::new(name);
            File::create(&path).unwrap_or_else(|e| panic!("{}", e))
        });

        match list_file {
            Some(ref mut f) => {
                for (pos, packet) in all_packets.iter().enumerate() {
                    for (k, v) in all_labels.iter() {
                        if *v == pos {
                            write!(f, "    {}:\n", k);
                        }
                    }

                    write!(f, "{:04x}        {{ {}; {}; {}; {} }}\n",
                           pos * 16,
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

        labels::resolve_labels(&mut all_packets, &all_labels);
        if self.verbose {
            for packet in all_packets.iter() {
                print!("0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x},\n",
                       encode(&packet[0]),
                       encode(&packet[1]),
                       encode(&packet[2]),
                       encode(&packet[3]))
            }
        }

        for packet in all_packets.iter() {
            print_bin(encode(&packet[0]), f);
            print_bin(encode(&packet[1]), f);
            print_bin(encode(&packet[2]), f);
            print_bin(encode(&packet[3]), f);
        }
    }

}
