use package::Package;

use mc::lexer::Lexer;
use mc::parser::Parser;
use mc::session::Interner;

use super::Target;

use ir::liveness::LivenessAnalyzer;
use ir::ast_to_intermediate::ASTToIntermediate;
use ir::constant_fold::ConstantFolder;
use ir::ssa::ToSSA;
use ir::conflicts::ConflictAnalyzer;
use ir::{Func, StaticIRItem};

use target::NameMangler;

use codegen::register_color::RegisterColorer;
use codegen::num_usable_vars;
use codegen::IrToAsm;
use codegen::combine::link;

use mas::labels;
use mas::encoder::encode;
use mas::ast::NopInst;
use mas::scheduler::{schedule, schedule_dummy};
use mas::lexer::new_asm_lexer;
use mas::parser::AsmParser;

use util::Name;

use std::io::{Writer, stdio, File, BufferedReader};
use std::collections::TreeSet;

pub struct AsmTarget {
    verbose: bool,
}

// TODO: move this somewhere common.
fn print_bin(n: u32, stream: &mut Writer) {
    // Write in little-endian format.
    (stream.write(vec!(
        (n >>  0) as u8,
        (n >>  8) as u8,
        (n >> 16) as u8,
        (n >> 24) as u8,
        ).as_slice())).ok();
}

impl Target for AsmTarget {
    fn new(args: Vec<String>) -> AsmTarget {
        let mut verbose = false;
        for arg in args.iter() {
            if *arg == String::from_str("verbose") {
                print!("Enabling verbose mode.\n");
                verbose = true;
            }
        }
        AsmTarget { verbose: verbose }
    }

    #[allow(unused_must_use)]
    fn compile(&self, p: Package, f: &mut Writer) {
        let Package {
            module:  module,
            session: session,
            typemap: mut typemap,
        } = p;

        let mangler = NameMangler::new(session, &module, true, true);
        let mut session = mangler.session;

        print!("Mangler: {}\n", mangler.names);

        let (mut result, mut staticitems) = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap,
                                                       &mangler.names);

            converter.convert_module(&module)
        };

        // TODO: this is a hack. Eventually we should extract names from labels
        // in any included asm files.
        let asm_staticitems = vec!("MANGLEDprelude_print_uint",
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
        staticitems.push_all_move(asm_staticitems);


        let global_map = ASTToIntermediate::allocate_globals(staticitems);
        if self.verbose {
            print!("Global map: {}\n", global_map);
        }
        let global_initializer = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap,
                                                       &mangler.names);

            converter.convert_globals(&global_map)
        };
        result.push(global_initializer);

        let fname = "src/mc/prelude.ma";
        let path = Path::new(fname);
        let file = File::open(&path).unwrap_or_else(|e| fail!("{}", e));

        let reader = BufferedReader::new(file);
        let asm_lexer = new_asm_lexer(fname, reader);
        let asm_peekable = asm_lexer.peekable();
        let mut asm_parser = AsmParser::new(asm_peekable);
        let (insts, labels) = asm_parser.parse_toplevel();

        let mut items = vec!((insts, labels));

        let mut strings: TreeSet<Name> = TreeSet::new();

        for insts in result.mut_iter() {
            match (*insts)[0] {
                Func(ref n, _, _) => {
                    // We override certain functions with asm versions in
                    // prelude.ma. This is a temporary hack.
                    match format!("{}", n).as_slice() {
                        "MANGLEDprelude_print_uint" |
                        "MANGLEDprelude_print_int" |
                        "MANGLEDprelude_print_newline" => continue,
                        _ => {},
                    }
                },
                _ => fail!()
            }
            ToSSA::to_ssa(insts, self.verbose);
            ConstantFolder::fold(insts, &global_map, self.verbose);
            let opinfo = LivenessAnalyzer::analyze(insts);
            if self.verbose {
                for a in opinfo.iter() {
                    print!("{}\n", a);
                }
                print!("{}\n", insts);
                let (conflict_map, counts, must_colors, mem_vars) =
                    ConflictAnalyzer::conflicts(insts, &opinfo);
                print!("conflicts: {}\ncounts: {}\nmust: {}\nin mem: {}\n",
                       conflict_map, counts, must_colors, mem_vars);
                print!("{}\n",
                       RegisterColorer::color(conflict_map, counts,
                                              must_colors, mem_vars,
                                              &global_map,
                                              num_usable_vars as uint));
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

            let (packets, new_labels) = schedule(&asm_insts,
                                                 &labels,
                                                 self.verbose);

            items.push((packets, new_labels));
        }

        items.push(IrToAsm::strings_to_asm(&session, &strings));

        let (mut all_packets, all_labels) = link(items);

        if self.verbose {
            for (pos, packet) in all_packets.iter().enumerate() {
                for (k, v) in all_labels.iter() {
                    if *v == pos {
                        print!("    {}:\n", k);
                    }
                }

                print!("{:04x}        {}, {}, {}, {},\n",
                       pos * 16,
                       packet[0],
                       packet[1],
                       packet[2],
                       packet[3])
            }
            for (k, v) in all_labels.iter() {
                if *v == all_packets.len() {
                    print!("{}:\n", k);
                }
            }
        }

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
