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

use codegen::register_color::RegisterColorer;
use codegen::num_usable_vars;
use codegen::IrToAsm;

use mas::labels;
use mas::encoder::encode;
use mas::ast::NopInst;
use mas::scheduler::schedule;

use std::io::Writer;
use std::io::stdio;

pub struct AsmTarget;

// TODO: move this somewhere common.
fn print_bin<T: Writer>(n: u32, stream: &mut T) {
    // Write in little-endian format.
    (stream.write(vec!(
        (n >>  0) as u8,
        (n >>  8) as u8,
        (n >> 16) as u8,
        (n >> 24) as u8,
        ).as_slice())).ok();
}

impl Target for AsmTarget {
    fn new(_args: Vec<String>) -> AsmTarget {
        AsmTarget
    }

    #[allow(unused_must_use)]
    fn compile(&self, p: Package, f: &mut Writer) {
        let Package {
            module:  module,
            session: mut session,
            typemap: mut typemap,
        } = p;

        let mut result = {
            let mut converter = ASTToIntermediate::new(&mut session,
                                                       &mut typemap);

            converter.convert_module(&module)
        };

        for insts in result.mut_iter() {
            ToSSA::to_ssa(insts, true);
            ConstantFolder::fold(insts, true);
            for a in LivenessAnalyzer::analyze(insts).iter() {
                write!(f, "{}\n", a);
            }
            write!(f, "{}\n", insts);
            let (conflict_map, counts, must_colors, mem_vars) =
                ConflictAnalyzer::conflicts(insts);
            write!(f, "conflicts: {}\ncounts: {}\nmust: {}\nin mem: {}\n",
                   conflict_map, counts, must_colors, mem_vars);
            write!(f, "{}\n",
                   RegisterColorer::color(conflict_map, counts,
                                          must_colors, mem_vars,
                                          num_usable_vars as uint));

            let (asm_insts, labels) = IrToAsm::ir_to_asm(insts);

            for (pos, inst) in asm_insts.iter().enumerate() {
                for (k, v) in labels.iter() {
                    if *v == pos {
                        print!("{}:\n", k);
                    }
                }
                write!(f, "   {}\n", inst);
            }
            for (k, v) in labels.iter() {
                if *v == asm_insts.len() {
                    print!("{}:\n", k);
                }
            }

            let (mut packets, new_labels) = schedule(&asm_insts,
                                                     &labels,
                                                     true);
            print!("New labels: {}\n", new_labels);
            for (pos, packet) in packets.iter().enumerate() {
                for (k, v) in new_labels.iter() {
                    if *v == pos {
                        print!("{}:\n", k);
                    }
                }

                print!("    {}, {}, {}, {},\n",
                       packet[0],
                       packet[1],
                       packet[2],
                       packet[3])
            }
            for (k, v) in new_labels.iter() {
                if *v == packets.len() {
                    print!("{}:\n", k);
                }
            }

            labels::resolve_labels(&mut packets, &new_labels);
            for packet in packets.iter() {
                print!("0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x},\n",
                       encode(&packet[0]),
                       encode(&packet[1]),
                       encode(&packet[2]),
                       encode(&packet[3]))
            }

            let mut stderr = stdio::stderr();

            for packet in packets.iter() {
                print_bin(encode(&packet[0]), &mut stderr);
                print_bin(encode(&packet[1]), &mut stderr);
                print_bin(encode(&packet[2]), &mut stderr);
                print_bin(encode(&packet[3]), &mut stderr);
            }
        }
    }

}
