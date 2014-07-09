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

use std::io::Writer;

pub struct AsmTarget;

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
            let (conflict_map, counts) = ConflictAnalyzer::conflicts(insts);
            write!(f, "conflicts: {}\ncounts: {}\n", conflict_map, counts);
            write!(f, "{}\n",
                   RegisterColorer::color(conflict_map, counts,
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

            let mut packets = asm_insts.iter().map(|x| [(*x).clone(),
                                                        NopInst,
                                                        NopInst,
                                                        NopInst]).collect();

            labels::resolve_labels(&mut packets, &labels);
            for packet in packets.iter() {
                print!("0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x},\n",
                       encode(&packet[0]),
                       encode(&packet[1]),
                       encode(&packet[2]),
                       encode(&packet[3]))
            }
        }
    }

}
