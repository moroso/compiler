use codegen::num_usable_vars;
use codegen::register_color::*;
use ir::*;
use ir::conflicts::ConflictAnalyzer;
use mas::ast::*;
use mas::util::pack_int;
use mc::ast::*;

pub struct IrToAsm;

fn lit_to_u32(lit: &LitNode) -> u32 {
    match *lit {
        NumLit(num, _) => num as u32,
        _ => unimplemented!(),
    }
}

fn binop_to_aluop(op: &BinOpNode, swapped: bool) -> AluOp {
    match *op {
        PlusOp => AddAluOp,
        MinusOp => if swapped { RsbAluOp } else { SubAluOp },
        // times, divide, and mod are special, and won't be handled here.
        // comparison ops are also not handled here.
        BitAndOp => AndAluOp,
        BitOrOp => OrAluOp,
        BitXorOp => XorAluOp,
        // Shifts are also special.

        _ => unimplemented!(),
    }
}

impl IrToAsm {
    pub fn ir_to_asm(ops: &Vec<Op>) -> Vec<InstNode> {
        let (conflicts, counts) = ConflictAnalyzer::conflicts(ops);
        let regmap = RegisterColorer::color(conflicts, counts, num_usable_vars);

        print!("regmap: {}\n", regmap);

        let mut result = vec!();
        for op in ops.iter() {
            match *op {
                Assign(ref lv, ref rv) => {
                    let var = match *lv {
                        VarLValue(var) => var,
                        _ => unimplemented!(),
                    };

                    print!("lv={}\n", var);

                    let lhs_reg = match *regmap.find(&var).unwrap() {
                        RegColor(reg) => reg,
                        // TODO: implement spilling.
                        _ => unimplemented!(),
                    };

                    match *rv {
                        DirectRValue(ref rve) => {
                            match *rve {
                                Variable(ref var) => {
                                    print!("rhs var={}\n", var);
                                    let rhs_reg =
                                        match *regmap.find(var).unwrap() {
                                            RegColor(reg) => reg,
                                            _ => unimplemented!(),
                                        };

                                    // Don't output redundant moves.
                                    if lhs_reg != rhs_reg {
                                        result.push(
                                            ALU1RegInst(
                                                Pred { inverted: false,
                                                       reg: 3 },
                                                MovAluOp,
                                                lhs_reg,
                                                rhs_reg,
                                                SllShift,
                                                0
                                                    )
                                                );
                                    }
                                },
                                Constant(ref lit) => {}
                            }
                        },
                        BinOpRValue(ref op, ref rve1, ref rve2) => {
                            match *rve1 {
                                Variable(ref var) => {
                                    let rhs_reg1 =
                                        match *regmap.find(var).unwrap() {
                                            RegColor(reg) => reg,
                                            _ => unimplemented!(),
                                        };

                                    match *rve2 {
                                        Variable(ref var) => {
                                            let rhs_reg2 =
                                                match *regmap.find(var).unwrap() {
                                                    RegColor(reg) => reg,
                                                    _ => unimplemented!(),
                                                };
                                            result.push(
                                                ALU2RegInst(
                                                    Pred { inverted: false,
                                                           reg: 3 },
                                                    binop_to_aluop(op,
                                                                   false),
                                                    lhs_reg,
                                                    rhs_reg1,
                                                    rhs_reg2,
                                                    SllShift,
                                                    0));
                                        },
                                        Constant(ref lit) => {
                                            let num = lit_to_u32(lit);
                                            let packed = pack_int(num,
                                                                  10);
                                            match packed {
                                                Some((val, rot)) =>
                                                    result.push(
                                                        ALU2ShortInst(
                                                            Pred {
                                                                inverted: false,
                                                                reg: 3 },
                                                            binop_to_aluop(
                                                                op,
                                                                false),
                                                            lhs_reg,
                                                            rhs_reg1,
                                                            val,
                                                            rot)),
                                                None => {
                                                    result.push(
                                                        ALU2LongInst(
                                                            Pred {
                                                                inverted: false,
                                                                reg: 3 },
                                                            binop_to_aluop(
                                                                op,
                                                                false),
                                                            lhs_reg,
                                                            rhs_reg1)
                                                        );
                                                    result.push(
                                                        LongInst(num));
                                                }
                                            }
                                        }
                                    }
                                },
                                _ => {},
                            }
                        }
                        _ => {}
                    }
                },
                _ => {},
            }
        }

        result
    }
}