use codegen::num_usable_vars;
use codegen::register_color::*;
use ir::*;
use ir::conflicts::ConflictAnalyzer;
use mas::ast::*;
use mas::util::pack_int;
use mc::ast::*;
use std::mem::swap;
use std::collections::TreeMap;

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

/// Convert a binoprvalue into the internal asm representation.
fn convert_binoprvalue<'a>(
    regmap: &TreeMap<Var, RegisterColor>,
    dest: Reg,
    op: &BinOpNode,
    mut op_l: &'a RValueElem,
    mut op_r: &'a RValueElem) -> Vec<InstNode> {

    let mut result = vec!();
    let mut swapped = false;

    if !op_l.is_variable() {
        swap(&mut op_l, &mut op_r);
        swapped = true;
    }

    let var_l = match *op_l {
        Variable(var) => var,
        _ => fail!("Trying to apply a binary operation to two constants. Did you remember to do the constant folding pass?"),
    };

    let reg_l = match *regmap.find(&var_l).unwrap() {
        RegColor(reg) => reg,
        // TODO: implement spilling.
        _ => unimplemented!(),
    };

    match *op_r {
        Variable(ref var) => {
            let reg_r = match *regmap.find(var).unwrap() {
                RegColor(reg) => reg,
                _ => unimplemented!(),
            };

            result.push(
                ALU2RegInst(
                    Pred { inverted: false,
                           reg: 3 },
                    binop_to_aluop(op, swapped),
                    dest,
                    reg_l,
                    reg_r,
                    SllShift,
                    0));
        },
        Constant(ref val) => {
            let num = lit_to_u32(val);
            let packed = pack_int(num,10);

            match packed {
                Some((val, rot)) =>
                    result.push(
                        ALU2ShortInst(
                            Pred {
                                inverted: false,
                                reg: 3 },
                            binop_to_aluop(op, swapped),
                            dest,
                            reg_l,
                            val,
                            rot)),
                None => {
                    result.push(
                        ALU2LongInst(
                            Pred {
                                inverted: false,
                                reg: 3 },
                            binop_to_aluop(op, swapped),
                            dest,
                            reg_l)
                            );
                    result.push(
                        LongInst(num));
                }
            }
        }
    }

    result
}

fn convert_directrvalue(
    regmap: &TreeMap<Var, RegisterColor>,
    dest: Reg,
    src: &RValueElem) -> Vec<InstNode> {

    let mut result = vec!();

    match *src {
        Variable(ref var) => {
            let rhs_reg =
                match *regmap.find(var).unwrap() {
                    RegColor(reg) => reg,
                    _ => unimplemented!(),
                };

            // Don't output redundant moves.
            if dest != rhs_reg {
                result.push(
                    ALU1RegInst(
                        Pred { inverted: false,
                               reg: 3 },
                        MovAluOp,
                        dest,
                        rhs_reg,
                        SllShift,
                        0
                            )
                        );
            }
        },
        Constant(ref val) => {
            let num = lit_to_u32(val);
            let packed = pack_int(num, 15);
            match packed {
                Some((val, rot)) =>
                    result.push(
                        ALU1ShortInst(
                            Pred { inverted: false,
                                   reg: 3 },
                            MovAluOp,
                            dest,
                            val,
                            rot)),
                None => {
                    result.push(
                        ALU1LongInst(
                            Pred { inverted: false,
                                   reg: 3 },
                            MovAluOp,
                            dest));
                    result.push(
                        LongInst(num));
                }
            }
        }
    }

    result
}


impl IrToAsm {
    pub fn ir_to_asm(ops: &Vec<Op>) -> Vec<InstNode> {
        let (conflicts, counts) = ConflictAnalyzer::conflicts(ops);
        let regmap = RegisterColorer::color(conflicts, counts, num_usable_vars);

        let mut result = vec!();
        for op in ops.iter() {
            match *op {
                Assign(ref lv, ref rv) => {
                    let var = match *lv {
                        VarLValue(var) => var,
                        _ => unimplemented!(),
                    };

                    let lhs_reg = match *regmap.find(&var).unwrap() {
                        RegColor(reg) => reg,
                        // TODO: implement spilling.
                        _ => unimplemented!(),
                    };

                    let insts = match *rv {
                        DirectRValue(ref rve) =>
                            convert_directrvalue(&regmap, lhs_reg, rve),
                        BinOpRValue(ref op, ref rve1, ref rve2) =>
                            convert_binoprvalue(&regmap, lhs_reg,
                                                op, rve1, rve2),
                        _ => vec!(),
                    };
                    result.push_all_move(insts);
                },
                _ => {},
            }
        }

        result
    }
}