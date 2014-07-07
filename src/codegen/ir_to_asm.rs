use codegen::num_usable_vars;
use codegen::register_color::*;
use ir::*;
use ir::conflicts::ConflictAnalyzer;
use mas::ast::*;
use mas::util::pack_int;
use mc::ast::*;
use util::{Width, Width32, Width16, Width8};
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

        _ => fail!("Unimplemented op: {}", op),
    }
}

/// Convert a binop into the internal asm representation.
fn convert_binop<'a>(
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

    let (reg_l, before_l, _) = var_to_reg(regmap, &var_l, 1);
    result.push_all_move(before_l);

    match *op_r {
        Variable(ref var) => {
            let (reg_r, before_r, _) = var_to_reg(regmap, var, 2);
            result.push_all_move(before_r);

            result.push(
                InstNode::alu2reg(
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
                        InstNode::alu2short(
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
                        InstNode::alu2long(
                            Pred {
                                inverted: false,
                                reg: 3 },
                            binop_to_aluop(op, swapped),
                            dest,
                            reg_l)
                            );
                    result.push(
                        InstNode::long(num));
                }
            }
        }
    }

    result
}

fn convert_unop<'a>(
    regmap: &TreeMap<Var, RegisterColor>,
    dest: Reg,
    op: &UnOpNode,
    rhs: &'a RValueElem) -> Vec<InstNode> {

    let pred = Pred {
        inverted: false,
        reg: 3 };

    let reg_op = match *op {
        Deref |
        AddrOf => fail!("Should not have & or * in IR."),
        Negate => |x| vec!(InstNode::alu2short(pred, RsbAluOp, dest, x, 0, 0)),
        LogNot => |x| vec!(InstNode::alu2short(pred, XorAluOp, dest, x, 1, 0)),
        BitNot => |x| vec!(InstNode::alu1reg(pred, MvnAluOp, dest, x,
                                             SllShift, 0)),
        Identity => |x| if dest == x { vec!() } else {
            vec!(InstNode::alu1reg(pred, MovAluOp, dest, x, SllShift, 0)) },
    };

    match *rhs {
        Variable(ref var) => {
            let (reg_r, mut before_r, _) = var_to_reg(regmap, var, 2);
            before_r.push_all_move(reg_op(reg_r));
            before_r
        },
        Constant(ref val) => {
            if *op != Identity { fail!("Should have been constant folded.") };

            let mut result = vec!();
            let num = lit_to_u32(val);
            let packed = pack_int(num, 15);
            match packed {
                Some((val, rot)) =>
                    result.push(
                        InstNode::alu1short(
                            pred,
                            MovAluOp,
                            dest,
                            val,
                            rot)),
                None => {
                    result.push(
                        InstNode::alu1long(
                            pred,
                            MovAluOp,
                            dest)
                            );
                    result.push(
                        InstNode::long(num));
                }
            }
            result
        }
    }
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
                    InstNode::alu1reg(
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
                        InstNode::alu1short(
                            Pred { inverted: false,
                                   reg: 3 },
                            MovAluOp,
                            dest,
                            val,
                            rot)),
                None => {
                    result.push(
                        InstNode::alu1long(
                            Pred { inverted: false,
                                   reg: 3 },
                            MovAluOp,
                            dest));
                    result.push(
                        InstNode::long(num));
                }
            }
        }
    }

    result
}

fn width_to_lsuwidth(width: &Width) -> LsuWidth {
    match *width {
        Width32 => LsuWidthL,
        Width16 => LsuWidthH,
        Width8  => LsuWidthB,
        _ => fail!(),
    }
}

// Given a variable, return the register corresponding to it.  Also
// return instructions that must be run before (for reads) and
// afterwards (for writes), for it to be valid (in the case of
// spilling). spill_pos must be 0, 1, or 2, and should not be re-used
// while another register with the same spill_pos is active.
fn var_to_reg(regmap: &TreeMap<Var, RegisterColor>,
              var: &Var,
              spill_pos: u8) -> (Reg, Vec<InstNode>, Vec<InstNode>) {
    (match *regmap.find(var).unwrap() {
        RegColor(reg) => reg,
        // TODO: implement spilling.
        _ => unimplemented!(),
    },
     vec!(),
     vec!())
}

impl IrToAsm {
    pub fn ir_to_asm(ops: &Vec<Op>) -> Vec<InstNode> {
        let (conflicts, counts) = ConflictAnalyzer::conflicts(ops);
        let regmap = RegisterColorer::color(conflicts, counts, num_usable_vars);

        let mut result = vec!();
        for op in ops.iter() {
            match *op {
                BinOp(ref var, ref op, ref rve1, ref rve2) => {
                    let (lhs_reg, _, after) = var_to_reg(&regmap, var, 0);
                    result.push_all_move(
                        convert_binop(&regmap, lhs_reg, op, rve1, rve2));
                    result.push_all_move(after);
                },
                UnOp(ref var, ref op, ref rve1) => {
                    let (lhs_reg, _, after) = var_to_reg(&regmap, var, 0);
                    result.push_all_move(
                        convert_unop(&regmap, lhs_reg, op, rve1));
                    result.push_all_move(after);
                }
                Load(ref var1, ref var2, ref width) |
                Store(ref var1, ref var2, ref width) => {
                    let store = match *op {
                        Load(..) => false,
                        _ => true,
                    };
                    let (reg1, before1, _) = var_to_reg(&regmap, var1, 0);
                    result.push_all_move(before1);
                    let (reg2, before2, _) = var_to_reg(&regmap, var2, 0);
                    result.push_all_move(before2);

                    let pred = Pred { inverted: false,
                                      reg: 3 };
                    let lsuop = LsuOp { store: store,
                                        width: width_to_lsuwidth(width) };
                    result.push(
                        if store {
                            StoreInst(pred,
                                      lsuop,
                                      reg1,
                                      0,
                                      reg2)
                        } else {
                            LoadInst(pred,
                                     lsuop,
                                     reg1,
                                     reg2,
                                     0)
                        });
                }
                _ => {},
            }
        }

        result
    }
}