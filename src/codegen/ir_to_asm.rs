use codegen::*;
use codegen::register_color::*;
use ir::*;
use ir::conflicts::ConflictAnalyzer;
use ir::liveness::LivenessAnalyzer;
use mas::ast::*;
use mas::util::pack_int;
use mc::ast::*;
use mc::session::Session;
use util::{Width, Name};
use std::mem::swap;
use std::collections::{BTreeMap, BTreeSet, VecMap};
use std::iter::FromIterator;
use std::cmp::max;

pub struct IrToAsm;

fn lit_to_longvalue(lit: &LitNode,
                    session: &mut Session,
                    strings: &mut BTreeSet<Name>) -> LongValue {
    match *lit {
        NumLit(num, _) => Immediate(num as u32),
        StringLit(ref s) => {
            let Name(name) = session.interner.intern(s.clone());
            strings.insert(Name(name));
            LabelOffs(format!("__INTERNED_STRING{}", name))
        },
        BoolLit(b) => Immediate(if b { 1u32 } else { 0u32 }),
        _ => unimplemented!(),
    }
}

// Convert an AST BinOpNode to a CompareType for the generated ASM.
// The parameters are the op itself, whether the operands are swapped,
// and whether this comparison is signed or not.
// We return the new compare type, and a boolean which tells us if we should
// treat the result as negated.
fn binop_to_cmpop(op: &BinOpNode,
                  signed: bool,
                  swapped: bool) -> Option<(CompareType, bool)> {
    match *op {
        EqualsOp => Some((CmpEQ, false)),
        NotEqualsOp => Some((CmpEQ, true)),
        GreaterEqOp |
        LessOp =>
            if swapped {
                Some((if signed { CmpLES } else { CmpLEU }, *op == LessOp ))
            } else {
                Some((if signed { CmpLTS } else { CmpLTU }, *op != LessOp ))
            },
        GreaterOp |
        LessEqOp =>
            if swapped {
                Some((if signed { CmpLTS } else { CmpLTU }, *op == LessEqOp ))
            } else {
                Some((if signed { CmpLES } else { CmpLEU }, *op != LessEqOp ))
            },
        AndAlsoOp |
        OrElseOp => panic!("AndAlso and OrElse should not appear in IR."),
        _ => None,
    }
}

// Convert an AST BinOpNode to an AluOp for the generated ASM.
fn binop_to_aluop(op: &BinOpNode, swapped: bool) -> Option<AluOp> {
    match *op {
        PlusOp => Some(AddAluOp),
        MinusOp => Some(if swapped { RsbAluOp } else { SubAluOp }),
        BitAndOp => Some(AndAluOp),
        BitOrOp => Some(OrAluOp),
        BitXorOp => Some(XorAluOp),
        // times, divide, and mod are special, and won't be handled here.
        // comparison ops are also not handled here.
        // Shifts are also special.
        _ => panic!("Unimplemented op: {}", op), // TODO: this should eventually
                                                // return None.
    }
}

/// Convert a binop into the internal asm representation.
fn convert_binop<'a>(
    regmap: &BTreeMap<Var, RegisterColor>,
    global_map: &BTreeMap<Name, StaticIRItem>,
    dest: Reg,
    op: &BinOpNode,
    signed: bool,
    mut op_l: &'a RValueElem,
    mut op_r: &'a RValueElem,
    offs: u32,
    session: &mut Session,
    strings: &mut BTreeSet<Name>) -> Vec<InstNode> {

    let mut result = vec!();
    let mut swapped = false;

    if !op_l.is_variable() {
        swap(&mut op_l, &mut op_r);
        swapped = true;
    }

    let var_l = match *op_l {
        Variable(var) => var,
        _ => panic!("Trying to apply a binary operation to two constants. Did you remember to do the constant folding pass?"),
    };

    let (reg_l, before_l, _) = var_to_reg(regmap, global_map, &var_l, 1, offs);
    result.extend(before_l.into_iter());

    // TODO: handle shifts, multiplication, division.

    match *op_r {
        Variable(ref var) => {
            assert!(!swapped);
            let (reg_r, before_r, _) = var_to_reg(regmap, global_map, var, 2,
                                                  offs);
            result.extend(before_r.into_iter());

            // TODO: signedness needs to be part of the IR.
            match binop_to_cmpop(op, signed, swapped) {
                Some((cmptype, negated)) => {
                    result.push(
                        InstNode::comparereg(
                            Pred { inverted: false,
                                   reg: 3 },
                            Pred { inverted: false,
                                   reg: 0 },
                            reg_l,
                            cmptype,
                            reg_r,
                            SllShift,
                            0
                        ));
                    result.push(
                        InstNode::alu1short(
                            Pred { inverted: negated,
                                   reg: 0 },
                            MovAluOp,
                            dest,
                            1,
                            0
                        ));
                    result.push(
                        InstNode::alu1short(
                            Pred { inverted: !negated,
                                   reg: 0 },
                            MovAluOp,
                            dest,
                            0,
                            0
                        ));
                },
                None =>
                    match *op {
                        TimesOp =>
                            result.push(
                                InstNode::mult(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    signed,
                                    dest,
                                    reg_l,
                                    reg_r)),
                        DivideOp =>
                            result.push(
                                InstNode::div(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    signed,
                                    dest,
                                    reg_l,
                                    reg_r)),
                        ModOp => {
                            result.push(
                                InstNode::div(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    signed,
                                    dest,
                                    reg_l,
                                    reg_r));
                            result.push(
                                InstNode::mfhi(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    dest
                                ));
                        },
                        LeftShiftOp |
                        RightShiftOp =>
                            result.push(
                                InstNode::alu1regsh(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    dest,
                                    MovAluOp,
                                    reg_l,
                                    if *op == LeftShiftOp {
                                        SllShift
                                    } else if signed {
                                        SraShift
                                    } else {
                                        SrlShift
                                    },
                                    reg_r
                                    )),
                        _ =>
                            result.push(
                                InstNode::alu2reg(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    binop_to_aluop(op, swapped).unwrap(),
                                    dest,
                                    reg_l,
                                    reg_r,
                                    SllShift,
                                    0))
                    }
            }
        },
        Constant(ref val) => {
            // In this case, we may have swapped the order of the operands.

            let longval = lit_to_longvalue(val, session, strings);
            let packed = match longval {
                Immediate(num) => pack_int(num,10),
                _ => None,
            };

            match binop_to_cmpop(op, signed, swapped) {
                Some((cmptype, negated)) => {
                    match packed {
                        Some((val, rot)) =>
                            result.push(
                                InstNode::compareshort(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    Pred { inverted: false,
                                           reg: 0 },
                                    reg_l,
                                    cmptype,
                                    val,
                                    rot)),
                        None => {
                            result.push(
                                InstNode::comparelong(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    Pred { inverted: false,
                                           reg: 0 },
                                    reg_l,
                                    cmptype));
                            result.push(
                                InstNode::anylong(longval));
                        }
                    }
                    result.push(
                        InstNode::alu1short(
                            Pred { inverted: negated,
                                   reg: 0 },
                            MovAluOp,
                            dest,
                            1,
                            0
                                ));
                    result.push(
                        InstNode::alu1short(
                            Pred { inverted: !negated,
                                   reg: 0 },
                            MovAluOp,
                            dest,
                            0,
                            0
                        ));
                },
                None =>
                    match *op {
                        TimesOp =>
                            // We don't need to worry about swappedness here,
                            // because multiplication commutes.
                            result.extend(vec!(
                                // TODO: don't always use longs here, and
                                // refactor the code around this to make
                                // the different cases easier to handle.
                                InstNode::alu1long(
                                    Pred { inverted: false,
                                           reg: 3},
                                    MovAluOp,
                                    GLOBAL_REG),
                                InstNode::anylong(longval),
                                InstNode::mult(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    signed,
                                    dest,
                                    reg_l,
                                    GLOBAL_REG)).into_iter()),
                        DivideOp =>
                            result.extend(vec!(
                                // TODO: don't always use longs here, and
                                // refactor the code around this to make
                                // the different cases easier to handle.
                                InstNode::alu1long(
                                    Pred { inverted: false,
                                           reg: 3},
                                    MovAluOp,
                                    GLOBAL_REG),
                                InstNode::anylong(longval),
                                if swapped {
                                    InstNode::div(
                                        Pred { inverted: false,
                                               reg: 3 },
                                        signed,
                                        dest,
                                        GLOBAL_REG,
                                        reg_l)
                                } else {
                                    InstNode::div(
                                        Pred { inverted: false,
                                               reg: 3 },
                                        signed,
                                        dest,
                                        reg_l,
                                        GLOBAL_REG)
                                }).into_iter()),
                        ModOp =>
                            result.extend(vec!(
                                // TODO: don't always use longs here, and
                                // refactor the code around this to make
                                // the different cases easier to handle.
                                InstNode::alu1long(
                                    Pred { inverted: false,
                                           reg: 3},
                                    MovAluOp,
                                    GLOBAL_REG),
                                InstNode::anylong(longval),
                                if swapped {
                                    InstNode::div(
                                        Pred { inverted: false,
                                               reg: 3 },
                                        signed,
                                        dest,
                                        GLOBAL_REG,
                                        reg_l)
                                } else {
                                    InstNode::div(
                                        Pred { inverted: false,
                                               reg: 3 },
                                        signed,
                                        dest,
                                        reg_l,
                                        GLOBAL_REG)
                                },
                                InstNode::mfhi(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    dest)
                                    ).into_iter()),
                        LeftShiftOp |
                        RightShiftOp =>
                            if swapped {
                                result.extend(vec!(
                                    InstNode::alu1long(
                                        Pred { inverted: false,
                                               reg: 3},
                                        MovAluOp,
                                        GLOBAL_REG),
                                    InstNode::anylong(longval),
                                    InstNode::alu1regsh(
                                        Pred { inverted: false,
                                               reg: 3 },
                                        dest,
                                        MovAluOp,
                                        GLOBAL_REG,
                                        if *op == LeftShiftOp {
                                            SllShift
                                        } else if signed {
                                            SraShift
                                        } else {
                                            SrlShift
                                        },
                                        reg_l
                                            )).into_iter())
                            } else {
                                let val = match longval {
                                    Immediate(v) => v as u8,
                                    _ => panic!("Expected an immediate"),
                                };
                                result.push(
                                    InstNode::alu1reg(
                                        Pred { inverted: false,
                                               reg: 3 },
                                        MovAluOp,
                                        dest,
                                        reg_l,
                                        if *op == LeftShiftOp {
                                            SllShift
                                        } else if signed {
                                            SraShift
                                        } else {
                                            SrlShift
                                        },
                                        val));
                            },
                        _ =>
                            match packed {
                                Some((val, rot)) =>
                                    result.push(
                                        InstNode::alu2short(
                                            Pred {
                                                inverted: false,
                                                reg: 3 },
                                            binop_to_aluop(op, swapped)
                                                .unwrap(),
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
                                            binop_to_aluop(op, swapped)
                                                .unwrap(),
                                            dest,
                                            reg_l)
                                            );
                                    result.push(
                                        InstNode::anylong(longval));
                                }
                            }
                    }
            }
        }
    }

    result
}

fn convert_unop<'a>(
    regmap: &BTreeMap<Var, RegisterColor>,
    global_map: &BTreeMap<Name, StaticIRItem>,
    dest: Reg,
    op: &UnOpNode,
    rhs: &'a RValueElem,
    offs: u32,
    session: &mut Session,
    strings: &mut BTreeSet<Name>) -> Vec<InstNode> {

    let pred = Pred {
        inverted: false,
        reg: 3 };

    // This needs to be special cased.
    if *op == AddrOf {
        match *rhs {
            Variable(ref v) => {
                match *regmap.get(v).unwrap() {
                    StackColor(n) => {
                        return vec!(InstNode::alu2short(pred,
                                                        AddAluOp,
                                                        dest,
                                                        STACK_POINTER,
                                                        offs + (n * 4) as u32,
                                                        0));
                    },
                    GlobalColor => unimplemented!(),
                    GlobalReferenceColor => unimplemented!(),
                    RegColor(ref reg) => {
                        // If we're taking the address of something with a
                        // register color, it had better be a global function.
                        // In that case, the address if the function is stored
                        // in that register, and we can just copy it to the
                        // destination.
                        let global_info = global_map.get(&v.name);
                        match global_info {
                            Some(ref gi) => {
                                assert!(gi.is_func);
                                if dest == *reg {
                                    return vec!();
                                } else {
                                    return vec!(
                                        InstNode::alu1reg(
                                            Pred { inverted: false,
                                                   reg: 3 },
                                            MovAluOp,
                                            dest,
                                            reg.clone(),
                                            SllShift,
                                            0))
                                }
                            },
                            None =>
                                panic!("Cannot take the address of a reg.")
                        }
                    },
                }
            },
            _ => panic!("Cannot take the address of a constant."),
        }
    }

    let reg_op: Box<Fn(Reg) -> Vec<InstNode>> = match *op {
        Deref |
        AddrOf => panic!("Should not have & or * in IR."),
        Negate => Box::new(|x| vec!(InstNode::alu2short(pred, RsbAluOp, dest, x, 0, 0))),
        LogNot => Box::new(|x| vec!(InstNode::alu2short(pred, XorAluOp, dest, x, 1, 0))),
        BitNot => Box::new(|x| vec!(InstNode::alu1reg(pred, MvnAluOp, dest, x,
                                             SllShift, 0))),
        Identity => Box::new(|x| if dest == x { vec!() } else {
            vec!(InstNode::alu1reg(pred, MovAluOp, dest, x, SllShift, 0)) }),
        SxbOp => Box::new(|x| vec!(InstNode::alu1reg(pred, SxbAluOp, dest, x,
                                            SllShift, 0))),
        SxhOp => Box::new(|x| vec!(InstNode::alu1reg(pred, SxhAluOp, dest, x,
                                            SllShift, 0))),
    };

    match *rhs {
        Variable(ref var) => {
            let (reg_r, mut before_r, _) = var_to_reg(regmap, global_map, var,
                                                      2, offs);
            before_r.extend(reg_op(reg_r).into_iter());
            before_r
        },
        Constant(ref val) => {
            if *op != Identity { panic!("Should have been constant folded. {}",
                                       op) };

            let mut result = vec!();
            let longval = lit_to_longvalue(val, session, strings);
            let packed = match longval {
                Immediate(num) => pack_int(num, 15),
                _ => None,
            };
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
                        InstNode::anylong(longval));
                }
            }
            result
        }
    }
}

fn width_to_lsuwidth(width: &Width) -> LsuWidth {
    match *width {
        Width::Width32 => LsuWidthL,
        Width::Width16 => LsuWidthH,
        Width::Width8  => LsuWidthB,
        _ => panic!(),
    }
}

// Given a variable, return the register corresponding to it.  Also
// return instructions that must be run before (for reads) and
// afterwards (for writes), for it to be valid (in the case of
// spilling). spill_pos must be 0, 1, or 2, and should not be re-used
// while another register with the same spill_pos is active.
// offs is where variables on the stack start (so, after all structs
// and such that are allocated on the stack).
fn var_to_reg(regmap: &BTreeMap<Var, RegisterColor>,
              global_map: &BTreeMap<Name, StaticIRItem>,
              var: &Var,
              spill_pos: u8,
              offs: u32) -> (Reg, Vec<InstNode>, Vec<InstNode>) {
    let pred = Pred { inverted: false, reg: 3 };
    match *regmap.get(var).unwrap() {
        RegColor(reg) => {
            let global_info = global_map.get(&var.name);
            match global_info {
                None =>
                    // It's an ordinary variable!
                    (reg,
                     vec!(), vec!()),
                Some(ref info) => {
                    // It's a function address. We need to load it into
                    // the register.
                    assert!(info.is_func);
                    (reg,
                     vec!(InstNode::alu1long(
                         pred,
                         MovAluOp,
                         reg),
                          InstNode::long_label(format!("{}", var.name))
                         ),
                     vec!())
                }
            }
        },
        StackColor(pos) => {
            // TODO: clean up these constants and document this.
            let reg = Reg { index: SPILL_REG_BASE + spill_pos };
            (reg,
             vec!(
                 InstNode::load(pred,
                                LsuOp { store: false,
                                        width: LsuWidthL },
                                reg,
                                STACK_POINTER,
                                (offs as isize + pos * 4) as i32)
                     ),
             vec!(
                 InstNode::store(pred,
                                 LsuOp { store: true,
                                         width: LsuWidthL },
                                 STACK_POINTER,
                                 (offs as isize + pos * 4) as i32,
                                 reg)
                     ),
                 )
        },
        GlobalColor => {
            let global_info = global_map.get(&var.name).unwrap();
            let offs = global_info.offset.expect("No offset for global item");
            let reg = Reg { index: SPILL_REG_BASE + spill_pos };
            if global_info.is_ref {
                (reg,
                 vec!(InstNode::alu1long(
                     pred,
                     MovAluOp,
                     reg),
                      InstNode::long(GLOBAL_MEM_START + offs as u32)
                      ),
                 vec!())
            } else {
                (reg,
                 vec!(InstNode::alu1long(
                     pred,
                     MovAluOp,
                     GLOBAL_REG),
                      InstNode::long(GLOBAL_MEM_START + offs as u32),
                      InstNode::load(pred,
                                     LsuOp { store: false,
                                             width: LsuWidthL },
                                     reg,
                                     GLOBAL_REG,
                                     0)
                      ),
                 vec!(InstNode::alu1long(
                     pred,
                     MovAluOp,
                     GLOBAL_REG),
                      InstNode::long(GLOBAL_MEM_START + offs as u32),
                      InstNode::store(pred,
                                      LsuOp { store: true,
                                              width: LsuWidthL },
                                      GLOBAL_REG,
                                      0,
                                      reg)
                      ),
                 )
            }
        }
        _ => unimplemented!(),
    }
}

fn assign_vars(regmap: &BTreeMap<Var, RegisterColor>,
               global_map: &BTreeMap<Name, StaticIRItem>,
               pred: &Pred,
               gens: &BTreeMap<Name, usize>,
               vars: &BTreeSet<Var>,
               offs: u32) -> Vec<InstNode> {
    let mut result = vec!();

    let mut reg_transformations: BTreeMap<Reg, (Reg,
                                               Vec<InstNode>,
                                               Vec<InstNode>)> = BTreeMap::new();
    let mut rev_map: BTreeMap<Reg, Reg> = BTreeMap::new();

    for var in vars.iter() {
        // Globals don't need assignments.
        if global_map.get(&var.name).is_some() {
            continue;
        }

        let new_var = Var {
            name: var.name.clone(),
            generation: Some(*gens.get(&var.name).unwrap())
        };
        let (src_reg, src_insts, _) = var_to_reg(regmap, global_map, var, 1,
                                                 offs);
        let (dest_reg, _, dest_insts) = var_to_reg(regmap, global_map,
                                                   &new_var, 1, offs);
        if src_reg != dest_reg {
            rev_map.insert(dest_reg.clone(), src_reg.clone());
            reg_transformations.insert(src_reg,
                                       (dest_reg.clone(),
                                        src_insts,
                                        dest_insts));
        }
    }

    // This is a little like a toposort, except that we might have cycles we need
    // to break.
    while !reg_transformations.is_empty() {
        // See if there's any assignment rA->rB where rB is not also the source
        // of an assignment. If so, it's safe to store rA in rB.
        let res = reg_transformations.iter()
            .filter(|&(_, &(dest, _, _))|
                    !reg_transformations.contains_key(&dest))
            .next().map(|(a, b)| (a.clone(), b.clone())) ;
        match res {
            Some((src_reg, (dest_reg, src_insts, dest_insts))) =>
            {
                // There's a leaf; it's safe to just assign it.
                reg_transformations.remove(&src_reg);
                result.extend(src_insts.into_iter());

                result.push(
                    InstNode::alu1reg(
                        pred.clone(),
                        MovAluOp,
                        dest_reg,
                        src_reg,
                        SllShift,
                        0));
                result.extend(dest_insts.into_iter());

            },
            None => {
                // This is more exciting! There's a cycle. We need to
                // use a temp register to break it, and then follow
                // the cycle around.

                // Start with any assignment at all. They're all part
                // of cycles.
                let (src_reg, (dest_reg, src_insts, dest_insts)) =
                    reg_transformations.iter()
                    .map(|(x, y)| (x.clone(), y.clone())).next().unwrap();
                reg_transformations.remove(&src_reg);
                result.extend(src_insts.into_iter());
                result.push(
                    InstNode::alu1reg(
                        pred.clone(),
                        MovAluOp,
                        GLOBAL_REG.clone(),
                        src_reg.clone(),
                        SllShift,
                        0));

                // GLOBAL_REG now contains the value that should
                // ultimately go into dest_reg.

                let mut this_src = &src_reg;
                loop {
                    // Find the register that's stored into our
                    // previous source register. That source register
                    // has been moved to its destination (or the temp
                    // register), so it's safe to move over it now.
                    this_src = rev_map.get(this_src).unwrap();

                    // If the register we're trying to move from is
                    // the original register we were moving from,
                    // we're at the end of the loop. Its *actual*
                    // value is in the temp register.
                    if *this_src == src_reg {
                        break;
                    }

                    let (this_dest, src_insts, dest_insts)
                        = reg_transformations.get(this_src).unwrap().clone();
                    reg_transformations.remove(this_src);
                    result.extend(src_insts.into_iter());
                    result.push(
                        InstNode::alu1reg(
                            pred.clone(),
                            MovAluOp,
                            this_dest,
                            this_src.clone(),
                            SllShift,
                            0));
                    result.extend(dest_insts.into_iter());
                }

                // Now it's just a matter of assigning the temporary
                // variable to the original destination.
                result.push(
                    InstNode::alu1reg(
                        pred.clone(),
                        MovAluOp,
                        dest_reg,
                        GLOBAL_REG.clone(),
                        SllShift,
                        0));
                result.extend(dest_insts.into_iter());
            }
        }
    }
    result
}

impl IrToAsm {
    fn empty_packet() -> [InstNode; 4] {
        [InstNode::long(0),
         InstNode::long(0),
         InstNode::long(0),
         InstNode::long(0)]
    }

    pub fn strings_to_asm(session: &Session,
                          strings: &BTreeSet<Name>) -> (Vec<[InstNode; 4]>,
                                                       BTreeMap<String, usize>) {
        let mut labels = BTreeMap::new();
        let mut insts: Vec<[InstNode; 4]> = vec!();
        for &Name(s) in strings.iter() {
            let mut packetpos = 0;
            let mut bytepos = 0;
            let mut cur = 0u32;
            let mut cur_packet: [InstNode; 4] = IrToAsm::empty_packet();
            labels.insert(format!("__INTERNED_STRING{}", s), insts.len());
            for b in session.interner.name_to_str(&Name(s)).bytes()
                .chain(vec!(0u8).into_iter()) {
                cur |= (b as u32) << (bytepos * 8);
                bytepos += 1;
                if bytepos == 4 {
                    bytepos = 0;
                    cur_packet[packetpos] = InstNode::long(cur);
                    packetpos += 1;
                    cur = 0;
                    if packetpos == 4 {
                        insts.push(cur_packet);
                        cur_packet = IrToAsm::empty_packet();
                        packetpos = 0;
                    }
                }
            }

            if bytepos != 0 {
                cur_packet[packetpos] = InstNode::long(cur);
                packetpos += 1;
            }

            if packetpos != 0 {
                insts.push(cur_packet);
            }
        }

        (insts, labels)
    }

    pub fn ir_to_asm(ops: &Vec<Op>,
                     global_map: &BTreeMap<Name, StaticIRItem>,
                     session: &mut Session,
                     strings: &mut BTreeSet<Name>
                     ) -> (Vec<InstNode>, BTreeMap<String, usize>) {
        let opinfo = LivenessAnalyzer::analyze(ops);
        let (conflicts, counts, must_colors, mem_vars) =
            ConflictAnalyzer::conflicts(ops, &opinfo);
        let regmap = RegisterColorer::color(conflicts, counts, must_colors,
                                            mem_vars, global_map,
                                            NUM_USABLE_VARS);
        // Does this function call any other function? If not, we can
        // avoid saving r31.
        let mut has_call = false;
        // Figure out where objects on the stack will go.
        // stack_item_map is a map from instruction index (for an alloca
        // instruction) to a stack offset.
        let mut stack_item_map: BTreeMap<usize, u32> = BTreeMap::new();
        // Start at 4, to skip over the saved return value.
        let mut stack_item_offs: u32 = 4;
        for (inst, op) in ops.iter().enumerate() {
            match *op {
                Op::Alloca(ref var, size) => {
                    // We must be aligned on 4-byte boundaries.
                    let size_adjust = (4 - (size % 4)) % 4;
                    if !global_map.get(&var.name).is_some() {
                        stack_item_map.insert(inst, stack_item_offs);
                        stack_item_offs += (size + size_adjust) as u32;
                    }
                },
                Op::Call(..) => { has_call = true; }
                _ => {}
            }
        }

        // Find the highest index of any register we use, so we know which
        // ones we need to save.
        let max_reg_index = regmap.iter().map(|(_, c)|
                                              match *c {
                                                  RegColor(ref r) => r.index,
                                                  _ => 0,
                                              }).max().unwrap_or(0);
        // Find the highest place on the stack we use.
        let max_stack_index = regmap.iter().map(|(_, c)|
                                                match *c {
                                                    StackColor(i) => i,
                                                    _ => 0,
                                                }).max().unwrap_or(0);

        let mut targets: BTreeMap<String, usize> = BTreeMap::new();

        let mut labels: VecMap<BTreeMap<Name, usize>> = VecMap::new();
        // Find out which variables are used at each label.
        // TODO: merge this in with the loop above, so we don't iterate over
        // the instruction list as many times?
        for op in ops.iter() {
            match *op {
                Op::Label(ref idx, ref vars) => {
                    let mut varmap: BTreeMap<Name, usize> = BTreeMap::new();
                    for var in vars.iter() {
                        varmap.insert(var.name.clone(),
                                      var.generation.expect(
                                          "Variable has no generation"));
                    }
                    labels.insert(*idx, varmap);
                }
                _ => {}
            }
        }

        // These will be useful below.
        let store32_op = LsuOp { store: true, width: LsuWidthL };
        let load32_op = LsuOp { store: false, width: LsuWidthL };

        let mut result = vec!();
        let mut num_saved = 0;
        for (pos, op) in ops.iter().enumerate() {
            match *op {
                Op::Func(ref name, _, ref abi) => {
                    if let Some(ref abi) = *abi {
                        if abi.to_string() == "C" {
                            // If the ABI is "C", we do nothing at all here!
                            continue;
                        }
                    }
                    targets.insert(format!("{}", name), result.len());
                    if let Some(ref abi) = *abi {
                        if abi.to_string() == "bare" {
                            // For the bare API, we include the label, but we omit all of
                            // the usual function entry stuff.
                            continue;
                        }
                    }

                    // Save the return address, offset by one packet size.
                    if has_call {
                        result.push(
                            InstNode::store(TRUE_PRED,
                                            store32_op,
                                            STACK_POINTER,
                                            0,
                                            LINK_REGISTER
                                            )
                                );
                        num_saved = 1;
                    }

                    // Save all callee-save registers
                    for (x, i) in (FIRST_CALLEE_SAVED_REG.index .. max_reg_index+1).enumerate() {
                        result.push(
                            InstNode::store(TRUE_PRED,
                                            store32_op,
                                            STACK_POINTER,
                                            (stack_item_offs as usize +
                                             x * 4) as i32,
                                            Reg { index: i })
                                );
                        num_saved = x + 1 + (if has_call { 1 } else { 0 });
                    }
                },
                Op::Return(ref rve) => {
                    // Store the result in r0.
                    result.extend(
                        convert_unop(&regmap, global_map, RETURN_REG,
                                     &Identity, rve,
                                     stack_item_offs,
                                     session, strings).into_iter());

                    // Restore all callee-save registers
                    for (x, i) in (FIRST_CALLEE_SAVED_REG.index .. max_reg_index+1).enumerate() {
                        result.push(
                            InstNode::load(TRUE_PRED,
                                           load32_op,
                                           Reg { index: i },
                                           STACK_POINTER,
                                           (stack_item_offs as usize +
                                            x * 4) as i32
                                           ));
                    }

                    // Restore link register
                    if has_call {
                        result.push(
                            InstNode::load(TRUE_PRED,
                                           load32_op,
                                           LINK_REGISTER,
                                           STACK_POINTER,
                                           0));
                    }
                    // Return
                    result.push(
                        InstNode::branchreg(TRUE_PRED,
                                            false,
                                            LINK_REGISTER,
                                            1));
                },
                Op::BinOp(ref var, ref op, ref rve1, ref rve2, signed) => {
                    let (lhs_reg, _, after) = var_to_reg(&regmap, global_map,
                                                         var, 0,
                                                         stack_item_offs);
                    result.extend(
                        convert_binop(&regmap, global_map, lhs_reg, op, signed,
                                      rve1, rve2, stack_item_offs, session,
                                      strings).into_iter());
                    result.extend(after.into_iter());
                },
                Op::UnOp(ref var, ref op, ref rve1) => {
                    let (lhs_reg, _, after) = var_to_reg(&regmap, global_map,
                                                         var, 0,
                                                         stack_item_offs);
                    result.extend(
                        convert_unop(&regmap, global_map, lhs_reg, op, rve1,
                                     stack_item_offs, session, strings).into_iter());
                    result.extend(after.into_iter());
                }
                Op::Load(ref var1, ref var2, ref width) |
                Op::Store(ref var1, ref var2, ref width) => {
                    let store = match *op {
                        Op::Load(..) => false,
                        _ => true,
                    };
                    let (reg1, before1, _) = var_to_reg(&regmap, global_map,
                                                        var1, 0,
                                                        stack_item_offs);
                    result.extend(before1.into_iter());
                    let (reg2, before2, _) = var_to_reg(&regmap, global_map,
                                                        var2, 0,
                                                        stack_item_offs);
                    result.extend(before2.into_iter());

                    let lsuop = LsuOp { store: store,
                                        width: width_to_lsuwidth(width) };
                    result.push(
                        if store {
                            InstNode::store(TRUE_PRED,
                                            lsuop,
                                            reg1,
                                            0,
                                            reg2)
                        } else {
                            InstNode::load(TRUE_PRED,
                                           lsuop,
                                           reg1,
                                           reg2,
                                           0)
                        });
                },
                Op::CondGoto(ref negated, Variable(ref var), ref label,
                         ref vars) => {
                    let (reg, before, _) = var_to_reg(&regmap, global_map,
                                                      var, 0,
                                                      stack_item_offs);
                    result.extend(before.into_iter());
                    result.push(
                        InstNode::compareshort(
                            TRUE_PRED,
                            Pred { inverted: false,
                                   reg: 0 },
                            reg,
                            CmpBS,
                            1,
                            0));
                    result.extend(assign_vars(&regmap, global_map,
                                                     &Pred { inverted: *negated,
                                                             reg: 0 },
                                                     &labels[*label],
                                                     vars, stack_item_offs).into_iter());
                    result.push(
                        InstNode::branchimm(
                            Pred { inverted: *negated,
                                   reg: 0},
                            false,
                            JumpLabel(format!("LABEL{}", label))));
                },
                Op::CondGoto(_, Constant(..), _, _) =>
                    panic!("Conditional Goto is not conditional!"),
                Op::Goto(ref label, ref vars) => {
                    result.extend(assign_vars(&regmap, global_map,
                                                     &TRUE_PRED,
                                                     &labels[*label],
                                                     vars, stack_item_offs).into_iter());
                    // Don't emit redundant jumps.
                    let next = &ops[pos+1];
                    match *next {
                        Op::Label(label2, _) if *label == label2 => {},
                        _ =>
                            result.push(
                                InstNode::branchimm(
                                    TRUE_PRED,
                                    false,
                                    JumpLabel(format!("LABEL{}", label))))
                    }
                },
                Op::Label(ref label, _) => {
                    targets.insert(format!("LABEL{}", label), result.len());
                },
                Op::Alloca(ref var, _) => {
                    let offs_opt = stack_item_map.get(&pos);
                    match offs_opt {
                        // This is a "true" alloca, and we put things on the
                        // stack.
                        Some(&offs) => {
                            let (reg, _, after) = var_to_reg(&regmap,
                                                             global_map,
                                                             var, 0,
                                                             stack_item_offs);
                            result.push(
                                InstNode::alu2short(
                                    TRUE_PRED,
                                    AddAluOp,
                                    reg,
                                    STACK_POINTER,
                                    // TODO: encode as base/shift?
                                    offs,
                                    0)
                                    );
                            result.extend(after.into_iter());
                        },
                        // This is actually allocated in global storage, and
                        // everything will be taken care of for us.
                        None => {}
                    }
                },
                Op::Call(_, ref f, ref vars) => {
                    // TODO: this is a lot messier than it should be.
                    // Clean it up!

                    // The register allocator will ensure that all variables
                    // that need to be passed on the stack actually are, and
                    // that the return variable is assigned correctly. We need
                    // to worry about setting up the stack frame and putting
                    // any variables that need to be passed on the stack in
                    // the right places.

                    let total_vars = vars.len();

                    let stack_arg_offs = stack_item_offs as isize +
                                         (max_stack_index + num_saved as isize) * 4;

                    // This is where the stack pointer should end up pointing.
                    // We always reserve at least NUM_PARAM_REGS slots: either
                    // we're using a slot for a parameter, or we're saving
                    // the caller-save variable that goes there.

                    // We want to save caller-save registers, but only if we
                    // actually use them.
                    let ref this_opinfo = opinfo[pos];
                    let ref live_vars = this_opinfo.live;
                    let mut reg_set: BTreeSet<Reg> = BTreeSet::new();
                    // Make a list of the registers we actually need to save.
                    for var in live_vars.iter() {
                        match *regmap.get(var).expect(
                            "Variable does not appear in regmap") {
                            RegColor(ref r) => {
                                if r.index as usize >= total_vars &&
                                    r.index as usize <= NUM_PARAM_REGS &&
                                    r.index > 0 {
                                        reg_set.insert(r.clone());
                                    }
                            },
                            _ => {}
                        }
                    }
                    let num_regs_to_save = reg_set.len();
                    let reg_list: Vec<Reg> =
                        FromIterator::from_iter(reg_set.iter().map(|&x|x));

                    let offs =
                        if total_vars >= NUM_PARAM_REGS {
                            // We're using all registers that we can, and
                            // possibly passing some arguments on the stack.
                            stack_arg_offs as i32 +
                                (total_vars as i32 - NUM_PARAM_REGS as i32) * 4
                        } else {
                            // We're not using all the registers, for
                            // arguments, so we may have to save some
                            // caller-save registers.
                            stack_arg_offs as i32 +
                                num_regs_to_save as i32 * 4
                        };
                    let (offs_base, offs_shift) =
                        pack_int(offs as u32, 10).expect(
                            "Unable to pack literal.");

                    // Save all caller-save registers that need to be saved.
                    // If we use, say, registers r0 - r3 as arguments, we
                    // must save r4 ... r7, and we put those in the first
                    // available stack slots.
                    // Note that if we fill all register slots, we have no
                    // saving to do.
                    // The "max" here is because we never want to save/restore
                    // r0.
                    for i in 0 .. reg_list.len() {
                        result.push(
                            InstNode::store(
                                TRUE_PRED,
                                store32_op,
                                STACK_POINTER,
                                (stack_arg_offs + i as isize * 4) as i32,
                                reg_list[i],
                                ));
                    }

                    // Put any parameters onto the stack that need to be on
                    // the stack. Note that either this loop or the previous
                    // (or both) will be empty; that's as it should be.
                    // If we've had to save registers, it means that we haven't
                    // used all register passing slots, and so we are not
                    // passing any registers on the stack. If we're passing
                    // registers on the stack, it means that we've used all
                    // register slots, and so we don't have to save any.
                    for (i, arg_idx) in (NUM_PARAM_REGS .. total_vars).enumerate() {
                        let (reg, before, _) = var_to_reg(&regmap,
                                                          global_map,
                                                          &vars[arg_idx], 0,
                                                          stack_item_offs);
                        result.extend(before.into_iter());
                        result.push(
                            InstNode::store(
                                TRUE_PRED,
                                store32_op,
                                STACK_POINTER,
                                (stack_arg_offs + i as isize * 4) as i32,
                                reg
                                ));
                    }

                    // At this point, we need to figure out if the thing we're
                    // calling is a global function itself (in which case we'll
                    // just call it), or a variable that's a function pointer.
                    let func_var = match *f {
                        Variable(v) => v,
                        // TODO
                        _ => panic!(),
                    };

                    let reg_opt = match global_map.get(&func_var.name) {
                        // It's a global. No register to deal with!
                        // TODO: check that it's actually a function, and not
                        // a global function pointer.
                        Some(..) => None,
                        // It's a variable. Figure out which register holds it.
                        None => {
                            let (reg, before, _) = var_to_reg(&regmap,
                                                              global_map,
                                                              &func_var, 0,
                                                              stack_item_offs);
                            Some((reg, before))
                        }
                    };

                    result.push(
                        InstNode::alu2short(
                            TRUE_PRED,
                            AddAluOp,
                            STACK_POINTER,
                            STACK_POINTER,
                            offs_base,
                            offs_shift));
                    match reg_opt {
                        Some((reg, before)) => {
                            result.extend(before.into_iter());
                            result.push(InstNode::branchreg(
                                TRUE_PRED,
                                true,
                                reg, 0));
                        },
                        None => {
                            result.push(InstNode::branchimm(
                                TRUE_PRED,
                                true,
                                JumpLabel(format!("{}", func_var.name))));
                        }
                    }
                    result.push(
                        InstNode::alu2short(
                            TRUE_PRED,
                            SubAluOp,
                            STACK_POINTER,
                            STACK_POINTER,
                            offs_base,
                            offs_shift)
                        );

                    for i in 0 .. reg_list.len() {
                        result.push(
                            InstNode::load(
                                TRUE_PRED,
                                load32_op,
                                reg_list[i],
                                STACK_POINTER,
                                (stack_arg_offs + i as isize * 4) as i32
                                ));
                    }

                },
                Op::AsmOp(ref insts) => {
                    result.push(InstNode::packets(insts.clone()));
                },
                Op::Nop => {},
            }
        }

        (result, targets)
    }
}
