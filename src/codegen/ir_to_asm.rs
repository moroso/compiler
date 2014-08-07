use codegen::*;
use codegen::register_color::*;
use ir::*;
use ir::conflicts::ConflictAnalyzer;
use ir::liveness::LivenessAnalyzer;
use mas::ast::*;
use mas::util::pack_int;
use mc::ast::*;
use mc::session::Session;
use util::{Width, Width32, Width16, Width8, Name};
use std::mem::swap;
use std::collections::{TreeMap, TreeSet, SmallIntMap};
use std::iter::range_inclusive;
use std::cmp::max;

pub struct IrToAsm;

fn lit_to_longvalue(lit: &LitNode,
                    session: &mut Session,
                    strings: &mut TreeSet<Name>) -> LongValue {
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
        OrElseOp => fail!("AndAlso and OrElse should not appear in IR."),
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
        _ => fail!("Unimplemented op: {}", op), // TODO: this should eventually
                                                // return None.
    }
}

/// Convert a binop into the internal asm representation.
fn convert_binop<'a>(
    regmap: &TreeMap<Var, RegisterColor>,
    global_map: &TreeMap<Name, StaticIRItem>,
    dest: Reg,
    op: &BinOpNode,
    signed: bool,
    mut op_l: &'a RValueElem,
    mut op_r: &'a RValueElem,
    offs: u32,
    session: &mut Session,
    strings: &mut TreeSet<Name>) -> Vec<InstNode> {

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

    let (reg_l, before_l, _) = var_to_reg(regmap, global_map, &var_l, 1, offs);
    result.push_all_move(before_l);

    // TODO: handle shifts, multiplication, division.

    match *op_r {
        Variable(ref var) => {
            let (reg_r, before_r, _) = var_to_reg(regmap, global_map, var, 2,
                                                  offs);
            result.push_all_move(before_r);

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
            let longval = lit_to_longvalue(val, session, strings);
            let packed = match longval {
                Immediate(num) => pack_int(num,10),
                _ => None,
            };

            // TODO: signedness needs to be part of the IR.
            match binop_to_cmpop(op, false, swapped) {
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
                            result.push_all_move(vec!(
                                // TODO: don't always use longs here, and
                                // refactor the code around this to make
                                // the different cases easier to handle.
                                InstNode::alu1long(
                                    Pred { inverted: false,
                                           reg: 3},
                                    MovAluOp,
                                    global_reg),
                                InstNode::anylong(longval),
                                InstNode::mult(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    false, // TODO: support signed.
                                    dest,
                                    reg_l,
                                    global_reg))),
                        DivideOp =>
                            result.push_all_move(vec!(
                                // TODO: don't always use longs here, and
                                // refactor the code around this to make
                                // the different cases easier to handle.
                                InstNode::alu1long(
                                    Pred { inverted: false,
                                           reg: 3},
                                    MovAluOp,
                                    global_reg),
                                InstNode::anylong(longval),
                                InstNode::div(
                                    Pred { inverted: false,
                                           reg: 3 },
                                    false, // TODO: support signed.
                                    dest,
                                    reg_l,
                                    global_reg))),
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
    regmap: &TreeMap<Var, RegisterColor>,
    global_map: &TreeMap<Name, StaticIRItem>,
    dest: Reg,
    op: &UnOpNode,
    rhs: &'a RValueElem,
    offs: u32,
    session: &mut Session,
    strings: &mut TreeSet<Name>) -> Vec<InstNode> {

    let pred = Pred {
        inverted: false,
        reg: 3 };

    // This needs to be special cased.
    if *op == AddrOf {
        match *rhs {
            Variable(ref v) => {
                match *regmap.find(v).unwrap() {
                    StackColor(n) => {
                        return vec!(InstNode::alu2short(pred,
                                                        AddAluOp,
                                                        dest,
                                                        stack_pointer,
                                                        offs + (n * 4) as u32,
                                                        0));
                    },
                    GlobalColor => unimplemented!(),
                    GlobalReferenceColor => unimplemented!(),
                    RegColor(..) => fail!("Cannot take the address of a reg."),
                }
            },
            _ => fail!("Cannot take the address of a constant."),
        }
    }

    let reg_op = match *op {
        Deref |
        AddrOf => fail!("Should not have & or * in IR."),
        Negate => |x| vec!(InstNode::alu2short(pred, RsbAluOp, dest, x, 0, 0)),
        LogNot => |x| vec!(InstNode::alu2short(pred, XorAluOp, dest, x, 1, 0)),
        BitNot => |x| vec!(InstNode::alu1reg(pred, MvnAluOp, dest, x,
                                             SllShift, 0)),
        Identity => |x| if dest == x { vec!() } else {
            vec!(InstNode::alu1reg(pred, MovAluOp, dest, x, SllShift, 0)) },
        SxbOp => |x| vec!(InstNode::alu1reg(pred, SxbAluOp, dest, x,
                                            SllShift, 0)),
        SxhOp => |x| vec!(InstNode::alu1reg(pred, SxhAluOp, dest, x,
                                            SllShift, 0)),
    };

    match *rhs {
        Variable(ref var) => {
            let (reg_r, mut before_r, _) = var_to_reg(regmap, global_map, var,
                                                      2, offs);
            before_r.push_all_move(reg_op(reg_r));
            before_r
        },
        Constant(ref val) => {
            if *op != Identity { fail!("Should have been constant folded. {}",
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
// offs is where variables on the stack start (so, after all structs
// and such that are allocated on the stack).
fn var_to_reg(regmap: &TreeMap<Var, RegisterColor>,
              global_map: &TreeMap<Name, StaticIRItem>,
              var: &Var,
              spill_pos: u8,
              offs: u32) -> (Reg, Vec<InstNode>, Vec<InstNode>) {
    let pred = Pred { inverted: false, reg: 3 };
    match *regmap.find(var).unwrap() {
        RegColor(reg) => {
            let global_info = global_map.find(&var.name);
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
            let reg = Reg { index: spill_reg_base + spill_pos };
            (reg,
             vec!(
                 InstNode::load(pred,
                                LsuOp { store: false,
                                        width: LsuWidthL },
                                reg,
                                stack_pointer,
                                (offs as int + pos * 4) as i32)
                     ),
             vec!(
                 InstNode::store(pred,
                                 LsuOp { store: true,
                                         width: LsuWidthL },
                                 stack_pointer,
                                 (offs as int + pos * 4) as i32,
                                 reg)
                     ),
                 )
        },
        GlobalColor => {
            let global_info = global_map.find(&var.name).unwrap();
            let offs = global_info.offset.expect("No offset for global item");
            let reg = Reg { index: spill_reg_base + spill_pos };
            if global_info.is_ref {
                (reg,
                 vec!(InstNode::alu1long(
                     pred,
                     MovAluOp,
                     reg),
                      InstNode::long(global_mem_start + offs as u32)
                      ),
                 vec!())
            } else {
                (reg,
                 vec!(InstNode::alu1long(
                     pred,
                     MovAluOp,
                     global_reg),
                      InstNode::long(global_mem_start + offs as u32),
                      InstNode::load(pred,
                                     LsuOp { store: false,
                                             width: LsuWidthL },
                                     reg,
                                     global_reg,
                                     0)
                      ),
                 vec!(InstNode::alu1long(
                     pred,
                     MovAluOp,
                     global_reg),
                      InstNode::long(global_mem_start + offs as u32),
                      InstNode::store(pred,
                                      LsuOp { store: true,
                                              width: LsuWidthL },
                                      global_reg,
                                      0,
                                      reg)
                      ),
                 )
            }
        }
        _ => unimplemented!(),
    }
}

fn assign_vars(regmap: &TreeMap<Var, RegisterColor>,
               global_map: &TreeMap<Name, StaticIRItem>,
               pred: &Pred,
               gens: &TreeMap<Name, uint>,
               vars: &TreeSet<Var>,
               offs: u32) -> Vec<InstNode> {
    let mut result = vec!();

    for var in vars.iter() {
        let new_var = Var {
            name: var.name.clone(),
            generation: Some(*gens.find(&var.name).unwrap())
        };
        let (src_reg, src_insts, _) = var_to_reg(regmap, global_map, var, 1,
                                                 offs);
        let (dest_reg, _, dest_insts) = var_to_reg(regmap, global_map,
                                                   &new_var, 1, offs);
        result.push_all_move(src_insts);
        if src_reg != dest_reg {
            result.push(
                InstNode::alu1reg(
                    pred.clone(),
                    MovAluOp,
                    dest_reg,
                    src_reg,
                    SllShift,
                    0));
        }
        result.push_all_move(dest_insts);
    }

    result
}

impl IrToAsm {
    fn empty_packet() -> [InstNode, ..4] {
        [InstNode::long(0),
         InstNode::long(0),
         InstNode::long(0),
         InstNode::long(0)]
    }

    pub fn strings_to_asm(session: &Session,
                          strings: &TreeSet<Name>) -> (Vec<[InstNode, ..4]>,
                                                       TreeMap<String, uint>) {
        let mut labels = TreeMap::new();
        let mut insts: Vec<[InstNode, ..4]> = vec!();
        for &Name(s) in strings.iter() {
            let mut packetpos = 0u;
            let mut bytepos = 0u;
            let mut cur = 0u32;
            let mut cur_packet: [InstNode, ..4] = IrToAsm::empty_packet();
            labels.insert(format!("__INTERNED_STRING{}", s), insts.len());
            for b in session.interner.name_to_str(&Name(s)).bytes()
                .chain(vec!(0u8).move_iter()) {
                cur |= b as u32 << (bytepos*8);
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
                     global_map: &TreeMap<Name, StaticIRItem>,
                     session: &mut Session,
                     strings: &mut TreeSet<Name>
                     ) -> (Vec<InstNode>, TreeMap<String, uint>) {
        let opinfo = LivenessAnalyzer::analyze(ops);
        let (conflicts, counts, must_colors, mem_vars) =
            ConflictAnalyzer::conflicts(ops, &opinfo);
        let regmap = RegisterColorer::color(conflicts, counts, must_colors,
                                            mem_vars, global_map,
                                            num_usable_vars);
        // Does this function call any other function? If not, we can
        // avoid saving r31.
        let mut has_call = false;
        // Figure out where objects on the stack will go.
        // stack_item_map is a map from instruction index (for an alloca
        // instruction) to a stack offset.
        let mut stack_item_map: TreeMap<uint, u32> = TreeMap::new();
        // Start at 4, to skip over the saved return value.
        let mut stack_item_offs: u32 = 4;
        for (inst, op) in ops.iter().enumerate() {
            match *op {
                Alloca(ref var, size) => {
                    if !global_map.find(&var.name).is_some() {
                        stack_item_map.insert(inst, stack_item_offs);
                        stack_item_offs += size as u32;
                    }
                },
                Call(..) => { has_call = true; }
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

        let mut targets: TreeMap<String, uint> = TreeMap::new();

        let mut labels: SmallIntMap<TreeMap<Name, uint>> = SmallIntMap::new();
        // Find out which variables are used at each label.
        // TODO: merge this in with the loop above, so we don't iterate over
        // the instruction list as many times?
        for op in ops.iter() {
            match *op {
                Label(ref idx, ref vars) => {
                    let mut varmap: TreeMap<Name, uint> = TreeMap::new();
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
        for (pos, op) in ops.iter().enumerate() {
            match *op {
                Func(ref name, _, is_extern) => {
                    if is_extern { continue; }
                    targets.insert(format!("{}", name), result.len());

                    // Save the return address, offset by one packet size.
                    if has_call {
                        result.push(
                            InstNode::store(true_pred,
                                            store32_op,
                                            stack_pointer,
                                            0,
                                            link_register
                                            )
                                );
                    }

                    // Save all callee-save registers
                    for (x, i) in range_inclusive(first_callee_saved_reg.index,
                                                  max_reg_index).enumerate() {
                        result.push(
                            InstNode::store(true_pred,
                                            store32_op,
                                            stack_pointer,
                                            (stack_item_offs as uint +
                                             x * 4) as i32,
                                            Reg { index: i })
                                );
                    }
                },
                Return(ref rve) => {
                    // Store the result in r0.
                    result.push_all_move(
                        convert_unop(&regmap, global_map, return_reg,
                                     &Identity, rve,
                                     stack_item_offs,
                                     session, strings));

                    // Restore all callee-save registers
                    for (x, i) in range_inclusive(first_callee_saved_reg.index,
                                                  max_reg_index).enumerate() {
                        result.push(
                            InstNode::load(true_pred,
                                           load32_op,
                                           Reg { index: i },
                                           stack_pointer,
                                           (stack_item_offs as uint +
                                            x * 4) as i32
                                           ));
                    }

                    // Restore link register
                    if has_call {
                        result.push(
                            InstNode::load(true_pred,
                                           load32_op,
                                           link_register,
                                           stack_pointer,
                                           0));
                    }
                    // Return
                    result.push(
                        InstNode::branchreg(true_pred,
                                            false,
                                            link_register,
                                            1));
                },
                BinOp(ref var, ref op, ref rve1, ref rve2, signed) => {
                    let (lhs_reg, _, after) = var_to_reg(&regmap, global_map,
                                                         var, 0,
                                                         stack_item_offs);
                    result.push_all_move(
                        convert_binop(&regmap, global_map, lhs_reg, op, signed,
                                      rve1, rve2, stack_item_offs, session,
                                      strings));
                    result.push_all_move(after);
                },
                UnOp(ref var, ref op, ref rve1) => {
                    let (lhs_reg, _, after) = var_to_reg(&regmap, global_map,
                                                         var, 0,
                                                         stack_item_offs);
                    result.push_all_move(
                        convert_unop(&regmap, global_map, lhs_reg, op, rve1,
                                     stack_item_offs, session, strings));
                    result.push_all_move(after);
                }
                Load(ref var1, ref var2, ref width) |
                Store(ref var1, ref var2, ref width) => {
                    let store = match *op {
                        Load(..) => false,
                        _ => true,
                    };
                    let (reg1, before1, _) = var_to_reg(&regmap, global_map,
                                                        var1, 0,
                                                        stack_item_offs);
                    result.push_all_move(before1);
                    let (reg2, before2, _) = var_to_reg(&regmap, global_map,
                                                        var2, 0,
                                                        stack_item_offs);
                    result.push_all_move(before2);

                    let lsuop = LsuOp { store: store,
                                        width: width_to_lsuwidth(width) };
                    result.push(
                        if store {
                            InstNode::store(true_pred,
                                            lsuop,
                                            reg1,
                                            0,
                                            reg2)
                        } else {
                            InstNode::load(true_pred,
                                           lsuop,
                                           reg1,
                                           reg2,
                                           0)
                        });
                },
                CondGoto(ref negated, Variable(ref var), ref label,
                         ref vars) => {
                    let (reg, before, _) = var_to_reg(&regmap, global_map,
                                                      var, 0,
                                                      stack_item_offs);
                    result.push_all_move(before);
                    result.push(
                        InstNode::compareshort(
                            true_pred,
                            Pred { inverted: false,
                                   reg: 0 },
                            reg,
                            CmpBS,
                            1,
                            0));
                    result.push_all_move(assign_vars(&regmap, global_map,
                                                     &true_pred,
                                                     labels.get(label),
                                                     vars, stack_item_offs));
                    result.push(
                        InstNode::branchimm(
                            Pred { inverted: *negated,
                                   reg: 0},
                            false,
                            JumpLabel(format!("LABEL{}", label))));
                },
                Goto(ref label, ref vars) => {
                    result.push_all_move(assign_vars(&regmap, global_map,
                                                     &true_pred,
                                                     labels.get(label),
                                                     vars, stack_item_offs));
                    // Don't emit redundant jumps.
                    let next = &ops[pos+1];
                    match *next {
                        Label(label2, _) if *label == label2 => {},
                        _ =>
                            result.push(
                                InstNode::branchimm(
                                    true_pred,
                                    false,
                                    JumpLabel(format!("LABEL{}", label))))
                    }
                },
                Label(ref label, _) => {
                    targets.insert(format!("LABEL{}", label), result.len());
                },
                Alloca(ref var, _) => {
                    let offs_opt = stack_item_map.find(&pos);
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
                                    true_pred,
                                    AddAluOp,
                                    reg,
                                    stack_pointer,
                                    // TODO: encode as base/shift?
                                    offs,
                                    0)
                                    );
                            result.push_all_move(after);
                        },
                        // This is actually allocated in global storage, and
                        // everything will be taken care of for us.
                        None => {}
                    }
                },
                Call(_, ref f, ref vars) => {
                    // TODO: this is a lot messier than it should be.
                    // Clean it up!

                    // The register allocator will ensure that all variables
                    // that need to be passed on the stack actually are, and
                    // that the return variable is assigned correctly. We need
                    // to worry about setting up the stack frame and putting
                    // any variables that need to be passed on the stack in
                    // the right places.

                    let total_vars = vars.len();

                    let stack_arg_offs = stack_item_offs as int +
                                         (max_stack_index + 1) * 4;

                    // This is where the stack pointer should end up pointing.
                    // We always reserve at least num_param_regs slots: either
                    // we're using a slot for a parameter, or we're saving
                    // the caller-save variable that goes there.

                    // We want to save caller-save registers, but only if we
                    // actually use them.
                    let ref this_opinfo = opinfo[pos];
                    let ref live_vars = this_opinfo.live;
                    let mut reg_set: TreeSet<Reg> = TreeSet::new();
                    // Make a list of the registers we actually need to save.
                    for var in live_vars.iter() {
                        match *regmap.find(var).expect(
                            "Variable does not appear in regmap") {
                            RegColor(ref r) => {
                                if r.index as uint >= total_vars &&
                                    r.index as uint <= num_param_regs &&
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
                        if total_vars >= num_param_regs {
                            // We're using all registers that we can, and
                            // possibly passing some arguments on the stack.
                            stack_arg_offs as i32 +
                                (total_vars as i32 - num_param_regs as i32) * 4
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
                    for i in range(0, reg_list.len()) {
                        result.push(
                            InstNode::store(
                                true_pred,
                                store32_op,
                                stack_pointer,
                                (stack_arg_offs + i as int * 4) as i32,
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
                    for (i, arg_idx) in range(num_param_regs,
                                              total_vars).enumerate() {
                        let (reg, before, _) = var_to_reg(&regmap,
                                                          global_map,
                                                          &vars[arg_idx], 0,
                                                          stack_item_offs);
                        result.push_all_move(before);
                        result.push(
                            InstNode::store(
                                true_pred,
                                store32_op,
                                stack_pointer,
                                (stack_arg_offs + i as int * 4) as i32,
                                reg
                                ));
                    }

                    // At this point, we need to figure out if the thing we're
                    // calling is a global function itself (in which case we'll
                    // just call it), or a variable that's a function pointer.
                    let func_var = match *f {
                        Variable(v) => v,
                        // TODO
                        _ => fail!(),
                    };

                    let reg_opt = match global_map.find(&func_var.name) {
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
                            true_pred,
                            AddAluOp,
                            stack_pointer,
                            stack_pointer,
                            offs_base,
                            offs_shift));
                    match reg_opt {
                        Some((reg, before)) => {
                            result.push_all_move(before);
                            result.push(InstNode::branchreg(
                                true_pred,
                                true,
                                reg, 0));
                        },
                        None => {
                            result.push(InstNode::branchimm(
                                true_pred,
                                true,
                                JumpLabel(format!("{}", func_var.name))));
                        }
                    }
                    result.push(
                        InstNode::alu2short(
                            true_pred,
                            SubAluOp,
                            stack_pointer,
                            stack_pointer,
                            offs_base,
                            offs_shift)
                        );

                    for i in range(0, reg_list.len()) {
                        result.push(
                            InstNode::load(
                                true_pred,
                                load32_op,
                                reg_list[i],
                                stack_pointer,
                                (stack_arg_offs + i as int * 4) as i32
                                ));
                    }

                }
                _ => {},
            }
        }

        (result, targets)
    }
}