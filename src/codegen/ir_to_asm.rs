use codegen::*;
use codegen::register_color::*;
use ir::*;
use ir::conflicts::ConflictAnalyzer;
use ir::liveness::LivenessAnalyzer;
use mas::ast::*;
use mas::util::pack_int;
use mas::parser::pack_1op_immediate;
use mc::ast::*;
use mc::session::Session;
use util::{Width, Name, align};
use std::mem::swap;
use std::collections::{BTreeMap, BTreeSet};
use std::iter::FromIterator;
use std::cmp::max;

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

fn width_to_lsuwidth(width: &Width) -> LsuWidth {
    match *width {
        Width::Width32 => LsuWidthL,
        Width::Width16 => LsuWidthH,
        Width::Width8  => LsuWidthB,
        _ => panic!(),
    }
}

pub struct IrToAsm<'a> {
    global_map: &'a BTreeMap<VarName, StaticIRItem>,
    pub strings: BTreeSet<Name>,
}

impl<'a> IrToAsm<'a> {
    pub fn new(global_map: &'a BTreeMap<VarName, StaticIRItem>,
               strings: BTreeSet<Name>) -> IrToAsm<'a> {
        IrToAsm::<'a> {
            global_map: global_map,
            strings: strings,
        }
    }

    fn empty_packet() -> [InstNode; 4] {
        [InstNode::long(0),
         InstNode::long(0),
         InstNode::long(0),
         InstNode::long(0)]
    }

    pub fn strings_to_asm(&mut self,
                          session: &mut Session) -> (Vec<[InstNode; 4]>,
                                                     BTreeMap<String, usize>) {
        let mut labels = BTreeMap::new();
        let mut insts: Vec<[InstNode; 4]> = vec!();
        for &Name(s) in self.strings.iter() {
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

    pub fn ir_to_asm(&mut self,
                     ops: &Vec<Op>,
                     session: &mut Session,
    ) -> (Vec<InstNode>, BTreeMap<String, usize>,
          BTreeMap<Var, RegisterColor>, // Register allocator output
          Vec<OpInfo>, // Liveness analyzer output
          Vec<usize> // IR -> ASM map.
    ) {
        /*
        Before this function will make any sense, you'll need to know how the stack is organized.

        When we call a function, going into the function r30 (the stack pointer) will point to
        an available part of the stack. Any memory on the stack at the address r30 points to or
        higher is ours to do whatever we want with. Arguments are passed in registers:
        r0 is the first, r1 is the second, up to NUM_PARAM_REGISTERS. All others are passed on the
        stack, in the memory up to where r30 points.

        Our own stack organization is as follows, in order:
         - the saved return address (only if we call other functions; otherwise it stays in r31);
         - space for structures that get allocated on the stack;
         - callee-save registers;
         - spilled variables.

        When we make a function call, in addition to all the above, we have:
         - caller-save registers;
         - arguments for the function being called, if they don't all fit into registers.

        It's essential that at all times, r30 is ahead of any part of the stack we're using.
        That is, we must assume that anything at higher addresses than r30 might be written
        to seemingly randomly (because when we're in kernel mode and we take an interrupt,
        that's what happens!).

        To make all of this easier to keep track of, we define variables storing the offsets
        of each of these regions, *relative to the start of this function's stack*. So the
        return address (if present) is always at offset 0, and so forth. In addition, we
        keep track of where r30 is relative to the start of the stack. Together, these let
        us find things on the stack!
        */
        let opinfo = LivenessAnalyzer::analyze(ops);
        let (conflicts, counts, must_colors, mem_vars) =
            ConflictAnalyzer::conflicts(ops, &opinfo);
        let regmap = RegisterColorer::color(conflicts, counts, must_colors,
                                            mem_vars, self.global_map,
                                            NUM_USABLE_VARS);
        // Does this function call any other function? If not, we can
        // avoid saving r31.
        let mut has_call = false;

        // Figure out where objects allocated on the stack will go.
        // stack_item_map is a map from instruction index (for an alloca
        // instruction) to a stack offset, relative to the beginning
        // of the region for stack-allocated structures.
        let mut stack_item_map: BTreeMap<usize, u32> = BTreeMap::new();

        let mut stack_items_len: u32 = 0;
        for (inst, op) in ops.iter().enumerate() {
            match op.val {
                OpNode::Alloca { ref var, size } => {
                    // We must be aligned on 4-byte boundaries.
                    let adjusted_size = align(size as usize, 4);
                    if !self.global_map.get(&var.name).is_some() {
                        stack_item_map.insert(inst, stack_items_len);
                        stack_items_len += (adjusted_size) as u32;
                    }
                },
                OpNode::Call { .. } => { has_call = true; }
                _ => {}
            }
        }

        let ret_addr_len = if has_call { 4 } else { 0 };

        // Find the highest index of any register we use, so we know which
        // ones we need to save.
        let max_reg_index = regmap.iter().map(|(_, c)|
                                              match *c {
                                                  RegColor(ref r) => r.index,
                                                  _ => 0,
                                              }).max().unwrap_or(0);

        // Find the length of the stack region for spilled variables.
        let spilled_regs_len = regmap.iter().map(|(_, c)|
                                                 match *c {
                                                     StackColor(i) => i + 1,
                                                     _ => 0,
                                                 }).max().unwrap_or(0) * 4;

        // Size of the storage for callee-save registers.
        // TODO: avoid saving registers we don't need to save.
        let callee_save_len =
            if max_reg_index >= FIRST_CALLEE_SAVED_REG.index {
                1 + max_reg_index - FIRST_CALLEE_SAVED_REG.index
            } else {
                0
            } * 4;


        // Here's where we figure out the offsets of each stack region, now that we know
        // the length of each one!

        // The offset and length of the saved return address (always 0: it's always the first
        // thing on the stack, if it's present).
        let ret_addr_offs = 0i32;

        // The offset of stack-allocated items
        let stack_items_offs = ret_addr_offs + ret_addr_len as i32;

        // The offset of the region of saved callee-saved registers.
        let callee_save_offs = stack_items_offs + stack_items_len as i32;

        // Ofset for spilled registers
        let spilled_regs_offs = callee_save_offs + callee_save_len as i32;

        // Where the stack pointer will end up pointing while we're inside the function,
        // and not in the process of calling another function. This is the first location
        // beyond all of the regions we just defined.
        let stack_ptr_offs = spilled_regs_offs + spilled_regs_len as i32;

        // Done figuring out stack sizes!

        let mut targets: BTreeMap<String, usize> = BTreeMap::new();

        let mut labels: BTreeMap<usize, BTreeMap<VarName, usize>> = BTreeMap::new();
        // Find out which variables are used at each label.
        // TODO: merge this in with the loop above, so we don't iterate over
        // the instruction list as many times?
        for op in ops.iter() {
            match op.val {
                OpNode::Label { label_idx: ref idx, ref vars } => {
                    let mut varmap: BTreeMap<VarName, usize> = BTreeMap::new();
                    for var in vars.iter() {
                        varmap.insert(var.name,
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

        // Map from IR locations to ASM locations.
        let mut correspondence = vec!();

        let mut result = vec!();
        for (pos, op) in ops.iter().enumerate() {
            correspondence.push(result.len());
            match op.val {
                OpNode::Func { ref name, ref abi, .. } => {
                    if let Some(ref abi) = *abi {
                        if abi.to_string() == "C" {
                            // If the ABI is "C", we do nothing at all here!
                            continue;
                        }
                    }
                    // Functions are represented by their base names, for compatibility
                    // with asm.
                    targets.insert(format!("{}", name.base_name()), result.len());
                    if let Some(ref abi) = *abi {
                        if abi.to_string() == "bare" {
                            // For the bare API, we include the label, but we omit all of
                            // the usual function entry stuff.
                            continue;
                        }
                    }

                    let (stack_ptr_offs_base, stack_ptr_offs_shift) =
                        pack_int(stack_ptr_offs as u32, 10).expect(
                            "Unable to pack literal.");

                    // Before we can save anything to the stack, we must advance the
                    // stack pointer.
                    if stack_ptr_offs > 0 {
                        result.push(
                            InstNode::alu2short(
                                TRUE_PRED,
                                AddAluOp,
                                STACK_POINTER,
                                STACK_POINTER,
                                stack_ptr_offs_base,
                                stack_ptr_offs_shift)
                        );
                    }

                    // Save the return address.
                    if has_call {
                        result.push(
                            InstNode::store(TRUE_PRED,
                                            store32_op,
                                            STACK_POINTER,
                                            -stack_ptr_offs + ret_addr_offs,
                                            LINK_REGISTER
                                            )
                                );
                    }

                    // Save all callee-save registers.
                    // TODO: don't save registers we know we can't use.
                    for (x, i) in (FIRST_CALLEE_SAVED_REG.index .. max_reg_index+1).enumerate() {
                        result.push(
                            InstNode::store(TRUE_PRED,
                                            store32_op,
                                            STACK_POINTER,
                                            -stack_ptr_offs + callee_save_offs + x as i32 * 4,
                                            Reg { index: i })
                                );
                    }
                },
                OpNode::Return { retval: ref rve_opt } => {
                    match *rve_opt {
                        Some(ref rve) =>
                            // Store the result in r0.
                            result.extend(
                                self.convert_unop(&regmap, RETURN_REG,
                                                  &Identity, rve,
                                                  -stack_ptr_offs,
                                                  -stack_ptr_offs + spilled_regs_offs,
                                                  session).into_iter()),
                        None => {}
                    }

                    // Restore all callee-save registers
                    for (x, i) in (FIRST_CALLEE_SAVED_REG.index .. max_reg_index+1).enumerate() {
                        result.push(
                            InstNode::load(TRUE_PRED,
                                           load32_op,
                                           Reg { index: i },
                                           STACK_POINTER,
                                           -stack_ptr_offs + callee_save_offs + x as i32* 4
                                           ));
                    }

                    // Restore link register
                    if has_call {
                        result.push(
                            InstNode::load(TRUE_PRED,
                                           load32_op,
                                           LINK_REGISTER,
                                           STACK_POINTER,
                                           -stack_ptr_offs + ret_addr_offs));
                    }

                    let (stack_ptr_offs_base, stack_ptr_offs_shift) =
                        pack_int(stack_ptr_offs as u32, 10).expect(
                            "Unable to pack literal.");

                    // Restore stack pointer
                    if stack_ptr_offs > 0 {
                        result.push(
                            InstNode::alu2short(
                                TRUE_PRED,
                                SubAluOp,
                                STACK_POINTER,
                                STACK_POINTER,
                                stack_ptr_offs_base,
                                stack_ptr_offs_shift)
                        );
                    }

                    // Return
                    result.push(
                        InstNode::branchreg(TRUE_PRED,
                                            false,
                                            LINK_REGISTER,
                                            1));
                },
                OpNode::BinOp {
                    target: ref var, ref op, lhs: ref rve1,
                    rhs: ref rve2, signed } => {
                    let (lhs_reg, _, after) = self.var_to_reg(&regmap,
                                                              var, 0,
                                                              -stack_ptr_offs,
                                                              -stack_ptr_offs
                                                              + spilled_regs_offs);
                    result.extend(
                        self.convert_binop(&regmap, lhs_reg, op, signed,
                                           rve1, rve2,
                                           -stack_ptr_offs,
                                           -stack_ptr_offs + spilled_regs_offs,
                                           session,
                                           ).into_iter());
                    result.extend(after.into_iter());
                },
                OpNode::UnOp { target: ref var, ref op, operand: ref rve1 } => {
                    let (lhs_reg, _, after) = self.var_to_reg(&regmap,
                                                              var, 0,
                                                              -stack_ptr_offs,
                                                              -stack_ptr_offs
                                                              + spilled_regs_offs);
                    result.extend(
                        self.convert_unop(&regmap, lhs_reg, op, rve1,
                                          -stack_ptr_offs,
                                          -stack_ptr_offs + spilled_regs_offs,
                                          session,
                                          ).into_iter());
                    result.extend(after.into_iter());
                }
                OpNode::Load { target: ref var1, addr: ref var2, ref width } |
                OpNode::Store { addr: ref var1, value: ref var2, ref width } =>
                {
                    let store = match op.val {
                        OpNode::Load { .. } => false,
                        _ => true,
                    };
                    let (reg1, before1, _) = self.var_to_reg(&regmap,
                                                             var1, 0,
                                                             -stack_ptr_offs,
                                                             -stack_ptr_offs
                                                             + spilled_regs_offs);
                    result.extend(before1.into_iter());
                    let (reg2, before2, _) = self.var_to_reg(&regmap,
                                                             var2, 0,
                                                             -stack_ptr_offs,
                                                             -stack_ptr_offs
                                                             + spilled_regs_offs);
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
                OpNode::CondGoto { ref negated, cond: Variable(ref var),
                                   label_idx: ref label, ref vars } => {
                    let (reg, before, _) = self.var_to_reg(&regmap,
                                                           var, 0,
                                                           -stack_ptr_offs,
                                                           -stack_ptr_offs + spilled_regs_offs);
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
                    result.extend(self.assign_vars(&regmap,
                                                   &Pred { inverted: *negated,
                                                           reg: 0 },
                                                   &labels[label],
                                                   vars,
                                                   -stack_ptr_offs,
                                                   -stack_ptr_offs + spilled_regs_offs).into_iter());
                    result.push(
                        InstNode::branchimm(
                            Pred { inverted: *negated,
                                   reg: 0},
                            false,
                            JumpLabel(format!("LABEL{}", label))));
                },
                OpNode::CondGoto { cond: Constant(..), .. } =>
                    panic!("Conditional Goto is not conditional!"),
                OpNode::Goto { label_idx: ref label, ref vars } => {
                    result.extend(self.assign_vars(&regmap,
                                                   &TRUE_PRED,
                                                   &labels[label],
                                                   vars,
                                                   -stack_ptr_offs,
                                                   -stack_ptr_offs + spilled_regs_offs).into_iter());
                    // Don't emit redundant jumps.
                    // Find the next non-nop instruction.
                    let mut found = false;
                    'outer: for later_op in ops.iter().skip(pos+1) {
                        match later_op.val {
                            // We've reached a matching label; no need to emit the jump.
                            OpNode::Label { label_idx: label2, .. } if *label == label2 => { found = true; break 'outer; },
                            // Nops, as well as other labels, don't count; skip them.
                            OpNode::Label { .. } |
                            OpNode::Nop {} => { },
                            _ => {
                                // We've hit a non-label, non-nop before hitting the jump target, so we
                                // have to emit the jump.
                                result.push(
                                    InstNode::branchimm(
                                        TRUE_PRED,
                                        false,
                                        JumpLabel(format!("LABEL{}", label))));
                                found = true;
                                break 'outer;
                            }
                        }
                    }
                    if !found {
                        // We hit the end. This can happen in a function with no return
                        // (infinite loop).
                        result.push(
                            InstNode::branchimm(
                                TRUE_PRED,
                                false,
                                JumpLabel(format!("LABEL{}", label))));
                    }
                },
                OpNode::Label { label_idx: ref label, .. } => {
                    targets.insert(format!("LABEL{}", label), result.len());
                },
                OpNode::Alloca { ref var, .. } => {
                    let offs_opt = stack_item_map.get(&pos);
                    match offs_opt {
                        // This is a "true" alloca, and we put things on the
                        // stack.
                        Some(&offs) => {
                            let (reg, _, after) = self.var_to_reg(&regmap,
                                                                  var, 0,
                                                                  -stack_ptr_offs,
                                                                  -stack_ptr_offs
                                                                  + spilled_regs_offs);
                            let (offs_base, offs_shift) =
                                pack_int((stack_ptr_offs - stack_items_offs - offs as i32) as u32,
                                         10).expect("Unable to pack literal.");

                            result.push(
                                InstNode::alu2short(
                                    TRUE_PRED,
                                    SubAluOp,
                                    reg,
                                    STACK_POINTER,
                                    offs_base,
                                    offs_shift)
                                    );
                            result.extend(after.into_iter());
                        },
                        // This is actually allocated in global storage, and
                        // everything will be taken care of for us.
                        None => {}
                    }
                },
                OpNode::Call { func: ref f, args: ref vars, .. } => {
                    // TODO: this is a lot messier than it should be.
                    // Clean it up!

                    /*
                    The register allocator will ensure that all variables
                    that need to be passed on the stack actually are, and
                    that the return variable is assigned correctly. We need
                    to worry about setting up the stack frame and putting
                    any variables that need to be passed on the stack in
                    the right places.

                    This is a good moment to remember all the extra stuff that
                    has to go on the stack for the function call:

                    - caller-save registers;
                    - arguments for the function being called, if they don't all fit into registers.

                    Note that we actually will only ever have one or the other. The only caller-save
                    registers other than registers used for arguments are r8, r9, and r10, but
                    we never need to save those. So if any arguments have to go on the stack,
                    it means that all registers for arguments are used so we don't have to
                    explicitly save any of them. If some argument registers are unused, then
                    no arguments are passed in memory but we may have to save some of the
                    registers.
                     */

                    let total_vars = vars.len();

                    // See above. One region or the other will be emtpy, so we can effectively
                    // consider their starting address to be the same, which is wherever
                    // r30 is currently pointing.
                    let stack_arg_offs = stack_ptr_offs;
                    let caller_save_offs = stack_ptr_offs;

                    // We want to save caller-save registers, but only if we
                    // actually use them.
                    let ref this_opinfo = opinfo[pos];
                    let ref live_vars = this_opinfo.live;

                    // regs_to_save tells us which caller-save registers we may need to save.
                    // In particular, any register not used for any live variable at this point
                    // does *not* need to be saved. Variables used as arguments also don't need
                    // to be saved. Furthermore, r0 doesn't need to be saved: we already know
                    // that it will end up with the return value, so the register allocator will
                    // have avoided assigning any live variable to it other than the variable
                    // that will contain the return value.
                    let mut regs_to_save: BTreeSet<Reg> = BTreeSet::new();
                    // Make a list of the registers we actually need to save.
                    for var in live_vars.iter() {
                        match *regmap.get(var).expect(
                            "Variable does not appear in regmap") {
                            RegColor(ref r) => {
                                if r.index as usize >= total_vars &&
                                    r.index as usize <= NUM_PARAM_REGS &&
                                    r.index > 0 {
                                        regs_to_save.insert(r.clone());
                                    }
                            },
                            _ => {}
                        }
                    }
                    // We now know how many registers we need to save, and we've made a list
                    // that tells us which ones and in what order.
                    let num_regs_to_save = regs_to_save.len();
                    let regs_to_save_list: Vec<Reg> =
                        FromIterator::from_iter(regs_to_save.iter().map(|&x|x));

                    // The total amount we grow the stack for this function call, either
                    // because we're saving caller-save registers or because we're passing
                    // arguments on the stack.
                    let extra_stack_len =
                        if total_vars >= NUM_PARAM_REGS {
                            // We're using all registers that we can, and
                            // possibly passing some arguments on the stack.
                            (total_vars as i32 - NUM_PARAM_REGS as i32) * 4
                        } else {
                            // We're not using all the registers, for
                            // arguments, so we may have to save some
                            // caller-save registers.
                            num_regs_to_save as i32 * 4
                        };

                    // Advance the stack pointer before we do any writes.
                    let new_stack_ptr_offs = (stack_ptr_offs + extra_stack_len) as i32;
                    let (extra_stack_len_base, extra_stack_len_shift) =
                        pack_int(extra_stack_len as u32, 10).expect(
                            "Unable to pack literal.");

                    // Before we can save anything to the stack, we must advance the
                    // stack pointer.
                    if extra_stack_len > 0 {
                        result.push(
                            InstNode::alu2short(
                                TRUE_PRED,
                                AddAluOp,
                                STACK_POINTER,
                                STACK_POINTER,
                                extra_stack_len_base,
                                extra_stack_len_shift)
                        );
                    }

                    // Save all caller-save registers that need to be saved.
                    // If we use, say, registers r0 - r3 as arguments, we
                    // must save r4 ... r7, and we put those in the first
                    // available stack slots.
                    // Note that if we fill all register slots, we have no
                    // saving to do.
                    // The "max" here is because we never want to save/restore
                    // r0.
                    for i in 0 .. regs_to_save_list.len() {
                        result.push(
                            InstNode::store(
                                TRUE_PRED,
                                store32_op,
                                STACK_POINTER,
                                -new_stack_ptr_offs + caller_save_offs + i as i32 * 4,
                                regs_to_save_list[i],
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
                        let (reg, before, _) = self.var_to_reg(&regmap,
                                                               &vars[arg_idx], 0,
                                                               -stack_ptr_offs,
                                                               -new_stack_ptr_offs
                                                               + spilled_regs_offs);
                        result.extend(before.into_iter());
                        result.push(
                            InstNode::store(
                                TRUE_PRED,
                                store32_op,
                                STACK_POINTER,
                                -new_stack_ptr_offs + stack_arg_offs + i as i32 * 4,
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

                    let reg_opt = match self.global_map.get(&func_var.name) {
                        // It's a global. No register to deal with!
                        // TODO: check that it's actually a function, and not
                        // a global function pointer.
                        Some(..) => None,
                        // It's a variable. Figure out which register holds it.
                        None => {
                            let (reg, before, _) = self.var_to_reg(&regmap,
                                                                   &func_var, 0,
                                                                   -stack_ptr_offs,
                                                                   -new_stack_ptr_offs
                                                                   + spilled_regs_offs);
                            Some((reg, before))
                        }
                    };

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
                                JumpLabel(format!("{}", func_var.name.base_name()))));
                        }
                    }

                    // Restore caller-save registers.
                    for i in 0 .. regs_to_save_list.len() {
                        result.push(
                            InstNode::load(
                                TRUE_PRED,
                                load32_op,
                                regs_to_save_list[i],
                                STACK_POINTER,
                                -new_stack_ptr_offs + caller_save_offs + i as i32 * 4
                                    ));
                    }

                    // Restore the stack pointer.
                    if extra_stack_len > 0 {
                        result.push(
                            InstNode::alu2short(
                                TRUE_PRED,
                                SubAluOp,
                                STACK_POINTER,
                                STACK_POINTER,
                                extra_stack_len_base,
                                extra_stack_len_shift)
                        );
                    }

                },
                OpNode::AsmOp { ref insts } => {
                    result.push(InstNode::packets(insts.clone()));
                },
                OpNode::Nop {} => {},
            }
        }

        correspondence.push(result.len());
        (result, targets, regmap, opinfo, correspondence)
    }

    // Given a variable, return the register corresponding to it.  Also
    // return instructions that must be run before (for reads) and
    // afterwards (for writes), for it to be valid (in the case of
    // spilling). spill_pos must be 0, 1, or 2, and should not be re-used
    // while another register with the same spill_pos is active.
    // args_offs is the index relative to r30 after arguments on the stack
    // end; spill_offs is the start of the stack region where spilled registers
    // are put.
    fn var_to_reg(&mut self,
                  regmap: &BTreeMap<Var, RegisterColor>,
                  var: &Var,
                  spill_pos: u8,
                  args_offs: i32,
                  spill_offs: i32) -> (Reg, Vec<InstNode>, Vec<InstNode>) {
        let pred = Pred { inverted: false, reg: 3 };
        match *regmap.get(var).unwrap() {
            RegColor(reg) => {
                // It's an ordinary variable!
                (reg,
                 vec!(), vec!())
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
                                    spill_offs + pos as i32 * 4)
                 ),
                 vec!(
                     InstNode::store(pred,
                                     LsuOp { store: true,
                                             width: LsuWidthL },
                                     STACK_POINTER,
                                     spill_offs + pos as i32 * 4,
                                     reg)
                 ),
                )
            },
            StackArgColor(pos) => {
                // TODO: clean up these constants and document this.
                let reg = Reg { index: SPILL_REG_BASE + spill_pos };
                (reg,
                 vec!(
                     InstNode::load(pred,
                                    LsuOp { store: false,
                                            width: LsuWidthL },
                                    reg,
                                    STACK_POINTER,
                                    args_offs + pos as i32 * 4)
                         ),
                 vec!(
                     InstNode::store(pred,
                                     LsuOp { store: true,
                                             width: LsuWidthL },
                                     STACK_POINTER,
                                     args_offs + pos as i32 * 4,
                                     reg)
                         ),
                 )
            },
            GlobalColor => {
                let global_info = self.global_map.get(&var.name).unwrap();
                let label = format!("{}", global_info.label.expect("No label for global item"));
                let reg = Reg { index: SPILL_REG_BASE + spill_pos };
                if global_info.is_func {
                    // It's a function address. We need to load it into
                    // the register.
                    (reg,
                     vec!(InstNode::alu1long(
                          pred,
                          MovAluOp,
                          reg),
                          // Functions are represented by their base names, for compatibility
                          // with asm.
                          InstNode::long_label(format!("{}", var.name.base_name()))
                     ),
                     vec!())
                } else if global_info.is_ref {
                    (reg,
                     vec!(InstNode::alu1long(
                         pred,
                         MovAluOp,
                         reg),
                          InstNode::long_label(label.clone())
                          ),
                     vec!())
                } else {
                    (reg,
                     vec!(InstNode::alu1long(
                         pred,
                         MovAluOp,
                         GLOBAL_REG),
                          InstNode::long_label(label.clone()),
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
                          InstNode::long_label(label.clone()),
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

    fn assign_vars(&mut self,
                   regmap: &BTreeMap<Var, RegisterColor>,
                   pred: &Pred,
                   gens: &BTreeMap<VarName, usize>,
                   vars: &BTreeSet<Var>,
                   args_offs: i32,
                   spill_offs: i32) -> Vec<InstNode> {
        let mut result = vec!();

        let mut reg_transformations: BTreeMap<Reg, (Reg,
                                                    Vec<InstNode>,
                                                    Vec<InstNode>)> = BTreeMap::new();
        let mut rev_map: BTreeMap<Reg, Reg> = BTreeMap::new();

        for var in vars.iter() {
            // Globals don't need assignments.
            if self.global_map.get(&var.name).is_some() {
                continue;
            }

            let new_var = Var {
                name: var.name,
                generation: Some(*gens.get(&var.name).unwrap())
            };
            let (src_reg, src_insts, _) = self.var_to_reg(regmap, var, 1, args_offs, spill_offs);
            let (dest_reg, _, dest_insts) = self.var_to_reg(regmap, &new_var, 1,
                                                            args_offs, spill_offs);
            if src_reg != dest_reg {
                rev_map.insert(dest_reg, src_reg);
                reg_transformations.insert(src_reg,
                                           (dest_reg,
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
                            *pred,
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
                            *pred,
                            MovAluOp,
                            GLOBAL_REG,
                            src_reg,
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
                                *pred,
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
                            *pred,
                            MovAluOp,
                            dest_reg,
                            GLOBAL_REG,
                            SllShift,
                            0));
                    result.extend(dest_insts.into_iter());
                }
            }
        }
        result
    }

    /// Convert a binop into the internal asm representation.
    fn convert_binop<'b>(
        &mut self,
        regmap: &BTreeMap<Var, RegisterColor>,
        dest: Reg,
        op: &BinOpNode,
        signed: bool,
        mut op_l: &'b RValueElem,
        mut op_r: &'b RValueElem,
        args_offs: i32,
        spill_offs: i32,
        session: &mut Session) -> Vec<InstNode> {

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

        let (reg_l, before_l, _) = self.var_to_reg(regmap, &var_l, 1, args_offs, spill_offs);
        result.extend(before_l.into_iter());

        // TODO: handle shifts, multiplication, division.

        match *op_r {
            Variable(ref var) => {
                assert!(!swapped);
                let (reg_r, before_r, _) = self.var_to_reg(regmap, var, 2, args_offs, spill_offs);
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
                                        false,
                                        dest,
                                        reg_l,
                                        reg_r)),
                            ModOp => {
                                result.push(
                                    InstNode::div(
                                        Pred { inverted: false,
                                               reg: 3 },
                                        signed,
                                        false,
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

                let longval = lit_to_longvalue(val, session, &mut self.strings);
                let packed = match longval {
                    Immediate(num) => pack_int(num, 10),
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
                                            false,
                                            dest,
                                            GLOBAL_REG,
                                            reg_l)
                                    } else {
                                        InstNode::div(
                                            Pred { inverted: false,
                                                   reg: 3 },
                                            signed,
                                            false,
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
                                            false,
                                            dest,
                                            GLOBAL_REG,
                                            reg_l)
                                    } else {
                                        InstNode::div(
                                            Pred { inverted: false,
                                                   reg: 3 },
                                            signed,
                                            false,
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

    fn convert_unop<'b>(
        &mut self,
        regmap: &BTreeMap<Var, RegisterColor>,
        dest: Reg,
        op: &UnOpNode,
        rhs: &'b RValueElem,
        args_offs: i32,
        spill_offs: i32,
        session: &mut Session) -> Vec<InstNode> {

        let pred = Pred {
            inverted: false,
            reg: 3 };

        // This needs to be special cased.
        if *op == AddrOf {
            match *rhs {
                Variable(ref v) => {
                    match *regmap.get(v).unwrap() {
                        StackColor(n) => {
                            return vec!(InstNode::alu2short(TRUE_PRED,
                                                            SubAluOp,
                                                            dest,
                                                            STACK_POINTER,
                                                            -(spill_offs + n as i32 * 4) as u32,
                                                            0));
                        },
                        StackArgColor(n) => {
                            return vec!(InstNode::alu2short(TRUE_PRED,
                                                            SubAluOp,
                                                            dest,
                                                            STACK_POINTER,
                                                            -(args_offs - n as i32 * 4) as u32,
                                                            0));
                        },
                        GlobalColor => {
                            let global_info = self.global_map.get(&v.name).unwrap();
                            if global_info.is_extern {
                                return vec!(
                                    InstNode::alu1long(
                                        TRUE_PRED,
                                        MovAluOp,
                                        dest),
                                    InstNode::long_label(format!("{}", v.name.base_name())),
                                    );
                            } else {
                                let label = format!("{}", global_info.label.expect("No label for global item"));
                                return vec!(
                                    InstNode::alu1long(
                                        TRUE_PRED,
                                        MovAluOp,
                                        dest),
                                    InstNode::long_label(label));
                            }
                        },
                        GlobalReferenceColor => unimplemented!(),
                        RegColor(ref reg) => {
                            // If we're taking the address of something with a
                            // register color, it had better be a global function.
                            // In that case, the address of the function is stored
                            // in that register, and we can just copy it to the
                            // destination.
                            let global_info = self.global_map.get(&v.name);
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
                                                *reg,
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
                let (reg_r, mut before_r, _) = self.var_to_reg(regmap, var,
                                                               2, args_offs, spill_offs);
                before_r.extend(reg_op(reg_r).into_iter());
                before_r
            },
            Constant(ref val) => {
                if *op != Identity { panic!("Should have been constant folded. {}",
                                            op) };

                let mut result = vec!();
                let longval = lit_to_longvalue(val, session, &mut self.strings);
                let packed = match longval {
                    Immediate(num) => pack_1op_immediate(num, Some(MovAluOp)),
                    _ => None,
                };
                match packed {
                    Some((aluop, val, rot)) =>
                        result.push(
                            InstNode::alu1short(
                                pred,
                                aluop,
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
}
