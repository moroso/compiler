use mas::ast::*;

// TODO: add asserts all over the place here.

pub fn encode_pred(pred: &Pred) -> u32 {
    (if pred.inverted { 1<<29 } else { 0 }) | 
    (pred.reg as u32 << 30)
}

pub fn encode_aluop(op: &AluOp) -> u32 {
    *op as u32 << 10
}

pub fn encode_rd(rd: &Reg) -> u32 {
    rd.index as u32 << 5
}

pub fn encode_rs(rs: &Reg) -> u32 {
    rs.index as u32 << 0
}

pub fn encode_rt(rt: &Reg) -> u32 {
    rt.index as u32 << 14
}

pub fn encode_shift_type(shifttype: &ShiftType) -> u32 {
    *shifttype as u32 << 19
}

pub fn encode_lsuop(lsuop: &LsuOp) -> u32 {
    ((if lsuop.store { 1 } else { 0 } << 2) |
     (lsuop.width as u32)) << 10
}

pub fn encode_comparetype(comparetype: &CompareType) -> u32 {
    *comparetype as u32 << 7
}

pub fn encode(inst: &InstNode) -> u32 {
    match *inst {
        ALU1ShortInst(pred, // Instruction predicate
                      aluop, // Actual op
                      rd, // Rd
                      val, // Constant value
                      rot // Rotate amount
                      ) => {
            encode_pred(&pred) |
            encode_aluop(&aluop) |
            encode_rd(&rd) |
            ((val&0x3ff) << 18) |
            (((val&(0x1f << 10)) >> 10) << 0) |
            (rot as u32 << 14)
        },
        ALU2ShortInst(pred, // Instruction predicate
                      aluop, // Actual op
                      rd, // Rd
                      rs, // Rs
                      val, // Constant value
                      rot // Rotate amount
                      ) => {
            encode_pred(&pred) |
            encode_aluop(&aluop) |
            encode_rd(&rd) |
            encode_rs(&rs) |
            ((val&0x3ff) << 18) |
            (rot as u32 << 14)
        },
        ALU1RegInst(pred,
                    aluop,
                    rd, // Rd
                    rt, // Rt
                    shifttype,
                    shiftamt // Shift amount
                    ) => {
            (0b101 << 26) |
            encode_pred(&pred) |
            encode_aluop(&aluop) |
            encode_rd(&rd) |
            encode_rt(&rt) |
            encode_shift_type(&shifttype) |
            (shiftamt as u32 << 21)
        },
        ALU2RegInst(pred,
                    aluop,
                    rd, // Rd
                    rs, // Rs
                    rt, // Rt
                    shifttype,
                    shiftamt // Shift amount
                    ) => {
            (0b101 << 26) |
            encode_pred(&pred) |
            encode_aluop(&aluop) |
            encode_rd(&rd) |
            encode_rs(&rs) |
            encode_rt(&rt) |
            encode_shift_type(&shifttype) |
            (shiftamt as u32 << 21)        
        },
        ALU2LongInst(pred,
                     aluop,
                     rd, // Rd
                     rs // Rs
                     ) => {
            (1 << 28) |
            encode_pred(&pred) |
            encode_aluop(&aluop) |
            encode_rd(&rd) |
            encode_rs(&rs)
        },
        ALU1LongInst(pred,
                     aluop,
                     rd // Rd
                     ) => {
            (1 << 28) |
            encode_pred(&pred) |
            encode_aluop(&aluop) |
            encode_rd(&rd)
        },
        ALU1RegShInst(pred,
                      rd, // Rd
                      rs, // Rs
                      shifttype,
                      rt // Rt
                      ) => {
            (1<<28) |
            (1<<21) |
            encode_pred(&pred) |
            encode_rd(&rd) |
            encode_rs(&rs) |
            encode_rt(&rt) |
            encode_shift_type(&shifttype)
        },
        LongInst(arg) => {
            arg
        },
        NopInst => {
            encode_pred(&Pred { reg: 3,
                                inverted: true })
        },
        LoadInst(pred,
                 lsuop,
                 rd, // Rd
                 rs, // Rs
                 offs // Offset
                 ) => {
            (0b1001 << 25) |
            encode_pred(&pred) |
            encode_lsuop(&lsuop) |
            encode_rd(&rd) |
            encode_rs(&rs) |
            ((offs as u32 & 0xfff) << 13)
        },
        StoreInst(pred,
                  lsuop,
                  rs, // Rs
                  offs, // Offset
                  rt // Rt
                  ) => {
            (0b1001 << 25) |
            encode_pred(&pred) |
            encode_lsuop(&lsuop) |
            encode_rs(&rs) |
            encode_rt(&rt) |
            (((offs as u32 & (0x3f<<6)) >> 6) << 19) |
            (((offs as u32 & (1<<5)) >> 5) << 13) |
            ((offs as u32 & (0x1f)) << 5)
        },
        CompareShortInst(pred,
                         destpred, // Destination pred register
                         rs, // Rs
                         comparetype,
                         val, // Constant value
                         rot // Rotate amount
                         ) => {
            encode_pred(&pred) |
            encode_aluop(&CompareAluOp) |
            encode_comparetype(&comparetype) |
            encode_rs(&rs) |
            (destpred.reg as u32 << 5) |
            ((val&0x3ff) << 18) |
            (((val&(0x1f << 10)) >> 10) << 0) |
            (rot as u32 << 14)
        },
        CompareRegInst(pred,
                       destpred, // Destination pred register
                       rs, // Rs
                       comparetype,
                       rt, // Rt
                       shifttype,
                       shiftamt // Shift amount
                       ) => {
            (0b101 << 26) |
            encode_pred(&pred) |
            encode_aluop(&CompareAluOp) |
            encode_comparetype(&comparetype) |
            encode_rs(&rs) |
            encode_rt(&rt) |
            encode_shift_type(&shifttype) |
            (destpred.reg as u32 << 5) |
            (shiftamt as u32 << 21)  
        },
        BranchImmInst(pred,
                      link,
                      ref target) => {
            // TODO: adjust this once we have an object format to allow
            // unresolved symbols...
            let offs = match *target {
                JumpOffs(offs) => offs,
                JumpLabel(ref s) => fail!("Unresolved label {}", s),
            };
            (0b110 << 26) |
            encode_pred(&pred) |
            (offs as u32 & ((1<<25)-1)) |
            (if link { 1 << 25 } else { 0 })
        },
        BranchRegInst(pred,
                      link,
                      rs,
                      offs) => {
            (0b111 << 26) |
            encode_pred(&pred) |
            encode_rs(&rs) |
            (offs as u32 & ((1<<25)-1)) |
            (if link { 1 << 25 } else { 0 })
        },
        BreakInst(pred,
                  val) => {
            (0b100010001 << 20) |
            encode_pred(&pred) |
            val
        },
        SyscallInst(pred,
                    val) => {
            (0b100010010 << 20) |
            encode_pred(&pred) |
            val
        }
        MtcInst(pred,
                coreg,
                rs) => {
            (0b100010111 << 20) |
            encode_pred(&pred) |
            (coreg as u32 << 5) |
            encode_rs(&rs)
        }
        MfcInst(pred,
                rd,
                coreg) => {
            (0b100010110 << 20) |
            encode_pred(&pred) |
            (coreg as u32) |
            encode_rd(&rd)
        }
        EretInst(pred) => {
            (0b100010100 << 20) |
            encode_pred(&pred)
        }
        FenceInst(pred) => {
            (0b100010011 << 20) |
            encode_pred(&pred)
        }
        MfhiInst(pred,
                 rd) => {
            (0b100011010 << 20) |
            encode_pred(&pred) |
            encode_rd(&rd)
        }
        MthiInst(pred,
                 rs) => {
            (0b100011011 << 20) |
            encode_pred(&pred) |
            encode_rs(&rs)
        }
        MultInst(pred,
                 signed,
                 rd,
                 rs,
                 rt) => {
            (0b100011000 << 20) |
            encode_pred(&pred) |
            (if signed { 1<<19 } else { 0 }) |
            encode_rd(&rd) |
            encode_rs(&rs) |
            encode_rt(&rt)
        }
        DivInst(pred,
                signed,
                rd,
                rs,
                rt) => {
            (0b100011001 << 20) |
            encode_pred(&pred) |
            (if signed { 1<<19 } else { 0 }) |
            encode_rd(&rd) |
            encode_rs(&rs) |
            encode_rt(&rt)
        }
        FlushInst(pred,
                  flushtype,
                  rs) => {
            (0b100010101 << 20) |
            encode_pred(&pred) |
            (flushtype as u32 << 10) |
            encode_rs(&rs)
        }
    }
}