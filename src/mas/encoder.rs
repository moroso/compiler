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
        }      
   }
}