// Internal representation of Moroso asm instructions.

// Note that we actually use the numeric values for the items in these enums.
// DO NOT REORDER unless we're changing the instruction encoding in the
// processor!

use std::fmt;
use std::fmt::{Formatter, Show};
use mas::util::fits_in_bits;

// A predicate that is always true.
pub static true_pred: Pred = Pred {
    inverted: false,
    reg: 3,
};

// The link register.
pub static link_reg: Reg = Reg {
    index: 31,
};



#[deriving(Clone, Eq, PartialEq)]
pub struct Pred {
    pub inverted: bool,
    pub reg: u8, // Can only take the values 0-3.
}

impl Show for Pred {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}p{}",
               if self.inverted { "!" } else { "" },
               self.reg)
    }
}

#[deriving(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Reg {
    pub index: u8,
}

impl Show for Reg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "r{}", self.index)
    }
}

// Opcodes for the ALU.
#[deriving(Show, Eq, PartialEq)]
pub enum AluOp {
    AddAluOp,
    AndAluOp,
    NorAluOp,
    OrAluOp,
    SubAluOp,
    RsbAluOp,
    XorAluOp,
    CompareAluOp,
    MovAluOp,
    MvnAluOp,
    SxbAluOp,
    SxhAluOp,
    // The rest are reserved.
}

impl AluOp {
    pub fn is_unary(&self) -> bool {
        match *self {
            MovAluOp |
            MvnAluOp |
            SxbAluOp |
            SxhAluOp => true,
            _ => false,
        }
    }

    pub fn is_binary(&self) -> bool {
        match *self {
            AddAluOp |
            AndAluOp |
            NorAluOp |
            OrAluOp |
            SubAluOp |
            RsbAluOp |
            XorAluOp => true,
            _ => false,
        }
    }

    pub fn is_compare(&self) -> bool {
        *self == CompareAluOp
    }
}

// Compare types.
#[deriving(Show, Eq, PartialEq)]
pub enum CompareType {
    CmpLTU,
    CmpLEU,
    CmpEQ,
    CmpRESERVED,
    CmpLTS,
    CmpLES,
    CmpBS,
    CmpBC,
}

// Shift types.
#[deriving(Clone, Eq, PartialEq, FromPrimitive)]
pub enum ShiftType {
    SllShift,
    SraShift,
    SrlShift,
    RorShift,
}

// Load/Store types.
#[deriving(Clone, Eq, PartialEq, Show)]
pub enum LsuWidth {
    LsuWidthB,
    LsuWidthH,
    LsuWidthL,
    LsuLLSC, // Not really a width, but...
}

#[deriving(Clone, Eq, PartialEq, Show)]
pub struct LsuOp {
    pub store: bool,
    pub width: LsuWidth,
}

impl Show for ShiftType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "r{}",
               match *self {
                   SllShift => "<<",
                   SraShift => ">>s",
                   SrlShift => ">>u",
                   RorShift => ">>r",
               }
        )
    }
}

#[deriving(Show, Eq, PartialEq)]
pub enum InstNode {
    ALU1ShortInst(Pred, // Instruction predicate
                  AluOp, // Actual op
                  Reg, // Rd
                  u32, // Constant value
                  u8 // Rotate amount
                  ),
    ALU2ShortInst(Pred, // Instruction predicate
                  AluOp, // Actual op
                  Reg, // Rd
                  Reg, // Rs
                  u32, // Constant value
                  u8 // Rotate amount
                  ),
    ALU1RegInst(Pred,
                AluOp,
                Reg, // Rd
                Reg, // Rt
                ShiftType,
                u8 // Shift amount
                ),
    ALU2RegInst(Pred,
                AluOp,
                Reg, // Rd
                Reg, // Rs
                Reg, // Rt
                ShiftType,
                u8 // Shift amount
                ),
    ALU2LongInst(Pred,
                 AluOp,
                 Reg, // Rd
                 Reg // Rs
                 ),
    ALU1LongInst(Pred,
                 AluOp,
                 Reg // Rd
                 ),
    ALU1RegShInst(Pred,
                  Reg, // Rd
                  Reg, // Rs
                  ShiftType,
                  Reg // Rt
                  ),
    LongInst(u32),
    NopInst,
    LoadInst(Pred,
             LsuOp,
             Reg, // Rd
             Reg, // Rs
             i32 // Offset
             ),
    StoreInst(Pred,
              LsuOp,
              Reg, // Rs
              i32, // Offset
              Reg // Rt
              ),
    CompareShortInst(Pred,
                     Pred, // Destination pred register
                     Reg, // Rs
                     CompareType,
                     u32, // Constant value
                     u8 // Rotate amount
                     ),
    CompareRegInst(Pred,
                   Pred, // Destination pred register
                   Reg, // Rs
                   CompareType,
                   Reg, // Rt
                   ShiftType,
                   u8 // Shift amount
                   ),
}

fn assert_pred(pred: Pred) {
    assert!(pred.reg < 4);
}

fn assert_reg(reg: Reg) {
    assert!(reg.index < 32);
}

fn assert_offs(offs: i32, width: u8) {
    let mask = (-1 as u32) << width;
    // Check that the sign bit is extended all the way.
    assert_eq!((mask & (offs as u32) == mask),
               (((offs as u32) & (1<<(width-1)) != 0)));
    // No sign bit means none of the high bits should be set.
    assert_eq!((mask & (offs as u32) == 0),
               ((offs as u32) & (1<<(width-1))) == 0);
}

// Helper functions that will make instruction representations, and also
// assert that all of the fields are valid.
impl InstNode {
    pub fn alu1short(pred: Pred, // Instruction predicate
                     aluop: AluOp, // Actual op
                     rd: Reg, // Rd
                     val: u32, // Constant value
                     rot: u8 // Rotate amount
                     ) -> InstNode {
        assert_pred(pred);
        assert!(aluop.is_unary());
        assert_reg(rd);
        assert!(fits_in_bits(val, 15));
        assert!(fits_in_bits(rot as u32, 4));
        ALU1ShortInst(pred,
                      aluop,
                      rd,
                      val,
                      rot)
    }
    pub fn alu2short(pred: Pred, // Instruction predicate
                     aluop: AluOp, // Actual op
                     rd: Reg, // Rd
                     rs: Reg, // Rs
                     val: u32, // Constant value
                     rot: u8 // Rotate amount
                     ) -> InstNode {
        assert_pred(pred);
        assert!(aluop.is_binary());
        assert_reg(rd);
        assert_reg(rs);
        assert!(fits_in_bits(val, 10));
        assert!(fits_in_bits(rot as u32, 4));
        ALU2ShortInst(pred,
                      aluop,
                      rd,
                      rs,
                      val,
                      rot)
    }
    pub fn alu1reg(pred: Pred,
                   aluop: AluOp,
                   rd: Reg, // Rd
                   rt: Reg, // Rt
                   shifttype: ShiftType,
                   amt: u8 // Shift amount
                   ) -> InstNode {
        assert_pred(pred);
        assert!(aluop.is_unary());
        assert_reg(rd);
        assert_reg(rt);
        assert!(fits_in_bits(amt as u32, 5));
        ALU1RegInst(pred,
                    aluop,
                    rd,
                    rt,
                    shifttype,
                    amt)
    }
    pub fn alu2reg(pred: Pred,
                   aluop: AluOp,
                   rd: Reg, // Rd
                   rs: Reg, // Rs
                   rt: Reg, // Rt
                   shifttype: ShiftType,
                   amt: u8 // Shift amount
                   ) -> InstNode {
        assert_pred(pred);
        assert!(aluop.is_binary());
        assert_reg(rd);
        assert_reg(rs);
        assert_reg(rt);
        assert!(fits_in_bits(amt as u32, 5));
        ALU2RegInst(pred,
                    aluop,
                    rd,
                    rs,
                    rt,
                    shifttype,
                    amt)
    }
    pub fn alu2long(pred: Pred,
                    aluop: AluOp,
                    rd: Reg, // Rd
                    rs: Reg // Rs
                    ) -> InstNode {
        assert_pred(pred);
        assert!(aluop.is_binary());
        assert_reg(rd);
        assert_reg(rs);
        ALU2LongInst(pred,
                     aluop,
                     rd,
                     rs)
    }
    pub fn alu1long(pred: Pred,
                    aluop: AluOp,
                    rd: Reg // Rd
                    ) -> InstNode {
        assert_pred(pred);
        assert!(aluop.is_unary());
        assert_reg(rd);
        ALU1LongInst(pred,
                     aluop,
                     rd)
    }
    pub fn alu1regsh(pred: Pred,
                     rd: Reg, // Rd
                     rs: Reg, // Rs
                     shifttype: ShiftType,
                     rt: Reg // Rt
                     ) -> InstNode {
        assert_pred(pred);
        assert_reg(rd);
        assert_reg(rs);
        assert_reg(rt);
        ALU1RegShInst(pred,
                      rd,
                      rs,
                      shifttype,
                      rt)
    }
    pub fn long(val: u32) -> InstNode {
        LongInst(val)
    }
    pub fn nop() -> InstNode { NopInst }
    pub fn load(pred: Pred,
                lsuop: LsuOp,
                rd: Reg, // Rd
                rs: Reg, // Rs
                offs: i32 // Offset
                ) -> InstNode {
        assert_pred(pred);
        assert_reg(rd);
        assert_reg(rs);
        assert_offs(offs, 12);
        LoadInst(pred,
                 lsuop,
                 rd,
                 rs,
                 offs)
    }
    pub fn store(pred: Pred,
                 lsuop: LsuOp,
                 rs: Reg, // Rs
                 offs: i32, // Offset
                 rt: Reg // Rt
                 ) -> InstNode {
        assert_pred(pred);
        assert_reg(rs);
        assert_reg(rt);
        assert_offs(offs, 12);
        StoreInst(pred,
                  lsuop,
                  rs,
                  offs,
                  rt)
    }
    pub fn compareshort(pred: Pred,
                        pred2: Pred, // Destination pred register
                        rs: Reg, // Rs
                        comparetype: CompareType,
                        val: u32, // Constant value
                        rot: u8 // Rotate amount
                        ) -> InstNode {
        assert_pred(pred);
        assert_pred(pred2);
        assert_eq!(pred2.inverted, false);
        assert_reg(rs);
        assert!(fits_in_bits(val, 10));
        assert!(fits_in_bits(rot as u32, 4));
        CompareShortInst(pred,
                         pred2,
                         rs,
                         comparetype,
                         val,
                         rot)
    }
    pub fn comparereg(pred: Pred,
                      pred2: Pred, // Destination pred register
                      rs: Reg, // Rs
                      comparetype: CompareType,
                      rt: Reg, // Rt
                      shifttype: ShiftType,
                      amt: u8 // Shift amount
                      ) -> InstNode {
        assert_pred(pred);
        assert_pred(pred2);
        assert_eq!(pred2.inverted, false);
        assert_reg(rs);
        assert_reg(rt);
        assert!(fits_in_bits(amt as u32, 5));
        CompareRegInst(pred,
                       pred2,
                       rs,
                       comparetype,
                       rt,
                       shifttype,
                       amt)
    }

}

pub type InstPacket = [InstNode, ..4];

impl Show for InstPacket {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{{ {}; {}; {}; {} }}",
               self[0], self[1], self[2], self[3])
    }
}