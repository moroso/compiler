// Internal representation of Moroso asm instructions.

// Note that we actually use the numeric values for the items in these enums.
// DO NOT REORDER unless we're changing the instruction encoding in the
// processor!

use std::fmt;
use std::fmt::{Formatter, Show};

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

#[deriving(Clone, Eq, PartialEq)]
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

// Compare types.
pub enum CompareType {
    CmpLTU,
    CmpLEU,
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
                Reg, // Rs
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
}
