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
