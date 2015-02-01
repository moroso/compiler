// Internal representation of Moroso asm instructions.

// Note that we actually use the numeric values for the items in these enums.
// DO NOT REORDER unless we're changing the instruction encoding in the
// processor!

use std::fmt;
use std::fmt::{Formatter, Display, Debug};
use mas::util::fits_in_bits;
use std::ops::Index;
use std::option::IterMut;

pub use self::CoReg::*;
pub use self::AluOp::*;
pub use self::CompareType::*;
pub use self::ShiftType::*;
pub use self::LsuWidth::*;
pub use self::FlushType::*;
pub use self::JumpTarget::*;
pub use self::LongValue::*;
pub use self::InstNode::*;

// A predicate that is always true.
pub static true_pred: Pred = Pred {
    inverted: false,
    reg: 3,
};

// The link register.
pub static link_reg: Reg = Reg {
    index: 31,
};



#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Copy)]
pub struct Pred {
    pub inverted: bool,
    pub reg: u8, // Can only take the values 0-3.
}

impl Display for Pred {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}p{}",
               if self.inverted { "!" } else { "" },
               self.reg)
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Copy)]
pub struct Reg {
    pub index: u8,
}

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "r{}", self.index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum CoReg {
    PFLAGS,
    PTB,
    EHA,
    EPC,
    EC0,
    EC1,
    EC2,
    EC3,
    EA0,
    EA1,
    SP0 = 16,
    SP1,
    SP2,
    SP3,
}

// Opcodes for the ALU.
#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
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
#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
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
#[derive(Clone, Eq, PartialEq, FromPrimitive, Ord, PartialOrd, Debug)]
pub enum ShiftType {
    SllShift,
    SrlShift,
    SraShift,
    RorShift,
}

// Load/Store types.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum LsuWidth {
    LsuWidthB,
    LsuWidthH,
    LsuWidthL,
    LsuLLSC, // Not really a width, but...
}

// Flush types
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum FlushType {
    DataFlush,
    InstFlush,
    DtlbFlush,
    ItlbFlush,
}

#[derive(Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
pub struct LsuOp {
    pub store: bool,
    pub width: LsuWidth,
}

impl Display for ShiftType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   SllShift => "<<",
                   SraShift => ">>s",
                   SrlShift => ">>u",
                   RorShift => ">>r",
               }
        )
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
pub enum JumpTarget {
    JumpOffs(i32),
    // TODO: allow arithmetic on labels.
    JumpLabel(String),
}

#[derive(Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
pub enum LongValue {
    Immediate(u32),
    LabelOffs(String),
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
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
                  AluOp,
                  Reg, // Rt
                  ShiftType,
                  Reg // Rs
                  ),
    LongInst(LongValue),
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
    CompareLongInst(Pred,
                    Pred, // Destination pred register
                    Reg, // Rs
                    CompareType
                    ),
    BranchImmInst(Pred,
                  bool, // link
                  JumpTarget),
    BranchRegInst(Pred,
                  bool, // link,
                  Reg, // Rs,
                  i32 // Offset
                  ),
    BreakInst(Pred,
              u32 // "Don't care" fields.
              ),
    SyscallInst(Pred,
                u32 // "Don't care" fields.
                ),
    MtcInst(Pred,
            CoReg,
            Reg // Rs
            ),
    MfcInst(Pred,
            Reg, // Rd
            CoReg
            ),
    EretInst(Pred),
    FenceInst(Pred),
    MthiInst(Pred,
             Reg // Rs
             ),
    MfhiInst(Pred,
             Reg // Rd
             ),
    MultInst(Pred,
             bool, // signed?
             Reg, // Rd
             Reg, // Rs
             Reg // Rt
             ),
    DivInst(Pred,
            bool, // signed?
            Reg, // Rd
            Reg, // Rs
            Reg // Rt
            ),
    FlushInst(Pred,
              FlushType,
              Reg // Rs
              )
}

fn assert_pred(pred: Pred) {
    assert!(pred.reg < 4);
}

fn assert_reg(reg: Reg) {
    assert!(reg.index < 32);
}

fn assert_offs(offs: i32, width: u8) {
    let mask = (-1 as u32) << (width as uint);
    // Check that the sign bit is extended all the way.
    assert_eq!((mask & (offs as u32) == mask),
               (((offs as u32) & (1<<((width-1) as uint)) != 0)));
    // No sign bit means none of the high bits should be set.
    assert_eq!((mask & (offs as u32) == 0),
               ((offs as u32) & (1<<((width-1) as uint))) == 0);
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
                     aluop: AluOp,
                     rt: Reg, // Rt
                     shifttype: ShiftType,
                     rs: Reg // Rs
                     ) -> InstNode {
        assert_pred(pred);
        assert_reg(rd);
        assert!(aluop.is_unary());
        assert_reg(rs);
        assert_reg(rt);
        ALU1RegShInst(pred,
                      rd,
                      aluop,
                      rt,
                      shifttype,
                      rs)
    }
    pub fn long(val: u32) -> InstNode {
        LongInst(Immediate(val))
    }
    pub fn long_label(label: String) -> InstNode {
        LongInst(LabelOffs(label))
    }
    pub fn anylong(arg: LongValue) -> InstNode {
        LongInst(arg)
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
        assert!(!lsuop.store);
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
        assert!(lsuop.store);
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
    pub fn comparelong(pred: Pred,
                       pred2: Pred, // Destination pred register
                       rs: Reg, // Rs
                       comparetype: CompareType) -> InstNode {
        assert_pred(pred);
        assert_pred(pred2);
        assert_eq!(pred2.inverted, false);
        assert_reg(rs);

        CompareLongInst(pred,
                        pred2,
                        rs,
                        comparetype)
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
    pub fn branchimm(pred: Pred,
                     link: bool, // link
                     target: JumpTarget) -> InstNode {
        assert_pred(pred);
        match target {
            JumpOffs(offs) => assert_offs(offs, 25),
            _ => {},
        }
        BranchImmInst(pred,
                      link,
                      target)
    }
    pub fn branchreg(pred: Pred,
                     link: bool, // link,
                     rs: Reg, // Rs,
                     offs: i32 // Offset
                     ) -> InstNode {
        assert_pred(pred);
        assert_reg(rs);
        assert_offs(offs, 25);
        BranchRegInst(pred,
                      link,
                      rs,
                      offs)
    }
    pub fn breaknum(pred: Pred,
                    val: u32
                    ) -> InstNode {
        assert_pred(pred);
        assert!(fits_in_bits(val as u32, 5));
        BreakInst(pred,
                  val)
    }
    pub fn syscall(pred: Pred,
                   val: u32
                   ) -> InstNode {
        assert_pred(pred);
        assert!(fits_in_bits(val as u32, 5));
        SyscallInst(pred,
                    val)
    }
    pub fn mtc(pred: Pred,
               coreg: CoReg,
               rs: Reg // Rs
               ) -> InstNode {
        assert_pred(pred);
        assert_reg(rs);
        MtcInst(pred,
                coreg,
                rs)
    }
    pub fn mfc(pred: Pred,
               rd: Reg, // Rs
               coreg: CoReg
               ) -> InstNode {
        assert_pred(pred);
        assert_reg(rd);
        MfcInst(pred,
                rd,
                coreg)
    }
    pub fn eret(pred: Pred
                ) -> InstNode {
        assert_pred(pred);
        EretInst(pred)
    }

    pub fn fence(pred: Pred
                 ) -> InstNode {
        assert_pred(pred);
        FenceInst(pred)
    }
    pub fn mfhi(pred: Pred,
                rd: Reg
                ) -> InstNode {
        assert_pred(pred);
        assert_reg(rd);
        MfhiInst(pred, rd)
    }
    pub fn mthi(pred: Pred,
                rs: Reg
                ) -> InstNode {
        assert_pred(pred);
        assert_reg(rs);
        MthiInst(pred, rs)
    }
    pub fn mult(pred: Pred,
                signed: bool, // signed?
                rd: Reg, // Rd
                rs: Reg, // Rs
                rt: Reg // Rt
                ) -> InstNode {
        assert_pred(pred);
        assert_reg(rd);
        assert_reg(rs);
        assert_reg(rt);
        MultInst(pred,
                 signed,
                 rd,
                 rs,
                 rt)
    }
    pub fn div(pred: Pred,
               signed: bool, // signed?
               rd: Reg, // Rd
               rs: Reg, // Rs
               rt: Reg // Rt
               ) -> InstNode {
        assert_pred(pred);
        assert_reg(rd);
        assert_reg(rs);
        assert_reg(rt);
        DivInst(pred,
                signed,
                rd,
                rs,
                rt)
    }
    pub fn flush(pred: Pred,
                 flushtype: FlushType,
                 rs: Reg // Rs
                 ) -> InstNode {
        assert_pred(pred);
        assert_reg(rs);
        FlushInst(pred,
                  flushtype,
                  rs)
    }
}

allow_string!(InstNode);

//pub struct InstPacket([InstNode; 4]);
pub type InstPacket = [InstNode; 4];
allow_string!(InstPacket);
/*
impl Index<uint> for InstPacket {
    type Output = InstNode;
    fn index<'a>(&'a self, i: &uint) -> &'a InstNode {
        let InstPacket(ref packet) = *self;
        &packet[*i]
    }
}

impl InstPacket {
    fn iter_mut(&mut self) -> IterMut<InstNode> {
        let InstPacket(ref mut packet) = *self;
        packet.iter_mut()
    }
}

impl Display for InstPacket {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let InstPacket(ref packet) = *self;
        write!(f, "{{ {}; {}; {}; {} }}",
               packet[0], packet[1], packet[2], packet[3])
    }
}
*/
