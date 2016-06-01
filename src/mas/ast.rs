// Internal representation of Moroso asm instructions.

// Note that we actually use the numeric values for the items in these enums.
// DO NOT REORDER unless we're changing the instruction encoding in the
// processor!

use std::fmt;
use std::fmt::{Formatter, Display, Debug};
use mas::util::{fits_in_bits, ror};
use std::ops::Index;
use std::option::IterMut;
use num::FromPrimitive;

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
pub static TRUE_PRED: Pred = Pred {
    inverted: false,
    reg: 3,
};

// The link register.
pub static LINK_REG: Reg = Reg {
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

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Copy)]
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

impl Display for CoReg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", *self)
    }
}

// Opcodes for the ALU.
#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Copy)]
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

impl Display for AluOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   AddAluOp => "+",
                   AndAluOp => "&",
                   NorAluOp => "~|",
                   OrAluOp => "|",
                   SubAluOp => "-",
                   RsbAluOp => "-:",
                   XorAluOp => "^",
                   CompareAluOp => "[CMP]",
                   MovAluOp => "",
                   MvnAluOp => "~",
                   SxbAluOp => "SXB ",
                   SxhAluOp => "SXH ",
               })
    }
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
#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Copy)]
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

impl Display for CompareType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   CmpLTU => "<",
                   CmpLEU => "<=",
                   CmpEQ => "==",
                   CmpRESERVED => "[RESV]",
                   CmpLTS => "<s",
                   CmpLES => "<=s",
                   CmpBS => "&",
                   CmpBC => "BC",
               })
    }
}

// Shift types.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Copy)]
pub enum ShiftType {
    SllShift,
    SrlShift,
    SraShift,
    RorShift,
}

// XXX: FromPrimitive can be derived from num-macros, but we only use
// it once and num-macros has had some breakage since it is a compiler
// plugin.
impl FromPrimitive for ShiftType {
    fn from_u64(n: u64) -> Option<Self> {
        match n {
            0 => Some(SllShift),
            1 => Some(SrlShift),
            2 => Some(SraShift),
            3 => Some(RorShift),
            _ => None
        }
    }
    fn from_i64(n: i64) -> Option<Self> { FromPrimitive::from_u64(n as u64) }

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

// Load/Store types.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Copy)]
pub enum LsuWidth {
    LsuWidthB,
    LsuWidthH,
    LsuWidthL,
    LsuLLSC, // Not really a width, but...
}

impl Display for LsuWidth {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   LsuWidthB => "b",
                   LsuWidthH => "h",
                   LsuWidthL => "l",
                   LsuLLSC => "llsc",
               })
    }
}

// Flush types
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Copy)]
pub enum FlushType {
    DataFlush,
    InstFlush,
    DtlbFlush,
    ItlbFlush,
}

impl Display for FlushType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   DataFlush => "data",
                   InstFlush => "inst",
                   DtlbFlush => "dtlb",
                   ItlbFlush => "itlb",
               })
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Ord, PartialOrd, Copy)]
pub struct LsuOp {
    pub store: bool,
    pub width: LsuWidth,
}

impl Display for LsuOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.width)
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
pub enum JumpTarget {
    JumpOffs(i32),
    // TODO: allow arithmetic on labels.
    JumpLabel(String),
}

impl Display for JumpTarget {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            JumpOffs(val) => write!(f, "{}", val),
            JumpLabel(ref label) => write!(f, "{}", label),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
pub enum LongValue {
    Immediate(u32),
    LabelOffs(String),
}

impl Display for LongValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Immediate(val) => write!(f, "{}", val),
            LabelOffs(ref label) => write!(f, "{}", label),
        }
    }
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
            bool, // wide?
            Reg, // Rd
            Reg, // Rs
            Reg // Rt
            ),
    FlushInst(Pred,
              FlushType,
              Reg // Rs
              ),

    // This is special: rather that being a CPU instruction itself, this represents a stream
    // of instruction packets. It is only used for inline ASM, which has to be passed all the
    // way from the parser down to the instruction scheduler. The scheduler will take this
    // "instruction" and break it into its parts.
    PacketsInst(Vec<Vec<InstNode>>),
}

fn assert_pred(pred: Pred) {
    assert!(pred.reg < 4);
}

fn assert_reg(reg: Reg) {
    assert!(reg.index < 32);
}

fn assert_offs(offs: i32, width: u8) {
    let mask = !0u32 << (width as usize);
    // Check that the sign bit is extended all the way.
    assert_eq!((mask & (offs as u32) == mask),
               (((offs as u32) & (1<<((width-1) as usize)) != 0)));
    // No sign bit means none of the high bits should be set.
    assert_eq!((mask & (offs as u32) == 0),
               ((offs as u32) & (1<<((width-1) as usize))) == 0);
}

fn format_pred(pred: Pred) -> String {
    if pred == TRUE_PRED {
        format!("")
    } else {
        format!("{}? ", pred)
    }
}

fn format_shifted_reg(reg: Reg, shifttype: ShiftType, shiftamt: u8) -> String {
    if shiftamt == 0 {
        format!("{}", reg)
    } else {
        format!("({} {} {})", reg, shifttype, shiftamt)
    }
}

fn format_reg_offs(reg: Reg, offs: i32) -> String {
    if offs == 0 {
        format!("{}", reg)
    } else if offs > 0 {
        format!("{} + {}", reg, offs)
    } else {
        format!("{} - {}", reg, -offs)
    }
}


impl Display for InstNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ALU1ShortInst(pred,
                          op,
                          rd,
                          val,
                          rot
                          ) => {
                write!(f, "{}{} <- {}{}",
                       format_pred(pred),
                       rd, op, ror(val, rot))
            }
            ALU2ShortInst(pred,
                          op,
                          rd,
                          rs,
                          val,
                          rot
                          ) => {
                write!(f, "{}{} <- {} {} {}",
                       format_pred(pred),
                       rd, rs, op, ror(val, rot))
            }
            ALU1RegInst(pred,
                        op,
                        rd,
                        rt,
                        shifttype,
                        shiftamt
                        ) => {
                write!(f, "{}{} <- {}{}",
                       format_pred(pred),
                       rd, op, format_shifted_reg(rt, shifttype, shiftamt))
            }
            ALU2RegInst(pred,
                        op,
                        rd,
                        rs,
                        rt,
                        shifttype,
                        shiftamt
                        ) => {
                write!(f, "{}{} <- {} {} {}",
                       format_pred(pred),
                       rd, rs, op,
                       format_shifted_reg(rt, shifttype, shiftamt))
            }
            ALU2LongInst(pred,
                         op,
                         rd,
                         rs,
                         ) => {
                write!(f, "{}{} <- {} {} long",
                       format_pred(pred),
                       rd, rs, op)
            }
            ALU1LongInst(pred,
                         op,
                         rd
                         ) => {
                write!(f, "{}{} <- {}long",
                       format_pred(pred),
                       rd, op)
            }
            ALU1RegShInst(pred,
                          rd,
                          op,
                          rt,
                          shifttype,
                          rs
                          ) => {
                write!(f, "{}{} <- {}({} {} {})",
                       format_pred(pred),
                       rd, op, rt, shifttype, rs)
            }
            LongInst(ref longval) => {
                write!(f, "long {}",
                       longval)
            }
            NopInst => {
                write!(f, "nop")
            }
            LoadInst(pred,
                     op,
                     rd,
                     rs,
                     offs
                     ) => {
                write!(f, "{}{} <- *{}({})",
                       format_pred(pred),
                       rd, op, format_reg_offs(rs, offs))
            }
            StoreInst(pred,
                      op,
                      rs,
                      offs,
                      rt
                      ) => {
                write!(f, "{}*{}({}) <- {}",
                       format_pred(pred),
                       op,
                       format_reg_offs(rs, offs),
                       rt)
            }
            CompareShortInst(pred,
                             destpred,
                             rs,
                             comparetype,
                             val,
                             rot
                             ) => {
                write!(f, "{}p{} <- {} {} {}",
                       format_pred(pred),
                       destpred.reg,
                       rs, comparetype, ror(val, rot))
            }
            CompareRegInst(pred,
                           destpred,
                           rs,
                           comparetype,
                           rt,
                           shifttype,
                           shiftamt
                           ) => {
                write!(f, "{}p{} <- {} {} {}",
                       format_pred(pred),
                       destpred.reg,
                       rs, comparetype,
                       format_shifted_reg(rt, shifttype, shiftamt))
            }
            CompareLongInst(pred,
                            destpred,
                            rs,
                            comparetype
                            ) => {
                write!(f, "{}p{} <- {} {} long",
                       format_pred(pred),
                       destpred.reg,
                       rs, comparetype)
            }
            BranchImmInst(pred,
                          linked,
                          ref target) => {
                write!(f, "{}{} {}",
                       format_pred(pred),
                       if linked { "bl" } else { "b" },
                       target)
            }
            BranchRegInst(pred,
                          linked,
                          rs,
                          offs
                          ) => {
                write!(f, "{}{} {}",
                       format_pred(pred),
                       if linked { "bl" } else { "b" },
                       format_reg_offs(rs, offs))
            }
            BreakInst(pred,
                      val
                      ) => {
                if val == 0 {
                    write!(f, "{}break", format_pred(pred))
                } else {
                    write!(f, "{}break {}", format_pred(pred), val)
                }
            }
            SyscallInst(pred,
                        val
                        ) => {
                if val == 0 {
                    write!(f, "{}syscall", format_pred(pred))
                } else {
                    write!(f, "{}syscall {}", format_pred(pred), val)
                }
            }
            MtcInst(pred,
                    coreg,
                    rs
                    ) => {
                write!(f, "{}{} <- {}",
                       format_pred(pred),
                       coreg, rs)
            }
            MfcInst(pred,
                    rd,
                    coreg
                    ) => {
                write!(f, "{}{} <- {}",
                       format_pred(pred),
                       rd, coreg)
            }
            EretInst(pred) => {
                write!(f, "{}eret",
                       format_pred(pred))
            }
            FenceInst(pred) => {
                write!(f, "{}fence",
                       format_pred(pred))
            }
            MthiInst(pred,
                     rd
                     ) => {
                write!(f, "{}ovf <- {}",
                       format_pred(pred),
                       rd)
            }
            MfhiInst(pred,
                     rs
                     ) => {
                write!(f, "{}{} <- ovf",
                       format_pred(pred),
                       rs)
            }
            MultInst(pred,
                     signed,
                     rd,
                     rs,
                     rt
                     ) => {
                write!(f, "{}{} <- {} {} {}",
                       format_pred(pred),
                       rd, rs, if signed { "*s" } else { "*" }, rt)
            }
            DivInst(pred,
                    signed,
                    wide,
                    rd,
                    rs,
                    rt
                    ) => {
                write!(f, "{}{} <- {} {}{} {}",
                       format_pred(pred),
                       rd, rs, if signed { "/s" } else { "/" },
                       if wide { "w" } else { "s" },
                       rt)
            }
            FlushInst(pred,
                      flushtype,
                      rs
                      ) => {
                write!(f, "{}flush.{} {}",
                       format_pred(pred),
                       flushtype,
                       rs)
            }
            PacketsInst(..) => {
                write!(f, "[INLINE ASM]")
            }
        }
    }
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
               wide: bool, // wide?
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
                wide,
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
    pub fn packets(packets: Vec<Vec<InstNode>>) -> InstNode {
        PacketsInst(packets)
    }
}

pub type InstPacket = [InstNode; 4];
