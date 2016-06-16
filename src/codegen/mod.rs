use mas::ast::Reg;

use std::fmt;
use std::fmt::{Formatter, Display};
pub use codegen::ir_to_asm::IrToAsm;
pub use self::RegisterColor::*;

pub mod register_color;
pub mod ir_to_asm;
pub mod combine;

/// How many variables are available to the register allocator.
pub static NUM_USABLE_VARS: usize = 30;

// Special registers.
pub static LINK_REGISTER: Reg = Reg { index: 31 };
pub static STACK_POINTER: Reg = Reg { index: 30 };
pub static FIRST_CALLEE_SAVED_REG: Reg = Reg { index: 11 };
pub static RETURN_REG: Reg = Reg { index: 0 };
// This register is used for pointers into global storage, or for other things
// that need a temporary register briefly.
pub static GLOBAL_REG: Reg = Reg { index: 11 };

// How many parameters we can pass in registers.
pub static NUM_PARAM_REGS: usize = 8;

// We use three registers, starting at this index, for spilled registers.
pub static SPILL_REG_BASE: u8 = 8;

pub static GLOBAL_MEM_START: u32 = 0xa0000;

pub static STACK_START: u32 = 0x100000;

#[derive(Ord, PartialOrd, PartialEq, Eq, Debug, Clone, Copy)]
pub enum RegisterColor {
    RegColor(Reg),
    // Offset on the stack, in words.
    StackColor(isize),
    // Argument on the stack. The isize is negative; -1 is the last argument.
    StackArgColor(isize),
    // This value is in global storage.
    GlobalColor,
    // This is a structure in global storage; we access it by reference.
    GlobalReferenceColor,
}

allow_string!(RegisterColor);

