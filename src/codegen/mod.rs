use mas::ast::Reg;

use std::fmt;
use std::fmt::{Formatter, Display};
pub use codegen::ir_to_asm::IrToAsm;
pub use self::RegisterColor::*;

pub mod register_color;
pub mod ir_to_asm;
pub mod combine;

/// How many variables are available to the register allocator.
pub static num_usable_vars: uint = 30;

// Special registers.
pub static link_register: Reg = Reg { index: 31 };
pub static stack_pointer: Reg = Reg { index: 30 };
pub static first_callee_saved_reg: Reg = Reg { index: 11 };
pub static return_reg: Reg = Reg { index: 0 };
// This register is used for pointers into global storage, or for other things
// that need a temporary register briefly.
pub static global_reg: Reg = Reg { index: 11 };

// How many parameters we can pass in registers.
pub static num_param_regs: uint = 8;

// We use three registers, starting at this index, for spilled registers.
pub static spill_reg_base: u8 = 8;

pub static global_mem_start: u32 = 0x8000;

#[derive(Ord, PartialOrd, PartialEq, Eq, Debug, Copy)]
pub enum RegisterColor {
    RegColor(Reg),
    // Offset on the stack, in words.
    StackColor(int),
    // This value is in global storage.
    GlobalColor,
    // This is a structure in global storage; we access it by reference.
    GlobalReferenceColor,
}

allow_string!(RegisterColor);
