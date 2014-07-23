pub use codegen::ir_to_asm::IrToAsm;
use mas::ast::Reg;

pub mod register_color;
pub mod ir_to_asm;

/// How many variables are available to the register allocator.
pub static num_usable_vars: uint = 30;

// Special registers.
pub static link_register: Reg = Reg { index: 31 };
pub static stack_pointer: Reg = Reg { index: 30 };
pub static first_callee_saved_reg: Reg = Reg { index: 11 };
pub static return_reg: Reg = Reg { index: 0 };

// How many parameters we can pass in registers.
pub static num_param_regs: uint = 8;

// We use three registers, starting at this index, for spilled registers.
pub static spill_reg_base: u8 = 8;

#[deriving(Ord, PartialOrd, PartialEq, Eq, Show)]
pub enum RegisterColor {
    RegColor(Reg),
    StackColor(int),
    GlobalColor,
}
