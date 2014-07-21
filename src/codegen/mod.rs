pub use codegen::ir_to_asm::IrToAsm;
use mas::ast::Reg;

pub mod register_color;
pub mod ir_to_asm;

/// How many variables are available to the register allocator.
pub static num_usable_vars: uint = 30;

#[deriving(Ord, PartialOrd, PartialEq, Eq, Show)]
pub enum RegisterColor {
    RegColor(Reg),
    StackColor(uint),
    GlobalColor,
}
