use std::io::Writer;
use package::Package;

pub use self::ccross::CTarget;
pub use self::ir::IRTarget;
pub use self::asm::AsmTarget;


mod ccross;
mod ir;
mod asm;

pub trait Target {
    fn new(args: Vec<String>) -> Self;
    fn compile(&self, p: Package, f: &mut Writer);
}
