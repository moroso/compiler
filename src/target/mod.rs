use package::Package;

pub use self::ir::IRTarget;
pub use self::asm::AsmTarget;
pub use self::util::NameMangler;
pub use self::ccross::CTarget;

use std::io::Write;

mod ccross;
mod ir;
mod asm;
mod util;

pub trait Target {
    fn new(args: Vec<String>) -> Box<Self>;
    fn compile(&self, p: Package, f: &mut Write);
}
