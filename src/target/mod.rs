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
mod debug_info;

pub trait Target {
    fn compile(&self, p: Package, f: &mut Write);
}
pub trait MkTarget: Target {
    fn new<'a>(args: &Vec<(String, Option<String>)>) -> Box<Self>;
}
