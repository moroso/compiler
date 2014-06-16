use package::Package;

pub use self::ccross::CTarget;
pub use self::ir::IRTarget;

mod ccross;
mod ir;

pub trait Target {
    fn new(args: Vec<String>) -> Self;
    fn compile(&self, p: Package);
}
