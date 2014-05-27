use package::Package;

pub use self::ccross::CTarget;

mod ccross;

pub trait Target {
    fn new(args: Vec<String>) -> Self;
    fn compile(&self, p: Package);
}
