use package::Package;

pub use self::ccross::CTarget;

mod ccross;

pub trait Target {
    fn new(args: Vec<StrBuf>) -> Self;
    fn compile(&self, p: Package);
}
