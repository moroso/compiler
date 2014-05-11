fn print_int(x: u32) -> u32 {}

fn f<T>(x: *T) -> *T {
    x
}

fn main() {
    let n: u32 = 5;
    // Note: we want to dereference this, but that requires a cast, and the
    // cross-compiler doesn't yet have the information it needs to do that.
    print_int(f(&n));
}