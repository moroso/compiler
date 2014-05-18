fn print_int(x: u32) {}

fn f<T>(x: *T) -> *T {
    x
}

fn main() {
    let n: u32 = 5;
    print_int(*f(&n));
}
