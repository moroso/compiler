fn print_int(x: u32) -> u32 {}

fn print_f_of_int(x: u32, f: fn(u32) -> u32) {
    print_int(f(x));
}

fn bar(n: u32) -> u32 {
    let x: u32 = n;
    let y: u32 = x-5;
    y
}

fn fib(x: u32) -> u32 {
    if x==0 { return 1; 0 }
    else if x==1 { bar(6) }
    else { fib(x-1) + fib(x-2) }
    fib2(x)
}

fn fib2(x: u32) -> u32 {
    x
}

fn deref(x: *u32) -> u32 {
    *x
}

fn main() -> u32 {
    print_int(fib(10));
    print_f_of_int(10, fib);
    let z: u32 = 5;
    print_int(deref(&z));
    while z > 0 {
        print_int(fib(z));
        z = z - 1;
    };
    let i: u32;
    for(i=0; i<7; i=i+1) {
        print_int(fib(i));
    };
    deref(&z)
}
