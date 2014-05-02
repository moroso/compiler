fn print_f_of_int(x: int, f: fn(int) -> int) {
    print_int(f(x));
}

fn bar(n: int) -> int {
    let x: int = n;
    let y: int = x-5;
    y
}

fn fib(x: int) -> int {
    if x==0 { return 1; 0 }
    else if x==1 { bar(6) }
    else { fib(x-1) + fib(x-2) }
}

fn deref(x: *int) -> int {
    *x
}

fn main() -> int {
    print_int(fib(10));
    print_f_of_int(10, fib);
    let z: int = 5;
    print_int(deref(&z));
    while z > 0 {
        print_int(fib(z));
        z = z - 1;
    };
    let i: int;
    for(i=0; i<7; i=i+1) {
        print_int(fib(i));
    };
    deref(&z)
}
