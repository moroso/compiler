fn bar(n: int) -> int {
    let x: int = n;
    let y: int = x-5;
    y
}

fn fib(x: int) -> int {
    if x==0 { 0 }
    else { if x==1 { bar(6) }
           else { fib(x-1) + fib(x-2) } }
}

fn main() -> int {
    print_int(fib(10));
}
