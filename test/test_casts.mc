fn print_int(n: u32) {}


fn main() {
    let n: u8 = 32u32 as u8;
    let n2: *u8 = 8 as *u8;
    print_int(n as u32);
    print_int(n2 as u32);
}