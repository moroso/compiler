fn print_int(x: u32) {}

static a = 5;
static b = 6;

fn main() {
    print_int(a+b);
    b = 1;
    print_int(a+b);
    a = b;
    print_int(a+b);
}