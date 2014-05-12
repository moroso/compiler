fn print_int(x: u32) -> u32 {}

enum TestEnum {
    Foo,
    Bar(u32),
    Baz,
    Quux(u32, *u32),
}

fn main() {
    let x: TestEnum = Foo;
    let y: TestEnum = Bar(5);
    let i: u32 = 123;
    let z: TestEnum = Quux(4+5, &i);

    match x {
        Foo => print_int(0),
        Bar(fieldx) => print_int(fieldx),
        Quux(fieldx, fieldy) => print_int(fieldx),
    };

    match y {
        Foo => print_int(0),
        Bar(fieldx) => print_int(fieldx),
        Quux(fieldx, fieldy) => print_int(fieldx),
    };

    match z {
        Foo => print_int(0),
        Bar(fieldx) => print_int(fieldx),
        Quux(fieldx, fieldy) => { print_int(fieldx); print_int(*fieldy); },
    };

    print_int(match z {
        Foo => 0,
        Bar(fieldx) => fieldx,
        Quux(fieldx, fieldy) => *fieldy,
    });

}
