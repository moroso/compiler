enum TestEnum {
    Foo,
    Bar(int),
    Baz,
    Quux(int, *int),
}

fn main() {
    let x: TestEnum = Foo;
    let y: TestEnum = Bar(5);
    let i: int = 5;
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
}