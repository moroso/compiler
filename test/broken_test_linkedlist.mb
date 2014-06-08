fn print_int(i: i32) {}
fn malloc<T>(i: u32) -> *T { 0 as *T }

struct ll {
    data: i32,
    next: *ll,
}

fn ll_empty() -> *ll {
    0 as *ll
}

fn ll_prepend(data: i32, list: *ll) -> *ll {
    let _temp_ll: *ll = 0 as *ll;
    let result: *ll = malloc(4) as *ll;
    result->data = data;
    result->next = list;
    result
}

fn main() -> i32 {
    let list: *ll = ll_empty();
    list = ll_prepend(5, list);
    list = ll_prepend(6, list);
    list = ll_prepend(7, list);

    let iterlist: *ll = 0 as *ll;
    for (iterlist = list; iterlist != (0 as *ll); iterlist = iterlist->next) {
        print_int(iterlist->data);
    };
    0
}
