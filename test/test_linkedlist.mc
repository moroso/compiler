fn print_int(i: i32) {}

struct ll {
    data: i32,
    next: *ll,
}

fn ll_empty() -> *ll {
    NULL
}

fn ll_prepend(data: i32, list: *ll) -> *ll {
    let result: *ll = malloc(sizeof(ll));
    result->data = data;
    result->next = list;
    result
}

fn main() -> i32 {
    let list: *ll = ll_empty();
    list = ll_prepend(5, list);
    list = ll_prepend(6, list);
    list = ll_prepend(7, list);

    let iterlist: *ll;
    for (iterlist = list; iterlist != NULL; iterlist = iterlist->next) {
        print_int(iterlist->data);
    };
}
