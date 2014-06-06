#include "list.mh"

///////////////// Testing

fn print_int(x: u32) {}
fn print_char(x: u32) {}
fn print_newline() { print_char(10); }

struct nobe {
    n: u32,
    link: list_node
}

fn print_list(head: *list_head) {
    let p: *nobe;
    list_foreach_entry(p, head, nobe, link) {
        print_int(p->n);
    }
    print_newline();
}

fn main() -> i32 {
    let head: list_head;
    let x1: nobe; let x2: nobe; let x3: nobe; let x4: nobe;
    x1.n = 1; x2.n = 2; x3.n = 3; x4.n = 4;

    list_init_head(&head);

    // Build a simple list.
    list_insert_tail(&x1.link, &head);
    list_insert_tail(&x2.link, &head);
    list_insert_tail(&x3.link, &head);
    list_insert_tail(&x4.link, &head);

    print_list(&head);

    // Modify it some.
    list_del(&x3.link);
    list_insert_head(&x3.link, &head);

    print_list(&head);

    let p: *nobe = list_head_entry(&head, nobe, link);
    let p1: *nobe = list_next_entry(&head, p, nobe, link);

    0
}
