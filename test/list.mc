// Modeled after Linux's list.h
// Needs the C preprocessor. container_of is pretty necessary for
// the whole scheme. The other macros are just convenient.

// I guess this will need to be split out into a header and an implementation.
// Or if we grow function inlining, the functions should be 'static inline'.

// Generic defines
#define null 0

#define offset_of(st, m) ((&(0 as *st)->m) as u32)
#define container_of(ptr, ty, member) \
    (((ptr as u32) - offset_of(ty, member)) as *ty)

// Defines for the linked list library

#define list_entry container_of

#define list_head list_node
#define list_insert_head list_insert_after
#define list_insert_tail list_insert_before

// These functions should only be used if the results are known to be
// well defined.
#define list_head_entry_(head, typ, link) \
    container_of(head->next, typ, link)
#define list_tail_entry_(head, typ, link) \
    container_of(head->prev, typ, link)

#define list_next_entry_(node, typ, link) \
    container_of(node->link.next, typ, link)
#define list_prev_entry_(node, typ, link) \
    container_of(node->link.prev, typ, link)

// Versions that return NULL when needed
#define list_head_entry(head, typ, link) \
    (list_is_empty(head) ? null : list_head_entry_(head, typ, link))
#define list_tail_entry(head, typ, link) \
    (list_is_empty(head) ? null : list_tail_entry_(head, typ, link))

#define list_next_entry(head, node, typ, link) \
    ((node)->link.next == (head) ? null : list_next_entry_(node, typ, link))
#define list_prev_entry(head, node, typ, link) \
    ((node)->link.prev == (head) ? null : list_prev_entry_(node, typ, link))



// If we had typeof we could drop the typ argument.
#define list_foreach_entry(cur, head, typ, link) \
    for (cur = list_head_entry_(head, typ, link); \
         &cur->link != (head); \
         cur = list_next_entry_(cur, typ, link))

// TODO: need static init macros once available


struct list_node {
    next: *list_node,
    prev: *list_node
}


fn list_init_head(head: *list_head) {
    head->next = head->prev = head;
}
fn list_init_node(head: *list_head) {
    head->next = head->prev = null;
}

fn __list_insert_between(n: *list_node, n1: *list_node, n2: *list_node) {
    n->prev = n1;
    n->next = n2;
    n1->next = n;
    n2->prev = n;
}

fn list_insert_after(n: *list_node, n1: *list_node) {
    list_insert_between(n, n1, n1->next);
}
fn list_insert_before(n: *list_node, n1: *list_node) {
    list_insert_between(n, n1->prev, n1);
}

fn list_del(n: *list_node) {
    n->next->prev = n->prev;
    n->prev->next = n->next;
    // To help catch bugs.
    n->next = n->prev = null;
}

fn list_is_empty(n: *list_head) -> bool {
    n->next == n
}
fn list_is_singleton(n: *list_head) -> bool {
    n->next == n->prev && n != n->next
}



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


    0
}
