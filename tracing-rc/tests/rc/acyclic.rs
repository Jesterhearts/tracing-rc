use tracing_rc::{
    rc::{
        collect_with_options,
        count_roots,
        Gc,
    },
    CollectOptions,
    Trace,
};

#[test]
fn acyclic_single_no_garbage() {
    let a = Gc::new(10);

    drop(a);
    assert_eq!(count_roots(), 0);
}

#[test]
fn acyclic_chain_no_garbage() {
    #[derive(Trace)]
    struct Int {
        i: Gc<u32>,
    }

    let a = Gc::new(Int { i: Gc::new(10) });

    drop(a);
    assert_eq!(
        count_roots(),
        0,
        "Added trivially acyclic node to root list"
    );
}

#[test]
fn acyclic_tree_young_gen_collects() {
    #[derive(Trace)]
    struct Int {
        i: Gc<u32>,
    }

    let a = Gc::new(10u32);
    let b = Gc::new(Int { i: a.clone() });
    let c = Gc::new(Int { i: a.clone() });

    drop(a);

    assert_eq!(*b.borrow().i.borrow(), 10);
    assert_eq!(*c.borrow().i.borrow(), 10);

    assert_eq!(
        count_roots(),
        1,
        "Possibly cyclic node a not added to root list"
    );

    drop(b);

    assert_eq!(count_roots(), 1, "Acyclic node b added to root list");

    drop(c);

    assert_eq!(count_roots(), 1, "Acyclic node c added to root list");

    collect_with_options(CollectOptions::YOUNG_ONLY);

    assert_eq!(count_roots(), 0, "Failed to cleanup node a");
}
