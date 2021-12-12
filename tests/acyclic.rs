use tracing_rc::{
    rc::{
        collect_with_options,
        collector::count_roots,
        Gc,
        GcVisitor,
        Traceable,
    },
    CollectOptions,
};

#[test]
fn acyclic_single_no_garbage() {
    let a = Gc::new(10);

    drop(a);
    assert_eq!(count_roots(), 0);
}

#[test]
fn acyclic_chain_no_garbage() {
    struct Int {
        i: Gc<u32>,
    }
    impl Traceable for Int {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            visitor.visit_node(&self.i);
        }
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
    struct Int {
        i: Gc<u32>,
    }
    impl Traceable for Int {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            visitor.visit_node(&self.i)
        }
    }

    let a = Gc::new(10u32);
    let b = Gc::new(Int { i: a.clone() });
    let c = Gc::new(Int { i: a.clone() });

    drop(a);

    assert_eq!(*b.i, 10);
    assert_eq!(*c.i, 10);

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
