use tracing_rc::{
    collect_with_options,
    collector::count_roots,
    CollectionType,
    GcPtr,
    GcVisitor,
    Traceable,
};

#[test]
fn acyclic_single_no_garbage() {
    let a = GcPtr::new(10);

    drop(a);
    assert_eq!(count_roots(), 0);
}

#[test]
fn acyclic_chain_no_garbage() {
    struct Int {
        i: GcPtr<u32>,
    }
    impl Traceable for Int {
        unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
            visitor(self.i.node())
        }
    }

    let a = GcPtr::new(Int { i: GcPtr::new(10) });

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
        i: GcPtr<u32>,
    }
    impl Traceable for Int {
        unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
            visitor(self.i.node())
        }
    }

    let a = GcPtr::new(10u32);
    let b = GcPtr::new(Int { i: a.clone() });
    let c = GcPtr::new(Int { i: a.clone() });

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

    collect_with_options(CollectionType::YoungOnly);

    assert_eq!(count_roots(), 0, "Failed to cleanup node a");
}
