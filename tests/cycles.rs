use std::cell::RefCell;

use tracing_rc::{
    collect_full,
    collector::count_roots,
    GcPtr,
    GcVisitor,
    Traceable,
};

#[test]
fn mono_simple_cycle() {
    struct Cycle {
        gc: Option<GcPtr<RefCell<Cycle>>>,
    }

    impl Traceable for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor)
        }
    }

    let a = GcPtr::new(RefCell::new(Cycle { gc: None }));
    a.borrow_mut().gc = Some(a.clone());

    assert_eq!(
        GcPtr::strong_count(&a),
        2,
        "Node a strong count not incremented"
    );

    drop(a);

    assert_eq!(count_roots(), 1, "Possibly cyclic node a not in roots");

    collect_full();

    assert_eq!(count_roots(), 0);
}

#[test]
fn simple_cycle() {
    struct Cycle {
        gc: Option<GcPtr<RefCell<Cycle>>>,
    }

    impl Traceable for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor)
        }
    }

    // A.ptr -> Null
    // A ref 1
    let a = GcPtr::new(RefCell::new(Cycle { gc: None }));

    // B.ptr -> A
    // B ref 1, A ref 2
    let b = GcPtr::new(RefCell::new(Cycle {
        gc: Some(a.clone()),
    }));

    // A.ptr -> B
    // B ref 2, A ref 2
    a.borrow_mut().gc = Some(b.clone());

    assert_eq!(
        GcPtr::strong_count(&a),
        2,
        "Node a strong count not incremented"
    );

    assert_eq!(
        GcPtr::strong_count(&b),
        2,
        "Node b strong count not incremented"
    );

    drop(b);

    assert_eq!(count_roots(), 1, "Node b not tracked in roots");

    assert_eq!(
        GcPtr::strong_count(&a),
        2,
        "Node a strong count decremented by drop of child"
    );

    assert_eq!(
        GcPtr::strong_count(a.borrow().gc.as_ref().unwrap()),
        2,
        "Node b strong count not properly handled after drop"
    );

    drop(a);

    assert_eq!(count_roots(), 2, "Node a not tracked in roots");

    collect_full();

    assert_eq!(
        count_roots(),
        0,
        "Cycle not cleaned up and removed from list"
    );
}

#[test]
fn dead_cycle_live_outbound() {
    struct Lives;

    impl Lives {
        #[inline(never)]
        fn alive(&self) -> bool {
            true
        }
    }

    impl Traceable for Lives {
        fn visit_children(&self, _: &mut GcVisitor) {}
    }

    struct Cycle {
        gc: Option<GcPtr<RefCell<Cycle>>>,
        edge: GcPtr<Lives>,
    }

    impl Traceable for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            self.edge.visit_children(visitor);
        }
    }

    let lives = GcPtr::new(Lives);

    // A.ptr -> Null
    // A ref 1, Lives ref 2
    let a = GcPtr::new(RefCell::new(Cycle {
        gc: None,
        edge: lives.clone(),
    }));

    // B.ptr -> A
    // B ref 1, A ref 2, Lives ref 3
    let b = GcPtr::new(RefCell::new(Cycle {
        gc: Some(a.clone()),
        edge: lives.clone(),
    }));

    // A.ptr -> B
    // B ref 2, A ref 2, Lives ref 3
    a.borrow_mut().gc = Some(b.clone());

    assert_eq!(
        GcPtr::strong_count(&lives),
        3,
        "Live node missed a refcount"
    );

    drop(b);

    // B hasn't been cleand up yet, so we should still have its strong refs to 3
    assert_eq!(
        GcPtr::strong_count(&lives),
        3,
        "Drop of cyclical parent b node decremented live refcount"
    );

    assert_eq!(count_roots(), 1, "Node b not in list of roots");

    drop(a);

    // A hasn't been cleand up yet, so we should still have its strong refs to 3
    assert_eq!(
        GcPtr::strong_count(&lives),
        3,
        "Drop of cyclical parent a node decremented live refcount"
    );

    assert_eq!(count_roots(), 2, "Node a not in list of roots");

    // Collect a & b, this will re-add live for possibly collection.
    collect_full();
    // Make sure we clean up the new & old gens for the last live reference.
    // This shouldn't drop it, just remove it from the list of possible cycles.
    collect_full();

    assert_eq!(count_roots(), 0, "Not all roots were collected");

    assert!(lives.alive());
}

#[test]
fn dead_cycle_dead_outbound() {
    struct Dead;

    impl Traceable for Dead {
        fn visit_children(&self, _: &mut GcVisitor) {}
    }

    struct Cycle {
        gc: Option<GcPtr<RefCell<Cycle>>>,
        edge: GcPtr<Dead>,
    }

    impl Traceable for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            visitor.visit_node(&self.edge)
        }
    }

    let dies = GcPtr::new(Dead);

    let a = GcPtr::new(RefCell::new(Cycle {
        gc: None,
        edge: dies.clone(),
    }));

    let b = GcPtr::new(RefCell::new(Cycle {
        gc: Some(a.clone()),
        edge: dies,
    }));

    a.borrow_mut().gc = Some(b.clone());

    drop(b);

    assert_eq!(count_roots(), 1, "Node b not in list of roots");

    drop(a);

    assert_eq!(count_roots(), 2, "Node a not in list of roots");

    collect_full();

    assert_eq!(count_roots(), 0, "Not all roots were collected");
}

#[test]
fn cycle_live_inbound() {
    struct Cycle {
        gc: Option<GcPtr<RefCell<Cycle>>>,
    }

    impl Traceable for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
        }
    }

    let a = GcPtr::new(RefCell::new(Cycle { gc: None }));

    let b = GcPtr::new(RefCell::new(Cycle {
        gc: Some(a.clone()),
    }));

    a.borrow_mut().gc = Some(b.clone());

    drop(b);

    assert_eq!(count_roots(), 1, "Node b not tracked in roots");

    // We don't expect anything to be cleaned up yet, since A is still live
    collect_full();
    assert_eq!(
        count_roots(),
        0,
        "Live node b not removed from list of roots"
    );

    // This is trivially true, but it makes it so that miri will yell if we're
    // touching anything dead.
    assert!(a.borrow().gc.as_ref().unwrap().borrow().gc.is_some());
    drop(a);

    assert_eq!(count_roots(), 1, "Node a not tracked in roots");

    collect_full();
    collect_full();

    assert_eq!(
        count_roots(),
        0,
        "Cycle not cleaned up and removed from list"
    );
}

#[test]
fn mono_cycle_live_inbound() {
    struct Cycle {
        gc: Option<GcPtr<RefCell<Cycle>>>,
    }

    impl Traceable for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
        }
    }

    let a = GcPtr::new(RefCell::new(Cycle { gc: None }));
    a.borrow_mut().gc = Some(a.clone());

    let last = a.clone();

    drop(a);

    assert_eq!(count_roots(), 1, "Node a not tracked in roots");

    // We don't expect anything to be cleaned up yet, since last is still live
    collect_full();
    assert_eq!(
        count_roots(),
        0,
        "Live node a not removed from list of roots"
    );

    // This is trivially true, but it makes it so that miri will yell if we're
    // touching anything dead.
    assert!(last.borrow().gc.is_some());
    drop(last);

    assert_eq!(count_roots(), 1, "Node last not tracked in roots");

    collect_full();

    assert_eq!(
        count_roots(),
        0,
        "Cycle not cleaned up and removed from list"
    );
}

#[test]
fn mono_cycle_live_outbound() {
    struct Cycle {
        gc: Option<GcPtr<RefCell<Cycle>>>,
        edge: GcPtr<u32>,
    }

    impl Traceable for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            visitor.visit_node(&self.edge);
        }
    }

    let live = GcPtr::new(10);

    let a = GcPtr::new(RefCell::new(Cycle {
        gc: None,
        edge: live.clone(),
    }));
    a.borrow_mut().gc = Some(a.clone());

    let last = a.clone();

    drop(a);

    assert_eq!(count_roots(), 1, "Node a not tracked in roots");

    // We don't expect anything to be cleaned up yet, since last is still live
    collect_full();
    assert_eq!(
        count_roots(),
        0,
        "Live node a not removed from list of roots"
    );

    assert_eq!(*live, 10);
    drop(last);

    assert_eq!(count_roots(), 1, "Node last not tracked in roots");

    collect_full();
    collect_full();

    assert_eq!(
        count_roots(),
        0,
        "Cycle not cleaned up and removed from list"
    );

    // We should still have access to live
    assert_eq!(*live, 10);
}
