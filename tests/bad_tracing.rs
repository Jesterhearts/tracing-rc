use std::cell::RefCell;

use tracing_rc::rc::{
    collect_full,
    Gc,
    GcVisitor,
    Traceable,
};

#[test]
fn add_value_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: RefCell<Option<Gc<Extra>>>,
        added: RefCell<Option<Gc<Extra>>>,
    }

    impl Traceable for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            // This will be completely leaked. Note that if this happens before visit_children,
            // we'll enter an infinite loop or overflow our stack. This is still safe though, and
            // you really shouldn't be doing this anyways.
            *self.added.borrow_mut() = self.gc.borrow().clone();
        }
    }

    let first = Gc::new(Extra {
        gc: RefCell::new(None),
        added: RefCell::new(None),
    });

    *first.gc.borrow_mut() = Some(first.clone());

    drop(first);

    collect_full();
}

#[test]
fn drop_cyclic_value_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: RefCell<Option<Gc<Extra>>>,
    }

    impl Traceable for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            // There should be > 1 outstanding refs to this value before this point, so it'll get
            // added to the old gen. This shouldn't cause undefined behavior though.
            *self.gc.borrow_mut() = None;
        }
    }

    let first = Gc::new(Extra {
        gc: RefCell::new(None),
    });

    *first.gc.borrow_mut() = Some(first.clone());

    drop(first);

    // This tries to collect the full cycle, but we get an extra instance in the young gen due to a
    // buggy Traceable implementation.
    collect_full();
    // This cleans up the bad value.
    collect_full();
}

#[test]
fn drop_acyclic_value_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: RefCell<Option<Gc<Extra>>>,
        acyclic: RefCell<Option<Gc<usize>>>,
    }

    impl Traceable for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            self.acyclic.visit_children(visitor);
            // There should be > 1 outstanding refs to this value before this point,
            // added to the old gen. This shouldn't cause undefined behavior though.
            // If the collector fails to add strong refs for tracked values, it'll make things blow
            // up.
            *self.acyclic.borrow_mut() = None;
        }
    }

    let first = Gc::new(Extra {
        gc: RefCell::new(None),
        acyclic: RefCell::new(Some(Gc::new(10))),
    });

    *first.gc.borrow_mut() = Some(first.clone());

    drop(first);

    collect_full();
    collect_full();
}

#[test]
fn retain_value_during_trace() {
    thread_local! { static SMUGGLE: RefCell<Option<Gc<Extra>>> = RefCell::new(None) };

    #[derive(Debug)]
    struct Extra {
        gc: RefCell<Option<Gc<Extra>>>,
    }

    impl Traceable for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);

            // This value should be immortal because of this statement.
            SMUGGLE.with(|smuggled| *smuggled.borrow_mut() = self.gc.borrow().clone());
        }
    }

    let first = Gc::new(Extra {
        gc: RefCell::new(None),
    });

    *first.gc.borrow_mut() = Some(first.clone());

    drop(first);

    collect_full();

    let mut smuggled = None;
    SMUGGLE.with(|smuggler| {
        smuggled = smuggler.borrow_mut().take();
    });
    assert!(smuggled.unwrap().gc.borrow().is_some());
}

#[test]
fn report_extra_values_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: RefCell<Option<Gc<Extra>>>,
    }

    impl Traceable for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            // This is clearly a bug, if the collector trusts the traceable trait, it may try to
            // free this value.
            self.gc.visit_children(visitor);
        }
    }

    let first = Gc::new(Extra {
        gc: RefCell::new(None),
    });

    *first.gc.borrow_mut() = Some(first.clone());

    drop(first);

    collect_full();
}

#[test]
fn report_grandchild_values_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: RefCell<Option<Gc<Extra>>>,
    }

    impl Traceable for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);

            // This may be None during the mark phase of the collector, because the grandchild might
            // be temporarily dead. Using the deref trait here would panic (which is safe).
            if let Some(grandchild) = self.gc.borrow().as_ref().unwrap().get() {
                grandchild.gc.borrow().visit_children(visitor)
            };
        }
    }

    let first = Gc::new(Extra {
        gc: RefCell::new(None),
    });

    let second = Gc::new(Extra {
        gc: RefCell::new(Some(first.clone())),
    });

    *first.gc.borrow_mut() = Some(second);

    drop(first);

    collect_full();
}
