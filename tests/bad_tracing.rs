use std::cell::RefCell;

use tracing_rc::{
    empty_traceable,
    rc::{
        collect_full,
        Gc,
        GcVisitor,
        Trace,
    },
};

#[test]
fn add_value_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: Option<Gc<Extra>>,
        added: RefCell<Option<Gc<Extra>>>,
    }

    impl Trace for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            *self.added.borrow_mut() = self.gc.clone();
            self.gc.visit_children(visitor);
        }
    }

    let first = Gc::new(Extra {
        gc: None,
        added: RefCell::new(None),
    });

    first.borrow_mut().gc = Some(first.clone());

    drop(first);

    collect_full();
}

#[test]
fn drop_cyclic_value_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: RefCell<Option<Gc<Extra>>>,
    }

    impl Trace for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            // There should be > 1 outstanding refs to this value before this point, but we know
            // it's buffered by the collector, we skip adding it for collection at this point.
            *self.gc.borrow_mut() = None;
        }
    }

    let first = Gc::new(Extra {
        gc: RefCell::new(None),
    });

    first.borrow_mut().gc = RefCell::new(Some(first.clone()));

    drop(first);

    collect_full();
}

#[test]
fn drop_acyclic_value_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: Option<Gc<Extra>>,
        acyclic: RefCell<Option<Gc<usize>>>,
    }

    impl Trace for Extra {
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
        gc: None,
        acyclic: RefCell::new(Some(Gc::new(10))),
    });

    first.borrow_mut().gc = Some(first.clone());

    drop(first);

    collect_full();
    collect_full();
}

#[test]
fn retain_value_during_trace() {
    thread_local! { static SMUGGLE: RefCell<Option<Gc<Extra>>> = RefCell::new(None) };

    #[derive(Debug)]
    struct Extra {
        gc: Option<Gc<Extra>>,
    }

    impl Trace for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);

            // This value should be immortal because of this statement.
            SMUGGLE.with(|smuggled| *smuggled.borrow_mut() = self.gc.clone());
        }
    }

    let first = Gc::new(Extra { gc: None });

    first.borrow_mut().gc = Some(first.clone());

    drop(first);

    collect_full();

    let mut smuggled = None;
    SMUGGLE.with(|smuggler| {
        smuggled = smuggler.borrow_mut().take();
    });
    assert!(smuggled.unwrap().borrow().gc.is_some());
}

#[test]
fn report_extra_values_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: Option<Gc<Extra>>,
    }

    impl Trace for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            // This is clearly a bug, if the collector trusts the traceable trait, it may try to
            // free this value.
            self.gc.visit_children(visitor);
        }
    }

    let first = Gc::new(Extra { gc: None });

    first.borrow_mut().gc = Some(first.clone());

    drop(first);

    collect_full();
}

#[test]
fn report_grandchild_values_during_trace() {
    #[derive(Debug)]
    struct Extra {
        gc: Option<Gc<Extra>>,
    }

    impl Trace for Extra {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);

            // This may be None during the mark phase of the collector, because the grandchild might
            // be temporarily dead. Using the deref trait here would panic (which is safe).
            if let Some(grandchild) = self.gc.as_ref().unwrap().try_borrow() {
                grandchild.gc.visit_children(visitor)
            };
        }
    }

    let first = Gc::new(Extra { gc: None });

    let second = Gc::new(Extra {
        gc: Some(first.clone()),
    });

    first.borrow_mut().gc = Some(second);

    drop(first);

    collect_full();
}

#[test]
fn overreport_children_open_child_borrow() {
    #[derive(Debug)]
    struct V {
        v: Vec<usize>,
    }
    empty_traceable!(V);

    #[derive(Debug)]
    struct Lies {
        gc: Gc<V>,
        cycle: Option<Gc<Lies>>,
    }

    impl Trace for Lies {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            self.gc.visit_children(visitor);

            self.cycle.visit_children(visitor);
        }
    }

    let child = Gc::new(V { v: vec![10] });

    {
        let parent = Gc::new(Lies {
            gc: child.clone(),
            cycle: None,
        });
        parent.borrow_mut().cycle = Some(parent.clone());

        drop(parent);

        let child_ref = child.borrow();
        let child_borrow: &Vec<usize> = &child_ref.v;

        // If we don't prevent drops while data is borrowed, this will cause ub
        collect_full();

        assert_eq!(child_borrow[0], 10);
    }
    drop(child);
    collect_full();
}
