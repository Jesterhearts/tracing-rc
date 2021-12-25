use std::sync::Mutex;

use tracing_rc::sync::{
    collect_full,
    count_roots,
    Agc,
    GcVisitor,
    Trace,
};

#[test]
fn mono_simple_cycle() {
    struct Cycle {
        gc: Mutex<Option<Agc<Cycle>>>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor)
        }
    }

    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
    });
    *a.read().gc.lock().unwrap() = Some(a.clone());

    drop(a);

    collect_full();

    assert_eq!(count_roots(), 0);
}

#[test]
fn simple_cycle() {
    struct Cycle {
        gc: Mutex<Option<Agc<Cycle>>>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor)
        }
    }

    // A.ptr -> Null
    // A ref 1
    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
    });

    // B.ptr -> A
    // B ref 1, A ref 2
    let b = Agc::new(Cycle {
        gc: Mutex::new(Some(a.clone())),
    });

    // A.ptr -> B
    // B ref 2, A ref 2
    *a.read().gc.lock().unwrap() = Some(b.clone());

    drop(b);

    assert_eq!(count_roots(), 1, "Node b not tracked in roots");

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
fn parallel_edges() {
    // Improper edge counting could cause a graph with parallel edges to leak nodes.
    struct Cycle {
        gc: Mutex<Option<Agc<Cycle>>>,
        gc2: Mutex<Option<Agc<Cycle>>>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            self.gc2.visit_children(visitor);
        }
    }

    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
        gc2: Mutex::new(None),
    });

    let b = Agc::new(Cycle {
        gc: Mutex::new(Some(a.clone())),
        gc2: Mutex::new(Some(a.clone())),
    });

    *a.read().gc.lock().unwrap() = Some(b.clone());
    *a.read().gc2.lock().unwrap() = Some(b.clone());

    drop(b);
    drop(a);

    collect_full();

    assert_eq!(
        count_roots(),
        0,
        "Cycle not cleaned up and removed from list"
    );
}

#[test]
fn live_grandchildren() {
    // Incorrect filtering of the dead candidate list can leave grand-child nodes dead. It won't
    // cause undefined behavior, but it's still bad.
    struct Cycle {
        gc: Mutex<Option<Agc<Cycle>>>,
        data: usize,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
        }
    }

    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
        data: 1,
    });

    let b = Agc::new(Cycle {
        gc: Mutex::new(Some(a.clone())),
        data: 2,
    });

    let c = Agc::new(Cycle {
        gc: Mutex::new(Some(b.clone())),
        data: 3,
    });

    *a.read().gc.lock().unwrap() = Some(c.clone());

    // A <- B <- C <-+ <- Stack
    // L_____________|
    drop(b);
    drop(a);

    collect_full();

    // A and B should be labeled as possibly dead, and C as live.
    // If we fail to visit all of C's possibly dead decendencents recursively, we'll fail to re-mark
    // a as live, and it'll become a zombie node, so we won't be able to access its data.
    let data = c
        // C -> B
        .read()
        .gc
        .lock()
        .unwrap()
        .as_ref()
        .unwrap()
        .read()
        // B -> A
        .gc
        .lock()
        .unwrap()
        .as_ref()
        .unwrap()
        .read()
        // A.data
        .data;

    assert_eq!(data, 1);

    // Make it so miri doesn't complain about nodes being leaked.
    drop(c);
    collect_full();
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

    impl Trace for Lives {
        fn visit_children(&self, _: &mut GcVisitor) {}
    }

    struct Cycle {
        gc: Mutex<Option<Agc<Cycle>>>,
        edge: Agc<Lives>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            self.edge.visit_children(visitor);
        }
    }

    let lives = Agc::new(Lives);

    // A.ptr -> Null
    // A ref 1, Lives ref 2
    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
        edge: lives.clone(),
    });

    // B.ptr -> A
    // B ref 1, A ref 2, Lives ref 3
    let b = Agc::new(Cycle {
        gc: Mutex::new(Some(a.clone())),
        edge: lives.clone(),
    });

    // A.ptr -> B
    // B ref 2, A ref 2, Lives ref 3
    *a.read().gc.lock().unwrap() = Some(b.clone());

    assert_eq!(Agc::strong_count(&lives), 3, "Live node missed a refcount");

    drop(b);

    // B hasn't been cleand up yet, so we should still have its strong refs to 3
    assert_eq!(
        Agc::strong_count(&lives),
        3,
        "Drop of cyclical parent b node decremented live refcount"
    );

    assert_eq!(count_roots(), 1, "Node b not in list of roots");

    drop(a);

    // A hasn't been cleand up yet, so we should still have its strong refs to 3
    assert_eq!(
        Agc::strong_count(&lives),
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

    assert!(lives.read().alive());
}

#[test]
fn dead_cycle_dead_outbound() {
    struct Dead;

    impl Trace for Dead {
        fn visit_children(&self, _: &mut GcVisitor) {}
    }

    struct Cycle {
        gc: Mutex<Option<Agc<Cycle>>>,
        edge: Agc<Dead>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            visitor.visit_node(&self.edge)
        }
    }

    let dies = Agc::new(Dead);

    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
        edge: dies.clone(),
    });

    let b = Agc::new(Cycle {
        gc: Mutex::new(Some(a.clone())),
        edge: dies,
    });

    *a.read().gc.lock().unwrap() = Some(b.clone());

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
        gc: Mutex<Option<Agc<Cycle>>>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
        }
    }

    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
    });

    let b = Agc::new(Cycle {
        gc: Mutex::new(Some(a.clone())),
    });

    *a.read().gc.lock().unwrap() = Some(b.clone());

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
    assert!(a
        .read()
        .gc
        .lock()
        .unwrap()
        .as_ref()
        .unwrap()
        .read()
        .gc
        .lock()
        .unwrap()
        .is_some());
    drop(a);

    assert_eq!(count_roots(), 1, "Node a not tracked in roots");

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
        gc: Mutex<Option<Agc<Cycle>>>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
        }
    }

    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
    });
    *a.read().gc.lock().unwrap() = Some(a.clone());

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
    assert!(last.read().gc.lock().unwrap().is_some());
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
        gc: Mutex<Option<Agc<Cycle>>>,
        edge: Agc<u32>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
            visitor.visit_node(&self.edge);
        }
    }

    let live = Agc::new(10);

    let a = Agc::new(Cycle {
        gc: Mutex::new(None),
        edge: live.clone(),
    });
    *a.read().gc.lock().unwrap() = Some(a.clone());

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

    assert_eq!(*live.read(), 10);
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
    assert_eq!(*live.read(), 10);
}
