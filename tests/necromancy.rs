use std::cell::RefCell;

use tracing_rc::{
    collect_full,
    GcPtr,
    GcVisitor,
    Traceable,
};

#[test]
fn pure_gc_necromancy() {
    struct Zombie {
        cycle: RefCell<Option<GcPtr<Zombie>>>,
        dead: RefCell<Option<GcPtr<Mancer>>>,
    }

    impl Traceable for Zombie {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.cycle.visit_children(visitor);
            self.dead.visit_children(visitor);
        }
    }

    thread_local! { static ZOMBIE: RefCell<Option<GcPtr<Zombie>>> = RefCell::new(None) };

    #[derive(Debug)]
    struct Necro {
        gc: RefCell<Option<GcPtr<Mancer>>>,
    }

    impl Drop for Necro {
        fn drop(&mut self) {
            ZOMBIE.with(|zombie| {
                *zombie.borrow_mut() = Some(GcPtr::new(Zombie {
                    cycle: RefCell::new(None),
                    dead: RefCell::new(Some(self.gc.borrow().as_ref().unwrap().clone())),
                }));
            })
        }
    }

    impl Traceable for Necro {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            visitor.visit_node(self.gc.borrow().as_ref().unwrap());
        }
    }

    #[derive(Debug)]
    struct Mancer {
        gc: GcPtr<Necro>,
    }

    impl Traceable for Mancer {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            visitor.visit_node(&self.gc);
        }
    }

    let necro = GcPtr::new(Necro {
        gc: RefCell::new(None),
    });
    let mancer = GcPtr::new(Mancer { gc: necro.clone() });
    *necro.gc.borrow_mut() = Some(mancer);

    drop(necro);

    collect_full();
    let mut resurrected_owner = None;
    ZOMBIE.with(|zombie| {
        resurrected_owner = zombie.borrow().clone();
        *zombie.borrow_mut() = None;
    });

    *resurrected_owner.as_ref().unwrap().cycle.borrow_mut() = resurrected_owner.clone();

    drop(resurrected_owner);

    collect_full();
}
