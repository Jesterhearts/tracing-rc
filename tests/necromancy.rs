use std::cell::RefCell;

use tracing_rc::rc::{
    collect_full,
    Gc,
    GcVisitor,
    Traceable,
};

#[test]
fn pure_gc_necromancy() {
    struct Zombie {
        cycle: Option<Gc<Zombie>>,
        dead: Option<Gc<Mancer>>,
    }

    impl Traceable for Zombie {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.cycle.visit_children(visitor);
            self.dead.visit_children(visitor);
        }
    }

    thread_local! { static ZOMBIE: RefCell<Option<Gc<Zombie>>> = RefCell::new(None) };

    #[derive(Debug)]
    struct Necro {
        gc: Option<Gc<Mancer>>,
    }

    impl Drop for Necro {
        fn drop(&mut self) {
            ZOMBIE.with(|zombie| {
                *zombie.borrow_mut() = Some(Gc::new(Zombie {
                    cycle: None,
                    dead: Some(self.gc.as_ref().unwrap().clone()),
                }));
            })
        }
    }

    impl Traceable for Necro {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            visitor.visit_node(self.gc.as_ref().unwrap());
        }
    }

    #[derive(Debug)]
    struct Mancer {
        gc: Gc<Necro>,
    }

    impl Traceable for Mancer {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            visitor.visit_node(&self.gc);
        }
    }

    let necro = Gc::new(Necro { gc: None });
    let mancer = Gc::new(Mancer { gc: necro.clone() });
    necro.borrow_mut().gc = Some(mancer);

    drop(necro);

    collect_full();
    let mut resurrected_owner = None;
    ZOMBIE.with(|zombie| {
        resurrected_owner = zombie.borrow().clone();
        *zombie.borrow_mut() = None;
    });

    resurrected_owner.as_ref().unwrap().borrow_mut().cycle = resurrected_owner.clone();

    drop(resurrected_owner);

    collect_full();
}
