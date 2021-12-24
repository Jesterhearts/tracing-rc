use std::{
    cell::RefCell,
    sync::Mutex,
};

use tracing_rc::sync::{
    collect_full,
    Agc,
    GcVisitor,
    Trace,
};

#[test]
fn pure_gc_necromancy() {
    struct Zombie {
        cycle: Mutex<Option<Agc<Zombie>>>,
        dead: Mutex<Option<Agc<Mancer>>>,
    }

    impl Trace for Zombie {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.cycle.visit_children(visitor);
            self.dead.visit_children(visitor);
        }
    }

    thread_local! { static ZOMBIE: RefCell<Option<Agc<Zombie>>> = RefCell::new(None) };

    #[derive(Debug)]
    struct Necro {
        gc: Mutex<Option<Agc<Mancer>>>,
    }

    impl Drop for Necro {
        fn drop(&mut self) {
            ZOMBIE.with(|zombie| {
                *zombie.borrow_mut() = Some(Agc::new(Zombie {
                    cycle: Mutex::new(None),
                    dead: Mutex::new(Some(self.gc.lock().unwrap().as_ref().unwrap().clone())),
                }));
            })
        }
    }

    impl Trace for Necro {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
        }
    }

    #[derive(Debug)]
    struct Mancer {
        gc: Agc<Necro>,
    }

    impl Trace for Mancer {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.gc.visit_children(visitor);
        }
    }

    let necro = Agc::new(Necro {
        gc: Mutex::new(None),
    });
    let mancer = Agc::new(Mancer { gc: necro.clone() });
    *necro.read().gc.lock().unwrap() = Some(mancer);

    drop(necro);

    collect_full();
    let mut resurrected_owner = None;
    ZOMBIE.with(|zombie| {
        resurrected_owner = zombie.borrow().clone();
        *zombie.borrow_mut() = None;
    });

    *resurrected_owner
        .as_ref()
        .unwrap()
        .read()
        .cycle
        .lock()
        .unwrap() = resurrected_owner.clone();

    drop(resurrected_owner);

    collect_full();
}
