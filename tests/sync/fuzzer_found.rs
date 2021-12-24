use std::sync::Mutex;

use tracing_rc::sync::{
    collect_full,
    Agc,
    GcVisitor,
    Trace,
};

#[derive(Default)]
struct Cycle {
    neighbors: Mutex<Vec<Agc<Cycle>>>,
}

impl Trace for Cycle {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        self.neighbors.visit_children(visitor);
    }
}

#[test]
fn fuzzfound_leak_01() {
    // 0
    let a = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });
    // 1
    let b = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });
    // 8
    let c = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });
    // 13
    let d = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });

    // 0 -> 0
    a.read().neighbors.lock().unwrap().push(a.clone());
    // 0 -> 13
    a.read().neighbors.lock().unwrap().push(d.clone());
    // 1 -> 8
    b.read().neighbors.lock().unwrap().push(c.clone());
    // 8 -> 0
    c.read().neighbors.lock().unwrap().push(a.clone());
    // 13 -> 13
    d.read().neighbors.lock().unwrap().push(d.clone());
    // 13 -> 0
    d.read().neighbors.lock().unwrap().push(a.clone());

    let b_stack = b.clone();
    let b_borrow = b_stack.read();

    let c_stack = c.clone();
    let c_borrow = c_stack.read();

    let d_stack = d.clone();
    let d_borrow = d_stack.read();

    drop(a);
    drop(b);
    drop(c);
    drop(d);

    // 1. a, b, c, d all put up for collection
    // 2. All marked live. a only reachable from c, d.
    collect_full();

    drop(b_borrow);
    drop(c_borrow);
    drop(d_borrow);

    drop(b_stack);
    drop(c_stack);
    drop(d_stack);

    // 1. b, c, d all put up for collection.
    // 2. b is dropped, drops its ref to c, c knows it is buffered and does nothing.
    // 3. c is dropped, drops its ref to a, a is _not_ buffered, so we try to buffer it in the young
    // gen, we'll panic if we have the cache borrowed, if we instead are dropping from a buffer,
    // it'll go in young gen.
    // 4. d goes into old gen.
    // 5. We trace d and find it is live (a references it, and is held by the young gen and we don't
    // know to count the young gen reference). If we unconditionally mark live, we'll
    // filter out a from the generations and leave it unreachable.
    collect_full();
}

#[test]
fn fuzzfound_leak_02() {
    // Very similar to fuzzfound_leak_01, just a more complex way of convincing the GC that a node
    // is still alive by interleaving checking the buffer status of the node with gc cycles. If
    // we unconditionally mark the node as unbuffered post-collection, we can end up leaking an
    // extra strong count in the destructor when we replace an already buffered young-gen node.

    // 0
    let a = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });
    // 1
    let b = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });
    // 4
    let c = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });
    // 8
    let d = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });
    // 13
    let e = Agc::new(Cycle {
        neighbors: Mutex::default(),
    });

    // 0 -> 0
    a.read().neighbors.lock().unwrap().push(a.clone());
    // 1 -> 8
    b.read().neighbors.lock().unwrap().push(d.clone());
    // 4 -> 13
    c.read().neighbors.lock().unwrap().push(e.clone());
    // 8 -> 0
    d.read().neighbors.lock().unwrap().push(a.clone());
    // 13 -> 0
    // 13 -> 13
    e.read().neighbors.lock().unwrap().push(a.clone());
    e.read().neighbors.lock().unwrap().push(e.clone());

    let b_stack = b.clone();
    let b_borrow = b_stack.read();

    let d_stack = d.clone();
    let d_borrow = d_stack.read();

    let e_stack = e.clone();
    let e_borrow = e_stack.read();

    drop(a);
    drop(b);
    drop(c);
    drop(d);
    drop(e);

    collect_full();

    drop(b_borrow);
    drop(d_borrow);
    drop(e_borrow);

    drop(b_stack);
    drop(d_stack);
    drop(e_stack);

    collect_full();
}
