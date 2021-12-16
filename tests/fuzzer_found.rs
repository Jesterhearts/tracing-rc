use tracing_rc::rc::{
    collect_full,
    Gc,
    GcVisitor,
    Trace,
};

#[test]
fn fuzzfound_leak_01() {
    struct Cycle {
        neighbors: Vec<Gc<Cycle>>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.neighbors.visit_children(visitor);
        }
    }

    // 0
    let a = Gc::new(Cycle { neighbors: vec![] });
    // 1
    let b = Gc::new(Cycle { neighbors: vec![] });
    // 8
    let c = Gc::new(Cycle { neighbors: vec![] });
    // 13
    let d = Gc::new(Cycle { neighbors: vec![] });

    // 0 -> 0
    a.borrow_mut().neighbors.push(a.clone());
    // 0 -> 13
    a.borrow_mut().neighbors.push(d.clone());
    // 1 -> 8
    b.borrow_mut().neighbors.push(c.clone());
    // 8 -> 0
    c.borrow_mut().neighbors.push(a.clone());
    // 13 -> 13
    d.borrow_mut().neighbors.push(d.clone());
    // 13 -> 0
    d.borrow_mut().neighbors.push(a.clone());

    let b_stack = b.clone();
    let b_borrow = b_stack.borrow();

    let c_stack = c.clone();
    let c_borrow = c_stack.borrow();

    let d_stack = d.clone();
    let d_borrow = d_stack.borrow();

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
    struct Cycle {
        neighbors: Vec<Gc<Cycle>>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.neighbors.visit_children(visitor);
        }
    }

    // 0
    let a = Gc::new(Cycle { neighbors: vec![] });
    // 1
    let b = Gc::new(Cycle { neighbors: vec![] });
    // 4
    let c = Gc::new(Cycle { neighbors: vec![] });
    // 8
    let d = Gc::new(Cycle { neighbors: vec![] });
    // 13
    let e = Gc::new(Cycle { neighbors: vec![] });

    // 0 -> 0
    a.borrow_mut().neighbors.push(a.clone());
    // 1 -> 8
    b.borrow_mut().neighbors.push(d.clone());
    // 4 -> 13
    c.borrow_mut().neighbors.push(e.clone());
    // 8 -> 0
    d.borrow_mut().neighbors.push(a.clone());
    // 13 -> 0
    // 13 -> 13
    e.borrow_mut().neighbors.push(a.clone());
    e.borrow_mut().neighbors.push(e.clone());

    let b_stack = b.clone();
    let b_borrow = b_stack.borrow();

    let d_stack = d.clone();
    let d_borrow = d_stack.borrow();

    let e_stack = e.clone();
    let e_borrow = e_stack.borrow();

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

#[test]
fn fuzzfound_uaf_01() {
    // Shuffles nodes in & out of the old gen and trace buffer until:
    // - unbuffering from the trace sees a node that is in the old/young gen
    // - that same node gets cleaned up later either by the collector or the destructor of a child
    //   node with one parent remaining.
    // if a bug is present in decrementing the reference count during cycle tracing, this will
    // trigger a UAF.
    struct Cycle {
        neighbors: Vec<Gc<Cycle>>,
    }

    impl Trace for Cycle {
        fn visit_children(&self, visitor: &mut GcVisitor) {
            self.neighbors.visit_children(visitor);
        }
    }

    // 0
    let a = Gc::new(Cycle { neighbors: vec![] });
    // 1
    let b = Gc::new(Cycle { neighbors: vec![] });
    // 2
    let c = Gc::new(Cycle { neighbors: vec![] });
    // 4
    let d = Gc::new(Cycle { neighbors: vec![] });
    // 7
    let f = Gc::new(Cycle { neighbors: vec![] });
    // 8
    let g = Gc::new(Cycle { neighbors: vec![] });
    // 11
    let i = Gc::new(Cycle { neighbors: vec![] });

    // 0 -> 4
    a.borrow_mut().neighbors.push(d.clone());

    // 0 -> 1
    a.borrow_mut().neighbors.push(b.clone());

    // 0 -> 4
    a.borrow_mut().neighbors.push(d.clone());

    // 2 -> 0
    c.borrow_mut().neighbors.push(a.clone());

    // 4 -> 11
    d.borrow_mut().neighbors.push(i.clone());

    // 8 -> 7
    g.borrow_mut().neighbors.push(f.clone());

    // 8 -> 0
    g.borrow_mut().neighbors.push(a.clone());

    // 11 -> 1
    i.borrow_mut().neighbors.push(b.clone());

    let a_stack = a.clone();
    let a_borrow = a_stack.borrow();

    let g_stack = g.clone();
    let g_borrow = g_stack.borrow();

    let i_stack = i.clone();
    let i_borrow = i_stack.borrow();

    drop(a);
    drop(b);
    drop(c);
    drop(d);
    drop(f);
    drop(g);
    drop(i);

    collect_full();

    drop(a_borrow);
    drop(g_borrow);
    drop(i_borrow);

    drop(a_stack);
    drop(g_stack);
    drop(i_stack);

    collect_full();
    collect_full();
}
