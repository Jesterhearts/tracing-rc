# tracing-rc
Cycle collecting reference counted pointers for Rust with a safe, simple api.

The `Gc` type implemented by this crate provides a cycle-aware smart pointer in the style of
`std::rc::Rc`, the major difference being that you cannot have weak references to a `Gc`. This is
useful if you have some number of interlinked data structures - e.g. `GraphNode`, `LuaTable`, etc. -
which don't have clear lifetimes or ownership.

`Gc` is probably the wrong choice for many usecases:
- If you have a data structure with clear parent-child relationships (like a doubly-linked list or
  tree), you should probably just use `std::rc::Rc` and `Weak`.
- If your data is only used for a certain scope and discarded wholesale, you will probably benefit
  from a crate like [typed-arena](https://lib.rs/crates/typed-arena) or
  [bumpalo](https://crates.io/crates/bumpalo)
- If you have a single type and want to add/remove values, something like
  [generational-arena](https://lib.rs/crates/generational-arena) is probably best.

# Usage of Unsafe
The library has two usages of unsafe where it drops the inner data of the `Gc` and `Agc` types. In
the case of `Gc`, this unsafe is protected by a `borrow_mut` call to a `RefCell` which is
intentionally leaked after dropping the data, making the inner (dropped) data inaccessible. `Agc`
similarly leaks a `WriteMutexGuard` to its inner data post-drop.

# Support for Concurrent Collection
This library has experimental support for concurrent collection behind the `sync` flag. Enabling the
`sync` flag will provide access to an atomic `Agc` type as well as concurrent `collect`
implementations.

# Soundness & Rc Collector Design Considerations
Because any implementation of the `Trace` trait and custom `Drop` implementations for objects
owned by a garbage collected pointer can run arbitrary code, they may attempt to create new copies or
drop existing traced objects (even already traced ones) in the middle of collection. In addition,
due to bugs in client code, `Trace` may report more items as children than it actually owns
(reporting fewer is trivially safe, as it will simply leak).

In order for this crate to be sound and present a safe `Trace` trait, the collector must not
cause undefined behavior in any of the scenarios outlined. In order to accomplish this, the
collector does the following:
1. Items that are waiting for collection or have been visited during tracing are given an extra
   strong or weak reference to make sure the memory remains allocated and the data it contains remains valid
   even if strong references are dropped during traversal.
2. Reference counts for traced items are not decremented by the collector during traversal (this is
   a difference from e.g. Bacon-Rajan which originally inspired this crate). The collector instead
   keeps a count of the number of times it reached each pointer through tracing, and then compares
   that count against the strong count for each pointer it traced to decide if the item is dead.
3. Before dropping any values, the collector marks the object dead. During this process, bugs in the
   `Trace` implementation may cause the collector to believe a dead value is still alive
   (causing a leak) or a live value is dead (making it inaccessible, but leaving the gc pointer
   valid). Safe code is unable to access dead values (it will panic or return Option::None), and
   cannot restore the live state of a dead object. Values are never temporarily dead, the collector
   only marks them dead after it has fully determined that it is a valid candidate for collection
   (all strong refs are from members of its cycle).
4. After the full set of traced objects has been marked, the collector begins dropping the inner
   data of objects it believes to be dead, after checking if there are any oustanding borrows
   (immutable or mutable). This drop _does not_ and _can not_ free the memory for the gc pointer,
   nor does it make the reference count or liveness inaccessible. The borrow check ensures that data
   isn't dropped out from underneath active borrows.
5. After it has completed dropping of the inner data of dead values, the collector releases its
   strong/weak reference. If the cycle was broken, that reference will be the last remaining
   reference to the `Gc` value, and its memory will be cleaned up. If there are remaining
   references, its memory will be cleaned up when the oustanding references fall out of scope.
 
There are a decent number number of tests designed to exercise each of these scenarios included, and all
of these tests pass miri (barring leaks for intentionally misbehaved code).

# Example
```rs
use tracing_rc::rc::{
    collect_full,
    Gc,
    GcVisitor,
    Trace,
};

struct GraphNode<T: 'static> {
    data: T,
    edge: Option<Gc<GraphNode<T>>>,
}

impl<T> Trace for GraphNode<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        self.edge.visit_children(visitor);
    }
}

fn main() {
    {
        let node_a = Gc::new(GraphNode {
            data: 10,
            edge: None,
        });
        let node_b = Gc::new(GraphNode {
            data: 11,
            edge: None,
        });
        let node_c = Gc::new(GraphNode {
            data: 12,
            edge: Some(node_a.clone()),
        });

        node_a.borrow_mut().edge = Some(node_b.clone());
        node_b.borrow_mut().edge = Some(node_c);

        let a = node_a.borrow();
        let b = a.edge.as_ref().unwrap().borrow();
        let c = b.edge.as_ref().unwrap().borrow();

        assert_eq!(a.data, c.edge.as_ref().unwrap().borrow().data);
        // all of the nodes go out of scope at this point and would normally be leaked.
    }

    // In this simple example, we always have cycles and our program is complete after this,
    // so we can't take advantage of the young generation picking up acyclic pointers without
    // tracing.
    collect_full();

    // All leaked nodes have been cleaned up!
}

```

# Other GC Implementations
There are a number of excellent implementations of garbage collection in Rust, some of which
inspired this crate.
* [bacon_rajan_cc](https://crates.io/crates/bacon_rajan_cc)
    * Like this crate, it is based on the Bacon-Rajan algorithm and provides a `Trace` trait which
      is used for cycle tracing. It was very helpful in understanding how the original paper works,
      although this crate takes a very different approach.
* [gc-arena](https://crates.io/crates/gc-arena)
    * A very neat implementation of garbage collection relying on lifetimes and arenas rather than
      cycle tracing.
* [cactusref](https://crates.io/crates/cactusref)
    * Another take on cycle collection which is pretty novel.
* [rust-gc](https://crates.io/crates/gc)
* [shredder](https://crates.io/crates/shredder)
* [shifgrethor](https://github.com/withoutboats/shifgrethor)