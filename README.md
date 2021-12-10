# tracing-rc
Cycle collecting reference counted pointers for Rust with a safe api.


TODO: This does not yet expose a safe api, but I'm pretty confident at this point that a safe api
_is_ possible and still reasonably performant and effective at capturing leaked cycles.


# Rc Collector Design Considerations
Because any implementation of the `Traceable` trait and custom `Drop` implementations of objects
owned by a garbage collected pointer can run arbitrary code, it may attempt to create new copies or
drop existing traced objects (even already traced ones) in the middle of collection. In addition,
due to bugs in client code, `Traceable` may report more items as children than it actually owns
(reporting fewer is trivially safe, as it will simply leak).

In order for this crate to be sound and present a safe `Traceable` trait, the collector must not
cause undefined behavior in any of the scenarios outlined. In order to accomplish this, the
collector does the following:
1. Items that are waiting for collection or have been visited during tracing are given an extra
   strong reference to make sure the memory remains allocated and the data it contains remains valid
   even if pointers are dropped during traversal.
2. Reference counts for traced items are not decremented by the collector during traversal (this is
   a difference from e.g. Bacon-Rajan which originally inspired this crate). The collector instead
   keeps a count of the number of times it reached each pointer through tracing, and then compares
   that count against the strong count for each pointer it traced to decide if the item is dead.
3. Before dropping any values, the collector marks the object dead. During this process, bugs in the
   `Traceable` implementation may cause the collector to believe a dead value is still alive
   (causing a leak) or a live value is dead (making it inaccessible, but leaving the GC pointer
   valid). Safe code is unable to access dead values (it will panic or return Option::None), and
   cannot restore the live state of a dead object. The collector itself only restores the live state
   for objects that it marked dead during the current collection cycle, and it only does so prior to
   performing any drops.
4. After the full set of traced objects has been marked, the collector begins dropping the inner
   data of objects it believes to be dead. This drop _does not_ and _can not_ free the memory for
   the GC pointer, nor does it make the reference count or liveness inaccessible.
5. After it has completed dropping of the inner data of dead values, the collector re-examines the
   list of dead values and checks their reference counts. Because GC pointers always decrement their
   refcount during drop, if the cycle was correctly cleaned up, the only remaining reference count
   will be the strong ref added by the collector itself. If the collector sees this, it knows that
   there is no way for safe code to still access the GC pointer and it can safely de-allocate it.
   
   If the number of references has _not_ dropped to zero, the collector marks the node as a
   `Zombie`, indicating that its inner data has already been dropped, but that the GC pointer itself
   may still be reachable from safe code. Safe code is prevented from getting a reference to the
   data stored in a zombie pointer. When a zombie value's refcount reaches zero it is automatically
   de-allocated, as it is impossible for it to participate in any cycles once its inner data has
   been dropped.

There is an extensive number of tests designed to exercise each of these scenarios included, and all
of these tests pass miri.