use std::{
    mem::ManuallyDrop,
    ops::Deref,
    sync::Arc,
};

use atomic::Atomic;
use parking_lot::{
    RwLock,
    RwLockReadGuard,
};

/// Notes:
/// There are several moving parts the collector needs to care about in order to prevent leaks or
/// early drop.
///
/// 1. The reference count of traced objects. This may change at any time.
/// 2. The graph of child components. This may be altered any time a reference is access outside the
/// collector.
/// 3. The status of the object. This may change at any time.
///
/// If we decide an object is live, it is pretty safe to mark all of its children live since we
/// don't do anything clever with buffering. The logic here is that the collector takes out a strong
/// reference to the objects it traces, so any concurrent drops of objects during collection (making
/// them dead) will put a weak reference into the young gen. If the object is acyclical, the [`Arc`]
/// drop implementation will call drop on [`AtomicInner`] which will tear down its internal data
/// properly. If the object is a node in a cyclical graph, it will retain at least one strong count
/// which will allow the collector to upgrade its weak pointer and trace/collect the object.
///
/// If we decide an object is dead, we need to exercise caution when tearing down its data, since
/// the child graph may have changed between the time we traced it and the time we're checking
/// refcounts. If we know the child graph is unchanged, we can follow the normal rules for child
/// cleanup (cleanup all children whose refcounts come from the dead graph). If it has changed, it's
/// pretty safe to treat the object as still live, since _someone_ must have had a (possibly
/// transitive) strong reference to it in order to alter the child graph, and so we'll at least have
/// a weak reference in the old/young gen if it's been dropped since.
///
/// We track the dirty state of the child graph using [`Status::Traced`], which the collector sets
/// as the state of any node it reaches during tracing. All operations which expose references
/// overwrite this status (we can't rely on `&mut` due to interior mutability). This way, the
/// collector can know that if it sees a dead object with the `Traced` status, both that object and
/// its children are possibly dead. Conversely, if `Traced` has been overwritten, the object was
/// definitely still alive.
///
/// In order to prevent races with altering child graphs during tracing, the collector acquires an
/// exclusive lock prior to setting `Status::Traced`. This is to prevent races where a thread
/// acquires a reference to an object's data prior to its traced status being set and subsequently
/// modifies the graph through that reference after it's been set.
mod collector;

/// Contains the `sync` version of the [`Trace`] trait.
pub mod trace;

pub use collector::{
    collect,
    collect_full,
    collect_with_options,
    count_roots,
    GcVisitor,
};
use collector::{
    WeakNode,
    YOUNG_GEN,
};
#[doc(inline)]
pub use trace::Trace;

/// Wraps a shared reference to a value in a [`Agc`].
pub struct Ref<'a, T: ?Sized> {
    guard: RwLockReadGuard<'a, ManuallyDrop<T>>,
}

impl<T: ?Sized> Deref for Ref<'_, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        self.guard.deref().deref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    /// The node has had its refcount incremented recently or a mutable/immutable borrow checked
    /// out and is definitely alive.
    Live,
    /// The node had its reference count decremented recently and may be dead.
    RecentlyDecremented,
    /// The Collector visited this node during tracing, and the node has not been accessed since.
    /// This state must be invalidated by any operation which returns a references (mutable or
    /// otherwise), or the collector may prematurely destroy data.
    ///
    /// The particular area of concern is a graph that looks like:
    /// - `Stack -> (Candidate) A -> B`
    /// - `Thread A: Traverse A -> B`
    /// - `Collector: Trace A, B`
    /// - `Thread A: Attach B <- C, release A`
    /// - `{ A (dead) \\ B <- C (Stack) }`
    /// - `Collector: Check A, A is dead (strong = 1, count = 1) & B is dead (strong = 2, count =
    ///   2).`
    ///
    /// If we don't know that A was possibly modified (and may have had its child graph modified),
    /// we'll try to destroy B. By invalidating `Traced` on access to A's data, we know to remove B
    /// from the list of `Dead` nodes.
    Traced,
    /// The Collector completed collection and believes the node is dead. This status is
    /// unrecoverable.
    Dead,
}

/// A cycle-collected reference-counted smart pointer which may be shared across threads and
/// supports concurrent collection.
///
/// `Agc<T>` provides shared ownership of a value of type `T`, allocated on the heap. Cloning it
/// will produce a new `Agc` instance which points to the same allocation as the original `Agc`.
///
/// Unlike [`std::sync::Arc`], `Agc` pointers may refer to each other in a way that creates cycles
/// arbitrarily without causing leaks, provided that the program calls [`collect`] to collect those
/// cycles.
///
/// - In most cases you **must** call collect to reclaim memory for dropped `Agc`s _even if they are
///   acyclical_.
/// - You may call [`collect`] from any thread to collect cycles. The implementation of collect is
///   intended to provide very low pause times for concurrently running threads, although it will
///   block the thread which actually invokes [`collect`] until the collection is complete.
///     - While it may be subject to change, the current implementation will block access to `Agc`s
///       data for as long as it takes to visit its direct descendents, and will only do so if that
///       `Agc` is a candidate for collection.
pub struct Agc<T: Trace + 'static> {
    ptr: Arc<AtomicInner<T>>,
}

impl<T> Agc<T>
where
    T: Trace + 'static,
{
    /// Construct a new `Agc` containing `data` which will be automatically cleaned up with it is no
    /// longer reachable, even in the presence of cyclical references.
    pub fn new(data: T) -> Self {
        Self {
            ptr: Arc::new(AtomicInner {
                status: Atomic::new(Status::Live),
                data: RwLock::new(ManuallyDrop::new(data)),
            }),
        }
    }
}

impl<T> Agc<T>
where
    T: Trace + 'static,
{
    /// Retrieve the current number of strong references outstanding.
    pub fn strong_count(this: &Self) -> usize {
        Arc::strong_count(&this.ptr)
    }

    /// Blocks the thread until it can acquire a non-exclusive reference into this `Agc`.
    ///
    /// The returned object is an RAII guard which releases a shared lock. Multiple references may
    /// be taken out at the same time.
    ///
    /// # Deadlocks
    /// May deadlock if the value is `Dead`. This should not occur under normal circumstances and
    /// likely indicates a bug in a [`Trace`] implementation.
    ///
    /// # Panics
    /// Panics if the value is `Dead`. This should not occur under normal circumstances and likely
    /// indicates a bug in a [`Trace`] implementation.
    pub fn read(&self) -> Ref<'_, T> {
        if self.ptr.status.load(atomic::Ordering::Acquire) != Status::Dead {
            let guard = self.ptr.data.read_recursive();
            // We must update this after acquiring the lock to prevent races with tracing and seeing
            // nodes as possibly dirty.
            self.ptr
                .status
                .store(Status::Live, atomic::Ordering::Release);
            Ref { guard }
        } else {
            panic!("Attempted to acquire a reference to a dead object");
        }
    }

    /// Try to get a non-exclusive reference into this `Agc`. This will not block the calling
    /// thread. This may fail if the object is being concurrently traced by the collector.
    ///
    /// Returns None if the pointer is dead or exclusively referenced.
    pub fn try_borrow(&self) -> Option<Ref<'_, T>> {
        if self.ptr.status.load(atomic::Ordering::Acquire) != Status::Dead {
            if let Some(guard) = self.ptr.data.try_read_recursive() {
                // We must update this after acquiring the lock to prevent races with tracing and
                // seeing nodes as possibly dirty.
                self.ptr
                    .status
                    .store(Status::Live, atomic::Ordering::Release);
                return Some(Ref { guard });
            }
        }

        None
    }

    /// Returns true if both this & other point to the same allocation.
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Arc::ptr_eq(&this.ptr, &other.ptr)
    }

    fn node(&self) -> WeakNode {
        let inner_ptr = Arc::downgrade(&self.ptr);
        WeakNode { inner_ptr }
    }
}

impl<T> Drop for Agc<T>
where
    T: Trace + 'static,
{
    fn drop(&mut self) {
        if self
            .ptr
            .status
            .fetch_update(
                atomic::Ordering::SeqCst,
                atomic::Ordering::SeqCst,
                |status| match status {
                    Status::Dead => None,
                    _ => Some(Status::RecentlyDecremented),
                },
            )
            .is_err()
        {
            return;
        }

        // It is okay if this overwrites/is overwritten by a `Status::Traced`. We won't drop the
        // inner data here, since the collector holds a strong reference, so we're not worried about
        // child destructors altering the object graph. If the collector completes its tracing, it
        // will end up believing this object is live anyways, since we haven't dropped the strong
        // reference yet. Since we are going to always store a weak reference in the young gen,
        // we're not worried about losing track of the object if it needs later cleanup.
        self.ptr
            .status
            .store(Status::RecentlyDecremented, atomic::Ordering::Release);

        let weak_node = self.node();
        YOUNG_GEN.insert(weak_node, 0);
    }
}

impl<T> Trace for Agc<T>
where
    T: Trace + 'static,
{
    fn visit_children(&self, visitor: &mut GcVisitor) {
        visitor.visit_node(self)
    }
}

impl<T> std::fmt::Debug for Agc<T>
where
    T: Trace + 'static,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Agc")
            .field("strong", &format_args!("{}", Self::strong_count(self)))
            .field(
                "data",
                &format_args!(
                    "{:?} @ {:?}",
                    self.try_borrow().map(|_| std::any::type_name::<T>()),
                    (&**self.ptr.data.read_recursive() as *const T)
                ),
            )
            .finish()
    }
}

impl<T> Clone for Agc<T>
where
    T: Trace + 'static,
{
    fn clone(&self) -> Self {
        let _guard = self.ptr.data.read_recursive();
        self.ptr
            .status
            .store(Status::Live, atomic::Ordering::Release);

        Self {
            ptr: self.ptr.clone(),
        }
    }
}

impl<T: Trace + 'static> PartialEq for Agc<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        *self.read() == *other.read()
    }
}

impl<T: Trace + 'static> Eq for Agc<T> where T: Eq {}

impl<T: Trace + 'static> PartialOrd for Agc<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.read().partial_cmp(&other.read())
    }
}

impl<T: Trace + 'static> Ord for Agc<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.read().cmp(&other.read())
    }
}

pub(crate) struct AtomicInner<T: ?Sized + Trace> {
    status: Atomic<Status>,
    data: RwLock<ManuallyDrop<T>>,
}

impl<T> Drop for AtomicInner<T>
where
    T: ?Sized + Trace,
{
    fn drop(&mut self) {
        self.drop_data();
    }
}

impl<T> std::fmt::Debug for AtomicInner<T>
where
    T: ?Sized + Trace,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Inner")
            .field("status", &self.status)
            .field(
                "data",
                &format_args!(
                    "{} @ {:?}",
                    std::any::type_name::<T>(),
                    std::ptr::addr_of!(self.data)
                ),
            )
            .finish()
    }
}

impl<T> AtomicInner<T>
where
    T: ?Sized + Trace,
{
    fn drop_data(&self) {
        if let Some(mut data_guard) = self.data.try_write() {
            self.status.store(Status::Dead, atomic::Ordering::Release);

            unsafe { ManuallyDrop::drop(&mut data_guard) };

            // This is only okay because we're using `parking_lot` mutex. If this is a
            // `std::sync::RwLock` it will cause undefined behavior or leak.
            //
            // https://github.com/rust-lang/rust/issues/85434
            std::mem::forget(data_guard);
        }
    }
}
