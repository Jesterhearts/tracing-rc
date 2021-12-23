use std::{
    mem::ManuallyDrop,
    sync::Arc,
};

use atomic::Atomic;
use parking_lot::RwLock;

use crate::sync::collector::GcVisitor;

mod collector;

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

pub trait Trace: Send + Sync {
    fn visit_children(&self, visitor: &mut GcVisitor);
}

impl Trace for () {
    fn visit_children(&self, _: &mut GcVisitor) {}
}

pub struct Agc<T: Trace + 'static> {
    ptr: Arc<AtomicInner<T>>,
}

impl<T> Agc<T>
where
    T: Trace + 'static,
{
    fn new(data: T) -> Self {
        Self {
            ptr: Arc::new(AtomicInner {
                status: Atomic::new(Status::Live),
                data: RwLock::new(ManuallyDrop::new(data)),
            }),
        }
    }
}

struct AtomicInner<T: ?Sized + Trace + 'static> {
    status: Atomic<Status>,
    data: RwLock<ManuallyDrop<T>>,
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
    T: ?Sized + Trace + 'static,
{
    fn drop_data(ptr: &Arc<Self>) {
        if let Some(mut data_guard) = ptr.data.try_write() {
            ptr.status.store(Status::Dead, atomic::Ordering::Release);

            unsafe { ManuallyDrop::drop(&mut data_guard) };

            std::mem::forget(data_guard);
        }
    }
}
