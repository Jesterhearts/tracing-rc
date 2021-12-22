use std::{
    cell::{
        Cell,
        RefCell,
    },
    mem::ManuallyDrop,
    ops::{
        Deref,
        DerefMut,
    },
    rc::Rc,
};

/// Collection algorithms for cleaning up dead [`Gc`]s.
mod collector;
/// Contains the [`Trace`] trait which must be implemented for items stored in a [`Gc`].
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
pub use trace::Trace;

use crate::Status;

/// Wraps an immutable borrowed reference to a value in a [`Gc`].
pub struct Ref<'a, T: ?Sized> {
    cell_ref: std::cell::Ref<'a, ManuallyDrop<T>>,
}

impl<T: ?Sized> Deref for Ref<'_, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        self.cell_ref.deref().deref()
    }
}

/// Wraps a mutable borrowed reference to a value in a [`Gc`].
pub struct RefMut<'a, T: ?Sized> {
    cell_ref: std::cell::RefMut<'a, ManuallyDrop<T>>,
}

impl<T: ?Sized> Deref for RefMut<'_, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        self.cell_ref.deref().deref()
    }
}

impl<T: ?Sized> DerefMut for RefMut<'_, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.cell_ref.deref_mut().deref_mut()
    }
}

/// A cycle-collected reference-counted smart pointer.
///
/// `Gc<T>` provides shared ownership of a value of type `T`, allocated on the heap. Cloning it
/// will produce a new `Gc` instance which points to the same allocation as the original `Gc`.
///
/// `Gc` provides `RefCell` like behavior for its data so there is no need to wrap inner data in a
/// `Cell` or `RefCell` type in order to achieve interior mutability. You may use [`Gc::borrow`] and
/// [`Gc::borrow_mut`]
///
/// Unlike [`std::rc::Rc`], `Gc` pointers may refer to each other in a way that creates cycles
/// arbitrarily without causing leaks, provided that the program calls [`collect`] to collect those
/// cycles.
///
/// It's important to note a few things about collection:
/// - The default [`collect`] implementation only performs cycle-tracing collection if a node has
///   been in waiting for collection for a while. This is so that we don't pay the cost of tracing
///   for acyclic nodes which may be marked for collection due to the number of outstanding
///   references, but don't actually participate in a cycle. You may use [`collect_full`] to force
///   tracing collection of all pending nodes if you prefer.
/// - Dropping of data is deferred until a call to [`collect`] or [`collect_full`] is made, and the
///   collector is able to determine that the `Gc` is actually dead. The collector guarantees that
///   (in the absence of bugs) the [`Drop`] implementation for your type will be executed if it is
///   determined to be dead, but cannot provide any guarantees on **when** it will be executed.
///     - [`collect`] has weak guarantees even in the presence of acyclic pointers - if a node
///       containing your type is acyclic, but has N strong references, it may take up to `min(N,
///       `[`CollectOptions::old_gen_threshold`](crate::CollectOptions::old_gen_threshold)` + 1)`
///       calls to `collect` for the value to be cleaned up even if all parents are dropped.
///     - The collector does guarantee that if [`collect_full`] is used, the `Drop` implementation
///       for your data will have been executed by the time `collect_full` completes if the value is
///       dead.
/// - The collector does not make any guarantees about the relative order of drops for dead nodes.
///   Even if the order appears stable, you **may not** rely on it for program correctness.
pub struct Gc<T: Trace + 'static> {
    ptr: Rc<Inner<T>>,
}

impl<T> Trace for Gc<T>
where
    T: Trace + 'static,
{
    fn visit_children(&self, visitor: &mut GcVisitor) {
        visitor.visit_node(self);
    }
}

impl<T> Gc<T>
where
    T: Trace + 'static,
{
    /// Construct a new `Gc` containing `data` which will be automatically cleaned up with it is no
    /// longer reachable, even in the presence of cyclical references.
    pub fn new(data: T) -> Self {
        Self {
            ptr: Rc::new(Inner {
                status: Cell::new(Status::Live),
                data: RefCell::new(ManuallyDrop::new(data)),
            }),
        }
    }
}

impl<T> Gc<T>
where
    T: Trace + 'static,
{
    /// Retrieve the current number of strong references outstanding.
    pub fn strong_count(this: &Self) -> usize {
        Rc::strong_count(&this.ptr)
    }

    /// Returns true if the data in Self hasn't been dropped yet. This will almost always be the
    /// case unless it is called inside of a Drop implementation or if there is a bug present in a
    /// Trace impl.
    pub fn is_live(this: &Self) -> bool {
        this.ptr.is_live()
    }

    /// Get a reference into this `Gc`.
    ///
    /// The borrow of the data ends until the returned `Ref` exists the scope. Multiple immutable
    /// borrows may be taken out at the same time.
    ///
    /// # Panics
    /// Panics if the value is currently mutably borrowed.
    pub fn borrow(&self) -> Ref<'_, T> {
        self.try_borrow().unwrap()
    }

    /// Try to get a reference into this `Gc`.
    ///
    /// Returns None if the pointer is dead or immutably borrowed, as the data contained in this
    /// pointer is possibly cleaned up or cannot be aliased.
    pub fn try_borrow(&self) -> Option<Ref<'_, T>> {
        if let Ok(cell_ref) = self.ptr.data.try_borrow() {
            self.ptr.mark_live();
            Some(Ref { cell_ref })
        } else {
            None
        }
    }

    /// Get a mutable reference into this `Gc`.
    ///
    /// Similar to `RefCell`, the mutable borrow of the data lasts until the returned RefMut or all
    /// RefMuts derived from it exit scope.
    ///
    /// # Panics
    /// Panics if the value is currently borrowed.
    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.try_borrow_mut().unwrap()
    }

    /// Try to get a mutable referenced into this `Gc`.
    ///
    /// Will return None if there are oustanding borrows, or if the pointer is dead.
    pub fn try_borrow_mut(&self) -> Option<RefMut<'_, T>> {
        if let Ok(cell_ref) = self.ptr.data.try_borrow_mut() {
            self.ptr.mark_live();
            Some(RefMut { cell_ref })
        } else {
            None
        }
    }

    /// Returns true if both this & other point to the same allocation.
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Rc::ptr_eq(&this.ptr, &other.ptr)
    }

    fn node(&self) -> WeakNode {
        let inner_ptr = Rc::downgrade(&self.ptr);
        WeakNode { inner_ptr }
    }
}

impl<T> std::fmt::Debug for Gc<T>
where
    T: Trace + 'static,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Gc")
            .field("strong", &format_args!("{}", Self::strong_count(self)))
            .field("status", &format_args!("{:?}", self.ptr.status.get()))
            .field(
                "data",
                &format_args!(
                    "{:?} @ {:?}",
                    self.try_borrow().map(|_| std::any::type_name::<T>()),
                    self.ptr.data.as_ptr()
                ),
            )
            .finish()
    }
}

impl<T> Clone for Gc<T>
where
    T: Trace + 'static,
{
    fn clone(&self) -> Self {
        self.ptr.mark_live();

        Self {
            ptr: self.ptr.clone(),
        }
    }
}

impl<T: Trace + 'static> Drop for Gc<T> {
    fn drop(&mut self) {
        if Rc::strong_count(&self.ptr) == 1 {
            Inner::drop_data(&self.ptr);
        } else if self.ptr.status.get() != Status::Dead && self.ptr.try_mark_ref_dropped() {
            let node = self.node();

            // It's possible that this will turn out to be a member of a cycle, so we need
            // to add it to the list of items the collector tracks.
            YOUNG_GEN.with(|gen| {
                let mut gen = gen.borrow_mut();
                // Because we haven't decremented the strong count yet, we can safely just
                // add this to the young gen without fear of early
                // deletion. It might already be present there, but that's fine.
                gen.insert(node, 0);
            });
        }
    }
}

impl<T: Trace + 'static> PartialEq for Gc<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        *self.borrow() == *other.borrow()
    }
}

impl<T: Trace + 'static> Eq for Gc<T> where T: Eq {}

impl<T: Trace + 'static> PartialOrd for Gc<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.borrow().partial_cmp(&other.borrow())
    }
}

impl<T: Trace + 'static> Ord for Gc<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.borrow().cmp(&other.borrow())
    }
}

pub(crate) struct Inner<T: Trace + ?Sized> {
    status: Cell<Status>,
    data: RefCell<ManuallyDrop<T>>,
}

impl<T> std::fmt::Debug for Inner<T>
where
    T: Trace + ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Inner")
            .field("status", &self.status)
            .field(
                "data",
                &format_args!("{} @ {:?}", std::any::type_name::<T>(), self.data.as_ptr()),
            )
            .finish()
    }
}

impl<T> Inner<T>
where
    T: Trace + ?Sized,
{
    fn drop_data(ptr: &Rc<Self>) {
        if let Ok(mut mut_borrow) = ptr.data.try_borrow_mut() {
            ptr.status.set(Status::Dead);

            // SAFETY: During drop of the data, we forget a mut borrow of it, which bans all access
            // to the data afterwards. If we're able to reach here, we're the last
            // mutable borrower.
            unsafe { ManuallyDrop::drop(&mut mut_borrow) };

            // TODO: Once RefMut::leak is stable, use that here.
            std::mem::forget(mut_borrow);
        }
    }

    fn is_live(&self) -> bool {
        matches!(
            self.status.get(),
            Status::Live | Status::RecentlyDecremented
        )
    }

    fn mark_live(&self) {
        if self.status.get() == Status::RecentlyDecremented {
            self.status.set(Status::Live);
        }
    }

    #[must_use]
    fn try_mark_ref_dropped(&self) -> bool {
        if self.data.try_borrow_mut().is_ok() {
            self.status.set(Status::RecentlyDecremented);
            true
        } else {
            // If someone has an open borrow to data, it cannot be dead.
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        cell::{
            BorrowError,
            Cell,
            RefCell,
        },
        mem::ManuallyDrop,
        rc::Rc,
    };

    use pretty_assertions::assert_eq;

    use crate::{
        rc::Inner,
        Status,
    };

    #[test]
    fn data_inaccessible_post_drop() {
        let inner = Rc::new(Inner {
            status: Cell::new(Status::Live),
            data: RefCell::new(ManuallyDrop::new(())),
        });

        Inner::drop_data(&inner);

        assert!(matches!(inner.data.try_borrow(), Err(BorrowError { .. })));
        assert_eq!(inner.status.get(), Status::Dead);
    }
}
