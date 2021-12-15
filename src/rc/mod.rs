use std::{
    cell::{
        Cell,
        Ref,
        RefCell,
        RefMut,
    },
    marker::PhantomData,
    mem::ManuallyDrop,
    num::NonZeroUsize,
    ptr::NonNull,
};

/// Collection algorithms for cleaning up dead [`Gc`]s.
pub mod collector;
/// Contains the [`Traceable`] trait which must be implemented for items stored in a [`Gc`].
pub mod traceable;

pub use collector::{
    collect,
    collect_full,
    collect_with_options,
    GcVisitor,
};
use collector::{
    Node,
    OLD_GEN,
    YOUNG_GEN,
};
pub use traceable::Traceable;

use crate::Status;

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
#[derive(Debug)]
pub struct Gc<T: Traceable + 'static> {
    ptr: NonNull<Inner<T>>,
    _t: PhantomData<T>,
}

impl<T> Traceable for Gc<T>
where
    T: Traceable + 'static,
{
    fn visit_children(&self, visitor: &mut GcVisitor) {
        visitor.visit_node(self);
    }
}

impl<T> Gc<T>
where
    T: Traceable + 'static,
{
    /// Construct a new Gc containing `data` which will be automatically cleaned up with it is no
    /// longer reachable, even in the presence of cyclical references.
    pub fn new(data: T) -> Self {
        Self {
            ptr: Box::leak(Box::new(Inner {
                strong: Cell::new(NonZeroUsize::new(1).unwrap()),
                status: Cell::new(Status::Live),
                buffered: Cell::new(false),
                data: ManuallyDrop::new(RefCell::new(data)),
            }))
            .into(),
            _t: PhantomData,
        }
    }
}

impl<T> Gc<T>
where
    T: Traceable + 'static,
{
    /// Retrieve the current number of strong references outstanding.
    pub fn strong_count(this: &Self) -> usize {
        this.get_inner().strong.get().get()
    }

    /// Returns true if the data in Self hasn't been dropped yet. This will almost always be the
    /// case unless it is called inside of a Drop implementation or if there is a bug present in a
    /// Traceable impl.
    pub fn is_live(this: &Self) -> bool {
        this.get_inner().is_live()
    }

    /// Get a reference into this Gc.
    ///
    /// The borrow of the data ends until the returned `Ref` exists the scope. Multiple immutable
    /// borrows may be taken out at the same time.
    ///
    /// # Panics
    /// Panics if the value is currently mutably borrowed.
    pub fn borrow(&self) -> Ref<'_, T> {
        assert!(Self::is_live(self));
        // SAFETY: Ptr is nonnull and not dangline and we asserted that self is live.
        unsafe { (&*self.ptr.as_ref().data).borrow() }
    }

    /// Try to get a reference into this Gc.
    ///
    /// Returns None if the pointer is dead or immutably borrowed, as the data contained in this
    /// pointer is possibly cleaned up or cannot be aliased.
    pub fn try_borrow(&self) -> Option<Ref<'_, T>> {
        if self.get_inner().is_live() {
            // SAFETY: Ptr is nonnull and not dangline and we asserted that self is live.
            unsafe { (&*self.ptr.as_ref().data).try_borrow().ok() }
        } else {
            None
        }
    }

    /// Get a mutable reference into this Gc.
    ///
    /// Similar to `RefCell`, the mutable borrow of the data lasts until the returned RefMut or all
    /// RefMuts derived from it exit scope.
    ///
    /// # Panics
    /// Panics if the value is currently borrowed.
    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        assert!(Self::is_live(self));
        // SAFETY: Ptr is nonnull and not dangline and we asserted that self is live.
        unsafe { (&*self.ptr.as_ref().data).borrow_mut() }
    }

    /// Try to get a mutable referenced into this Gc.
    ///
    /// Will return None if there are oustanding borrows, or if the pointer is dead.
    pub fn try_borrow_mut(&self) -> Option<RefMut<'_, T>> {
        if Self::is_live(self) {
            // SAFETY: We checked self is still alive.
            unsafe { (&*self.ptr.as_ref().data).try_borrow_mut().ok() }
        } else {
            None
        }
    }

    /// Returns true if both this & other point to the same allocation.
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        std::ptr::eq(this.ptr.as_ptr(), other.ptr.as_ptr())
    }
}

impl<T> Gc<T>
where
    T: Traceable + 'static,
{
    fn node(&self) -> Node {
        Node {
            inner_ptr: self.coerce_inner(),
        }
    }

    fn get_inner(&self) -> SafeInnerView<'_> {
        // SAFETY: Barring bugs in the collector, self.ptr is not dangling and not null. We don't
        // access inner.data.
        unsafe { SafeInnerView::from(self.ptr.as_ref()) }
    }

    fn coerce_inner(&self) -> NonNull<Inner<dyn Traceable>> {
        // SAFETY: Barring bugs in the collector, self.ptr is not dangling and not null.
        unsafe { NonNull::new_unchecked(self.ptr.as_ptr() as *mut Inner<dyn Traceable>) }
    }
}

impl<T> Clone for Gc<T>
where
    T: Traceable + 'static,
{
    fn clone(&self) -> Self {
        let inner = self.get_inner();
        inner.mark_live();

        inner.increment_strong_count();

        Self {
            ptr: self.ptr,
            _t: PhantomData,
        }
    }
}

impl<T: Traceable + 'static> Drop for Gc<T> {
    fn drop(&mut self) {
        // SAFETY: We do not hold open refs to inner in this scope since it may get de-allocated
        // later. We only de-allocate if there are no outstanding strong references. We only drop if
        // the node is dead.
        unsafe {
            if self.ptr.as_ref().strong.get() == NonZeroUsize::new(1).unwrap() {
                // This is the last remaining strong ref to this value so we can
                // safely drop the inner value and de-allocate the container.
                Inner::zombie_safe_drop_data_dealloc(self.ptr);
                return;
            }
        }

        // SAFETY: ptr is not null & not dangling
        let inner = unsafe { self.ptr.as_ref() };
        if inner.status.get() == Status::Dead {
            // SAFETY: We own a strong reference.
            unsafe { inner.decrement_strong_count() };
        } else {
            // SAFETY: We've checked that inner is not dead and can safely touch it.
            let marked_decref = unsafe { inner.try_mark_ref_dropped() };
            if marked_decref && !inner.buffered.replace(true) {
                let ptr = self.coerce_inner();

                // It's possible that this will turn out to be a member of a cycle, so we need
                // to add it to the list of items the collector tracks.
                YOUNG_GEN.with(|gen| {
                    debug_assert!(OLD_GEN.with(|gen| !gen.borrow().contains(&ptr)));

                    let mut gen = gen.borrow_mut();
                    debug_assert!(!gen.contains_key(&ptr));

                    // Because we haven't decremented the strong count yet, we can safely just
                    // add this to the young gen without fear of early
                    // deletion.
                    gen.insert(ptr, 0);
                });
            } else {
                // SAFETY: We own a strong reference & we know that the either the collector is
                // tracking the value or someone else is.
                unsafe { inner.decrement_strong_count() }
            }
        }
    }
}

impl<T: Traceable + 'static> PartialEq for Gc<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        *self.borrow() == *other.borrow()
    }
}

impl<T: Traceable + 'static> Eq for Gc<T> where T: Eq {}

impl<T: Traceable + 'static> PartialOrd for Gc<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.borrow().partial_cmp(&other.borrow())
    }
}

impl<T: Traceable + 'static> Ord for Gc<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.borrow().cmp(&other.borrow())
    }
}

struct SafeInnerView<'v> {
    strong: &'v Cell<NonZeroUsize>,
    status: &'v Cell<Status>,
}

impl<'v, T> From<&'v Inner<T>> for SafeInnerView<'v>
where
    T: ?Sized + Traceable,
{
    fn from(inner: &'v Inner<T>) -> Self {
        SafeInnerView {
            strong: &inner.strong,
            status: &inner.status,
        }
    }
}

impl SafeInnerView<'_> {
    fn increment_strong_count(&self) {
        self.strong.set(
            self.strong
                .get()
                .get()
                .checked_add(1)
                .and_then(NonZeroUsize::new)
                .expect("Strong count overflowed"),
        );
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
}

struct Inner<T: Traceable + ?Sized> {
    strong: Cell<NonZeroUsize>,
    status: Cell<Status>,
    buffered: Cell<bool>,
    data: ManuallyDrop<RefCell<T>>,
}

impl<T> std::fmt::Debug for Inner<T>
where
    T: Traceable + ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Inner")
            .field("strong", &self.strong)
            .field("status", &self.status)
            .field("buffered", &self.buffered)
            .field(
                "data",
                &format!("{} @ {:?}", std::any::type_name::<T>(), self.data.as_ptr()),
            )
            .finish()
    }
}

impl<T> Inner<T>
where
    T: Traceable + ?Sized,
{
    /// # Safety:
    /// - ptr.data must not have been dropped.
    /// - ptr.data must not have any aliasing borrows.
    unsafe fn drop_data(ptr: NonNull<Self>) {
        debug_assert!(ptr.as_ref().data.try_borrow_mut().is_ok());

        ManuallyDrop::drop(&mut (*ptr.as_ptr()).data);
    }

    /// # Safety:
    /// - ptr must not have been deallocated.
    /// - ptr must have been created by [`Box::into_raw`] or [`Box::leak`]
    /// - ptr must not be reachable from safe code.
    unsafe fn dealloc(ptr: NonNull<Self>) {
        debug_assert_eq!(ptr.as_ref().strong.get().get(), 1);

        let boxed = Box::from_raw(ptr.as_ptr());
        drop(boxed);
    }

    unsafe fn zombie_safe_drop_data_dealloc(ptr: NonNull<Self>) {
        if ptr.as_ref().status.get() != Status::Dead {
            Self::drop_data(ptr);
        }

        Self::dealloc(ptr);
    }

    /// # Safety:
    /// self.data must not be dropped.
    #[must_use]
    unsafe fn try_mark_ref_dropped(&self) -> bool {
        // If someone has an open borrow to data, it cannot be dead.
        if self.data.try_borrow_mut().is_ok() && self.status.get() == Status::Live {
            self.status.set(Status::RecentlyDecremented);
            true
        } else {
            false
        }
    }

    /// # Safety:
    /// - The caller of this function must not attempt to access self after calling this function.
    unsafe fn decrement_strong_count(&self) {
        self.strong.set(
            NonZeroUsize::new(self.strong.get().get() - 1).expect("Underflow in strong count"),
        );
    }

    /// # Safety:
    /// - The caller of this function must not attempt to access self after calling this function.
    unsafe fn unbuffer_from_collector(&self) {
        self.buffered.set(false);
        self.decrement_strong_count();
    }

    /// Attempts to mark the node dead if it is possible to get exclusive access to data. This will
    /// not always be true in the presense of buggy Traceable implementations.
    #[must_use]
    fn try_mark_dead(&self) -> bool {
        if self.data.try_borrow_mut().is_ok() {
            self.status.set(Status::Dead);
            true
        } else {
            false
        }
    }
}
