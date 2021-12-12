use std::{
    cell::Cell,
    marker::PhantomData,
    mem::ManuallyDrop,
    num::NonZeroUsize,
    ops::Deref,
    ptr::NonNull,
};

pub mod collector;
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
/// `Gc<T>` provides shared ownership of a value of type `T`, allocated on the heap. Cloining it
/// will produce a new `Gc` instance which pints to the same allocation as the original `Gc`.
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
///       `[`CollectionOptions::old_gen_threshold`]` + 1)` calls to `collect` for the value to be
///       cleaned up even if all parents are dropped.
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
        visitor.visit_node(self)
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
                data: ManuallyDrop::new(data),
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
    pub fn strong_count(this: &Gc<T>) -> usize {
        this.get_inner().strong_count().get()
    }

    /// Returns true if the data in Self hasn't been dropped yet. This will almost always be the
    /// case unless it is called inside of a Drop implementation or if there is a bug present in a
    /// Traceable impl.
    pub fn is_live(this: &Gc<T>) -> bool {
        this.get_inner().is_live()
    }

    /// Get a reference into this Gc.
    ///
    /// Returns None if the pointer is dead, as the data contained in this
    /// pointer is possibly cleaned up.
    pub fn get(this: &Gc<T>) -> Option<&T> {
        if this.get_inner().is_live() {
            Some(this.as_ref())
        } else {
            None
        }
    }

    /// Try to get a mutable referenced into this Gc. Will return None if there are oustanding
    /// strong references to this, or if the pointer is dead.
    pub fn get_mut(this: &mut Gc<T>) -> Option<&mut T> {
        if this.get_inner().strong_count() == NonZeroUsize::new(1).unwrap() && Self::is_live(this) {
            // SAFETY: We are the only reference and our data is still alive.
            unsafe { Some(Self::get_mut_unchecked(this)) }
        } else {
            None
        }
    }

    /// Gets a reference into this Gc without checking if pointer is still
    /// alive.
    ///
    /// # Safety
    /// This gc pointer must not have had its inner data dropped yet
    pub unsafe fn get_unchecked(this: &Gc<T>) -> &T {
        &this.ptr.as_ref().data
    }

    /// Gets a reference into this Gc without checking if pointer is still
    /// alive or has unique access
    ///
    /// # Safety
    /// - This gc pointer must not have had its inner data dropped yet.
    /// - There must be no other aliasing references to the inner data.
    pub unsafe fn get_mut_unchecked(this: &mut Gc<T>) -> &mut T {
        &mut this.ptr.as_mut().data
    }

    /// Returns true if both this & other point to the same allocation.
    pub fn ptr_eq(this: &Gc<T>, other: &Gc<T>) -> bool {
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

    fn get_inner(&self) -> &Inner<T> {
        // SAFETY: Barring bugs in the collector, self.ptr is not dangling and not null.
        unsafe { self.ptr.as_ref() }
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
        unsafe {
            if self.ptr.as_ref().strong_count() == NonZeroUsize::new(1).unwrap() {
                // This is the last remaining strong ref to this value so we can
                // safely drop the inner value and de-allocate the container.

                // The collector is the only code which marks node as zombies, and it does so
                // _after_ it has finished processing the node. We can safely de-allocate here
                // without causing a double free.
                //
                // Note that we _must not_ attempt to drop the data, since it was already
                // dropped when the node was marked dead.
                Inner::zombie_safe_drop_data_dealloc(self.ptr);
            } else if self.ptr.as_ref().status.get() == Status::Dead {
                self.ptr.as_ref().decrement_strong_count();
            } else {
                // Indicate that it might be the root of a cycle.
                self.ptr.as_ref().mark_ref_dropped();
                if self.ptr.as_ref().buffered.replace(true) {
                    // Already tracked for possible cyclical deletion, we can drop a strong
                    // reference here without fear of early deletion.
                    self.ptr.as_ref().decrement_strong_count();
                } else {
                    // Convert it to an unsized generic type
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
                }
            }
        }
    }
}

impl<T: Traceable + 'static> PartialEq for Gc<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: Traceable + 'static> Eq for Gc<T> where T: Eq {}

impl<T: Traceable + 'static> PartialOrd for Gc<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl<T: Traceable + 'static> Ord for Gc<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T> AsRef<T> for Gc<T>
where
    T: Traceable + 'static,
{
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T> Deref for Gc<T>
where
    T: Traceable + 'static,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        assert!(Self::is_live(self));

        // SAFETY: We asserted that self is live.
        unsafe { Self::get_unchecked(self) }
    }
}

struct Inner<T: Traceable + ?Sized> {
    strong: Cell<NonZeroUsize>,
    status: Cell<Status>,
    buffered: Cell<bool>,
    data: ManuallyDrop<T>,
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
                &format!(
                    "{} @ {:?}",
                    std::any::type_name::<T>(),
                    (&*self.data as *const T)
                ),
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
    /// - ptr.data must not have any aliasing references.
    unsafe fn drop_data(ptr: NonNull<Self>) {
        ManuallyDrop::drop(&mut (*ptr.as_ptr()).data)
    }

    /// # Safety:
    /// - ptr must not have been deallocated.
    /// - ptr must have been created by Box::into_raw/leak
    unsafe fn dealloc(ptr: NonNull<Self>) {
        debug_assert_eq!(ptr.as_ref().strong.get().get(), 1);

        let boxed = Box::from_raw(ptr.as_ptr());
        drop(boxed)
    }

    unsafe fn zombie_safe_drop_data_dealloc(ptr: NonNull<Self>) {
        if ptr.as_ref().status.get() != Status::Dead {
            Self::drop_data(ptr)
        }

        Self::dealloc(ptr);
    }

    fn strong_count(&self) -> NonZeroUsize {
        self.strong.get()
    }

    fn increment_strong_count(&self) {
        self.strong.set(
            self.strong
                .get()
                .get()
                .checked_add(1)
                .and_then(NonZeroUsize::new)
                .unwrap(),
        )
    }

    /// # Safety:
    /// - The caller of this function must not attempt to access self after calling this function.
    unsafe fn decrement_strong_count(&self) {
        self.strong
            .set(NonZeroUsize::new(self.strong.get().get() - 1).unwrap())
    }

    /// # Safety:
    /// - The caller of this function must not attempt to access self after calling this function.
    unsafe fn unbuffer_from_collector(&self) {
        self.buffered.set(false);
        self.decrement_strong_count();
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

    fn mark_dead(&self) {
        self.status.set(Status::Dead);
    }

    fn mark_ref_dropped(&self) {
        if self.status.get() == Status::Live {
            self.status.set(Status::RecentlyDecremented);
        }
    }
}
