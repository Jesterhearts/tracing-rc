use std::{
    cell::Cell,
    marker::PhantomData,
    mem::ManuallyDrop,
    ops::Deref,
    ptr::NonNull,
};

pub mod collector;
pub mod traceable;

use collector::Status;
pub use collector::{
    collect,
    collect_full,
    collect_with_options,
    CollectionType,
    GcVisitor,
};
pub use traceable::Traceable;

use crate::collector::{
    Node,
    OLD_GEN,
    YOUNG_GEN,
};

struct Inner<T: Traceable + ?Sized> {
    strong: Cell<usize>,
    status: Cell<Status>,
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
    /// - ptr must not have any aliasing references.
    unsafe fn drop_data(mut ptr: NonNull<Self>) {
        ManuallyDrop::drop(&mut ptr.as_mut().data)
    }

    /// # Safety:
    /// - ptr must not have been deallocated.
    /// - ptr must have been created by Box::into_raw/leak
    unsafe fn dealloc(ptr: NonNull<Self>) {
        let boxed = Box::from_raw(ptr.as_ptr());
        drop(boxed)
    }

    unsafe fn drop_data_dealloc(ptr: NonNull<Self>) {
        Self::drop_data(ptr);
        Self::dealloc(ptr);
    }

    fn strong_count(&self) -> usize {
        self.strong.get()
    }

    fn increment_strong_count(&self) {
        self.strong.set(self.strong.get().checked_add(1).unwrap())
    }

    fn decrement_strong_count(&self) {
        self.strong.set(self.strong.get() - 1)
    }

    fn is_live(&self) -> bool {
        matches!(
            self.status.get(),
            Status::Live | Status::RecentlyDecremented
        )
    }

    /// # Safety: The inner data must not have been dropped or the node must be unreachable through
    /// safe code.
    unsafe fn force_live(&self) {
        self.status.set(Status::Live);
    }

    fn mark_live(&self) {
        if self.status.get() == Status::RecentlyDecremented {
            self.status.set(Status::Live);
        }
    }

    fn mark_dead(&self) {
        if self.status.get() != Status::Zombie {
            self.status.set(Status::Dead);
        }
    }

    fn mark_zombie(&self) {
        self.status.set(Status::Zombie);
    }

    fn mark_ref_dropped(&self) {
        if self.status.get() == Status::Live {
            self.status.set(Status::RecentlyDecremented);
        }
    }
}

#[derive(Debug)]
pub struct GcPtr<T: Traceable + 'static> {
    ptr: NonNull<Inner<T>>,
    _t: PhantomData<T>,
}

impl<T> Traceable for GcPtr<T>
where
    T: Traceable + 'static,
{
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        visitor(self.node())
    }
}

impl<T> GcPtr<T>
where
    T: Traceable + 'static,
{
    pub fn new(data: T) -> Self {
        Self {
            ptr: Box::leak(Box::new(Inner {
                strong: Cell::new(1),
                status: Cell::new(Status::Live),
                data: ManuallyDrop::new(data),
            }))
            .into(),
            _t: PhantomData,
        }
    }

    pub fn strong_count(ptr: &Self) -> usize {
        unsafe { ptr.ptr.as_ref().strong_count() }
    }

    /// Get a reference into this GcPointer.
    ///
    /// Returns None if the pointer is dead, as the data contained in this
    /// pointer is possibly cleaned up.
    pub fn get(&self) -> Option<&T> {
        unsafe {
            if self.ptr.as_ref().is_live() {
                Some(self.as_ref())
            } else {
                None
            }
        }
    }

    /// Gets a reference into this GcPtr without checking if pointer is still
    /// alive.
    ///
    /// # Safety
    /// This gc pointer must not have had its inner data dropped yet
    pub unsafe fn get_unchecked(&self) -> &T {
        &self.ptr.as_ref().data
    }

    pub fn node(&self) -> Node {
        unsafe {
            Node {
                inner_ptr: self.coerce_inner(),
            }
        }
    }

    unsafe fn coerce_inner(&self) -> NonNull<Inner<dyn Traceable>> {
        // SAFETY: self.ptr is NonNull
        NonNull::new_unchecked(self.ptr.as_ptr() as *mut Inner<dyn Traceable>)
    }
}

impl<T: Traceable + 'static> PartialEq for GcPtr<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr || unsafe { self.ptr.as_ref().data == other.ptr.as_ref().data }
    }
}

impl<T: Traceable + 'static> Eq for GcPtr<T> where T: Eq {}

impl<T: Traceable + 'static> PartialOrd for GcPtr<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        unsafe { self.ptr.as_ref().data.partial_cmp(&other.ptr.as_ref().data) }
    }
}

impl<T: Traceable + 'static> Ord for GcPtr<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { self.ptr.as_ref().data.cmp(&other.ptr.as_ref().data) }
    }
}

impl<T> Clone for GcPtr<T>
where
    T: Traceable + 'static,
{
    fn clone(&self) -> Self {
        let inner = unsafe { self.ptr.as_ref() };

        // Technically mark_live is safe to call without this check, but it's going to get
        // inlined anyways and this makes things a little more explicit.
        if inner.is_live() {
            inner.mark_live();
        }

        inner.increment_strong_count();

        Self {
            ptr: self.ptr,
            _t: PhantomData,
        }
    }
}

impl<T> AsRef<T> for GcPtr<T>
where
    T: Traceable + 'static,
{
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T> Deref for GcPtr<T>
where
    T: Traceable + 'static,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            assert!(self.ptr.as_ref().is_live());
            &self.ptr.as_ref().data
        }
    }
}

impl<T: Traceable + 'static> Drop for GcPtr<T> {
    fn drop(&mut self) {
        unsafe {
            self.ptr.as_ref().decrement_strong_count();

            if self.ptr.as_ref().status.get() == Status::Dead {
                // The collector already knows about this and will handle dropping its internal
                // data & deallocating anything needed.
                return;
            }

            if self.ptr.as_ref().strong_count() == 0 {
                // This has dropped to zero refs and is not stored in the collector. We can
                // safely drop the inner value and de-allocate the container.

                if self.ptr.as_ref().status.get() == Status::Zombie {
                    // The collector is the only code which marks node as zombies, and it does so
                    // _after_ it has finished processing the node. We can safely de-allocate here
                    // without causing a double free.
                    //
                    // Note that we _must not_ attempt to drop the data, since it was already
                    // dropped when the node was marked dead.
                    Inner::dealloc(self.ptr)
                } else {
                    debug_assert_eq!(self.ptr.as_ref().status.get(), Status::Live);

                    // We know the node is alive and hasn't had its inner data dropped yet, so we
                    // can safely drop & deallocate the data.
                    Inner::drop_data_dealloc(self.ptr);
                }
            } else {
                // Indicate that it might be the root of a cycle.
                self.ptr.as_ref().mark_ref_dropped();

                // Convert it to an unsized generic type
                let ptr = self.coerce_inner();

                if YOUNG_GEN.with(|gen| gen.borrow().contains_key(&ptr))
                    || OLD_GEN.with(|gen| gen.borrow().contains(&ptr))
                {
                    // Already tracked for possible cyclical deletion
                    return;
                }

                // It's possible that this will turn out to be a member of a cycle, so we need
                // to add it to the list of items the collector tracks.
                YOUNG_GEN.with(|gen| {
                    debug_assert!(OLD_GEN.with(|gen| !gen.borrow().contains(&ptr)));

                    let mut gen = gen.borrow_mut();
                    debug_assert!(!gen.contains_key(&ptr));

                    // The generation will handle deletion, so we need to bump the strong count by
                    // one to prevent early deletion.
                    ptr.as_ref().increment_strong_count();
                    gen.insert(ptr, 0);
                });
            }
        }
    }
}
