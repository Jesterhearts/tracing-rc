use std::{
    cell::{
        Cell,
        RefCell,
    },
    mem::ManuallyDrop,
    num::NonZeroUsize,
    ops::Deref,
    ptr::NonNull,
};

use indexmap::{
    IndexMap,
    IndexSet,
};

#[derive(Debug)]
pub struct Node {
    inner_ptr: NonNull<Inner<dyn Traceable>>,
}

pub type GcVisitor<'cycle> = dyn FnMut(Node) + 'cycle;

/// Must be implemented for any value which will be stored inside of a GcPtr.
///
/// While this is implemented for many of Rust's basic types, it's not
/// recommended that you store them in a GcPtr, as there is still a real
/// cost to doing so. You're probably better off using std::rc.
pub trait Traceable {
    /// Visit the gc pointers owned by this type or its decendents.
    ///
    /// This function is marked unsafe as improper implementation can lead to
    /// undefined behavior.
    ///
    /// # Safety
    /// - You MUST NOT report GcPtrs owned by the inner contents of GcPtrs.
    /// ```
    /// use tracing_rc::{
    ///     GcPtr,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// struct MyStruct {
    ///     ptr: GcPtr<MyStruct>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor(self.ptr.node());
    ///
    ///         // This is bad and will cause undefined behavior
    ///         // visitor(self.ptr.ptr.node());
    ///     }
    /// }
    /// ```
    /// - You MUST NOT report a unique GcPtr instance twice.
    /// ```
    /// use tracing_rc::{
    ///     GcPtr,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// struct MyStruct {
    ///     ptr: GcPtr<usize>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor(self.ptr.node());
    ///
    ///         // This is bad and will cause undefined behavior
    ///         // visitor(self.ptr.node());
    ///     }
    /// }
    /// ```
    /// - You MUST NOT report GcPtrs that are not owned by your object or its decendents.
    ///     - It is acceptable skip reporting, although doing so will result in memory leaks.
    /// ```
    /// use tracing_rc::{
    ///     GcPtr,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// thread_local! { static GLOBAL_PTR: GcPtr<usize> = GcPtr::new(10)}
    ///
    /// struct MyStruct {
    ///     ptr: GcPtr<MyStruct>,
    ///     leaks: GcPtr<usize>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor(self.ptr.node());
    ///
    ///         // Leaving this line commented out will leak, which is safe.
    ///         // Uncommenting it is safe and will allow leaks to be cleaned up.
    ///         // visitor(self.leaks.node());
    ///
    ///         // This is bad and will cause undefined behavior
    ///         // GLOBAL_PTR.with(|ptr| visitor(ptr.node()));
    ///     }
    /// }
    /// ```
    unsafe fn visit_children(&self, visitor: &mut GcVisitor);
}

/// This is safe to use, as it does not visit any children and thust cannot
/// cause a pointer to be dropped erroneously. It will cause memory leaks if it
/// is used to implement tracing on a type which ends up participating in a
/// cycle.
///
/// Despite its safety, you probably don't want to implement this anyways, since
/// if you know that your type cannot participate in a cycle, you should
/// probably just use std::rc.
macro_rules! empty_traceable {
    ($t:ident) => {
        impl Traceable for $t {
            unsafe fn visit_children(&self, _: &mut GcVisitor) {}
        }
    };
    ($first:ident, $($rest:ident),+) => {
        empty_traceable!($first);
        empty_traceable!($($rest),+);
    };
}

empty_traceable!(f32, f64);
empty_traceable!(i8, i16, i32, i64, isize, i128);
empty_traceable!(u8, u16, u32, u64, usize, u128);
empty_traceable!(bool, char);
empty_traceable!(String);

/// Controls the style of collection carried out.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionType {
    /// Do a simple pass over the young gen, collecting non-cyclical pointers
    /// and moving old pointers to the old gen. Then perform a cycle-tracing
    /// collection over the old gen.
    Default,
    /// Only run collection for the young gen and ignore the old gen completely.
    YoungOnly,
    /// Move all pointers from young gen to old gen and perform a full
    /// cycle-tracing collection.
    Full,
}

impl Default for CollectionType {
    fn default() -> Self {
        Self::Default
    }
}

const OLD_GEN_THRESHOLD: usize = 10;

#[cfg(debug_assertions)]
thread_local! { static REVERSE_COLLECTION: Cell<bool> = Cell::new(true) }

#[inline]
fn reverse_collection() -> bool {
    #[cfg(debug_assertions)]
    return REVERSE_COLLECTION.with(|rev| rev.get());

    #[cfg(not(debug_assertions))]
    return false;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    Live,
    RecentlyDecremented,
    Dead,
    Zombie,
}

thread_local! { static OLD_GEN: RefCell<IndexSet<NonNull<Inner<dyn Traceable>>>> = RefCell::new(IndexSet::default()) }
thread_local! { static YOUNG_GEN: RefCell<IndexMap<NonNull<Inner<dyn Traceable>>, usize>> = RefCell::new(IndexMap::default()) }

pub(crate) fn count_roots() -> usize {
    OLD_GEN.with(|old| old.borrow().len()) + YOUNG_GEN.with(|young| young.borrow().len())
}

// Debugging utility.
#[allow(dead_code)]
fn visit_all_roots(visitor: &mut dyn FnMut(&Inner<dyn Traceable>)) {
    unsafe {
        YOUNG_GEN.with(|young| {
            for (root, _) in young.borrow().iter() {
                visitor(root.as_ref())
            }
        });
        OLD_GEN.with(|old| {
            for root in old.borrow().iter() {
                visitor(root.as_ref())
            }
        });
    }
}

/// Perform a full, cycle-tracing collection of both the old & young gen.
pub fn collect_full() {
    collect_with_options(CollectionType::Full)
}

/// Perform a normal collection cycle.
pub fn collect() {
    collect_with_options(CollectionType::default())
}

/// Perform a collection cycle based on `kind`.
pub fn collect_with_options(kind: CollectionType) {
    collect_new_gen(kind);
    if kind != CollectionType::YoungOnly {
        unsafe { collect_old_gen() };
    }
}

fn collect_new_gen(kind: CollectionType) {
    OLD_GEN.with(|old_gen| {
        let mut old_gen = old_gen.borrow_mut();
        YOUNG_GEN.with(|gen| {
            gen.borrow_mut().retain(|ptr, generation| {
                unsafe {
                    assert_ne!(ptr.as_ref().strong_count(), 0);
                    assert_ne!(ptr.as_ref().status.get(), Status::Dead);

                    if ptr.as_ref().strong_count() == 1 {
                        // This generation is the last remaining owner of the pointer, so we can
                        // safely drop it. It's not possible from safe code for another reference to
                        // this pointer to be generated during drop.
                        Inner::drop_data_dealloc(*ptr);
                        return false;
                    }
                }

                if kind != CollectionType::Full {
                    *generation += 1;

                    if *generation < OLD_GEN_THRESHOLD {
                        // Not old enough yet to bother with cycle detection.
                        return true;
                    }
                }

                // Move it to the old gen
                old_gen.insert(*ptr);
                false
            })
        });
    });
}

unsafe fn trace_children(
    ptr: NonNull<Inner<dyn Traceable>>,
    traced_nodes: &mut IndexMap<NonNull<Inner<dyn Traceable>>, NonZeroUsize>,
) {
    ptr.as_ref().data.visit_children(&mut |node| {
        let ptr = node.inner_ptr;
        match traced_nodes.entry(ptr) {
            indexmap::map::Entry::Occupied(mut known) => {
                // We've already seen this node. We do a saturating add because it's ok to
                // undercount references here and usize::max references is kind of a degenerate
                // case.
                *known.get_mut() = NonZeroUsize::new_unchecked(known.get().get().saturating_add(1));
            }
            indexmap::map::Entry::Vacant(new) => {
                // Visiting the children of this pointer may cause a
                // reference to it to be dropped, so we must increment the
                // strong count here.
                ptr.as_ref().increment_strong_count();

                // We haven't yet visited this pointer, add it to the list
                // of seen pointers. We set the initial refcount to 2 here
                // because we've seen it once and we know our traced nodes
                // set contains it with one strong ref.
                new.insert(NonZeroUsize::new_unchecked(2));

                match ptr.as_ref().status.get() {
                    Status::Live | Status::RecentlyDecremented => {
                        trace_children(ptr, traced_nodes);
                    }
                    Status::Zombie => {
                        // This is reachable if a previously dead node was
                        // resurrected during a drop and stored as a child
                        // of a cycle. Its inner data is dropped, so we
                        // can't touch it.
                    }
                    Status::Dead => {
                        panic!(
                            "Dead node {:#?} @ {:?} in reachable object graph",
                            ptr.as_ref(),
                            ptr
                        );
                    }
                }
            }
        }
    });
}

unsafe fn mark_live(
    inner: &Inner<dyn Traceable>,
    traced_nodes: &mut IndexMap<NonNull<Inner<dyn Traceable>>, NonZeroUsize>,
) {
    inner.force_live();

    inner.data.visit_children(&mut |node| {
        traced_nodes.remove(&node.inner_ptr);

        let node = node.inner_ptr.as_ref();

        match node.status.get() {
            Status::Live => {
                // Already visited & marked live.
            }
            Status::Zombie => {
                // The node has been previously torn down, we can't really resurrect it
                // here or we cause undefined behavior depending on the constraints of the inner
                // type. Thankfully Zombie nodes cannot cause cycles (or if they do,
                // there's no way to clean them up).
            }
            Status::Dead => {
                // The node is part of a cycle and would be dead if the live root that's
                // holding onto it were to die, but it's not actually dead.
                // Resurrect it.
                mark_live(node, traced_nodes);
            }
            Status::RecentlyDecremented => {
                // The node was given to the collector because we thought it might be
                // dead, but it's not, yay!
                mark_live(node, traced_nodes);
            }
        }
    });
}

unsafe fn mark(
    ptr: NonNull<Inner<dyn Traceable>>,
    traced_nodes: &mut IndexMap<NonNull<Inner<dyn Traceable>>, NonZeroUsize>,
) {
    let refs = if let Some(refs) = traced_nodes.remove(&ptr) {
        // We visited this node durring tracing and need to possibly mark it as dead.
        refs.get()
    } else {
        // We have either already marked this node as dead or live or it is a brand new node
        // created during tracing.
        //
        // If we already marked it dead, it'll need to be resurrected by a reachable live node. If
        // it was created during tracing it's either already in our dead graph and will get cleaned
        // up normally, or there's a real reference outside that can reach it and we'll clean it
        // up later. Or it's leaked, but there's only so much we can do here.
        return;
    };

    if ptr.as_ref().strong_count() > refs {
        // There is at least one non-cyclical ref, mark the node & all decendents as
        // live
        mark_live(ptr.as_ref(), traced_nodes);
    } else if ptr.as_ref().is_live() {
        // The node hasn't been marked dead yet and is not a zombie, so we need to mark
        // it dead. Remember that we can't visit the children of
        // zombie nodes.
        ptr.as_ref().mark_dead();

        // The node was part of a cycle, we can handle cleanup at a later stage.
        // For now we need to visit its children because even a dead cycle may have
        // outward bound edges to non-dead nodes and we need to mark them as live if so.
        ptr.as_ref().data.visit_children(&mut |node| {
            mark(node.inner_ptr, traced_nodes);
        });
    }
}

unsafe fn collect_old_gen() {
    let mut candidate_nodes = OLD_GEN.with(|old_gen| {
        let mut old_gen = old_gen.borrow_mut();

        old_gen
            .drain(..)
            .collect::<IndexSet<NonNull<Inner<dyn Traceable>>>>()
    });

    let mut traced_nodes = candidate_nodes
        .iter()
        .map(|ptr| {
            (
                *ptr,
                // initially 1, since all items in the collector get a strong reference
                NonZeroUsize::new_unchecked(1),
            )
        })
        .collect();

    // Iterate over all nodes reachable from the old gen tracking them in the list of all
    // traced nodes.
    for ptr in candidate_nodes.iter() {
        match ptr.as_ref().status.get() {
            Status::Live | Status::Zombie => {
                // We'll collect it later if it a new strong reference was created and added
                // to a now dead cycle before being collected. We don't really have any work
                // to do here otherwise.
            }
            Status::Dead => {
                // This should be unreachable. The only way for nodes to reach the old gen
                // is through the GcPtr drop implementation, which does not add nodes to the
                // collector if they are dead.
                panic!("Dead node {:#?} @ {:?} in old gen", ptr.as_ref(), ptr);
            }
            Status::RecentlyDecremented => {
                // This node had a strong ref dropped recently and might form a cycle, trace
                // it
                trace_children(*ptr, &mut traced_nodes);
            }
        }
    }

    candidate_nodes.extend(traced_nodes.keys().copied());

    for ptr in candidate_nodes.iter() {
        mark(*ptr, &mut traced_nodes);
    }

    if reverse_collection() {
        candidate_nodes.reverse();
    }

    // Drop the dead node data. Because the drop implementations may run arbitrary code, it is
    // possible that zombie nodes will get created post-drop.
    candidate_nodes.retain(|ptr| {
        // At this point, nodes in our candidate list marked dead are _definitely dead_, so we can
        // go ahead and drop them. During drop, new refs might get added to dead nodes, so we will
        // pass back over the list we dropped and double-check their refcounts before actually
        // de-allocating them.
        if ptr.as_ref().status.get() == Status::Dead {
            Inner::drop_data(*ptr);
            true
        } else {
            // Remember that when we added a new node to our old generation or our traced list, we
            // bumped the refcount to keep it alive. We must decrement the strong count here to
            // prevent a leak.
            ptr.as_ref().decrement_strong_count();
            false
        }
    });

    // De-allocate the dropped nodes. The refcount must be checked here as someone
    // may have stashed a copy during drop.
    for ptr in candidate_nodes.into_iter() {
        debug_assert_eq!(ptr.as_ref().status.get(), Status::Dead);

        // Remember that when we added nodes to our traced list, we bumped the refcount to keep
        // them alive.
        ptr.as_ref().decrement_strong_count();

        // If the strong count is 0, candidate_nodes was the last owner of the data and we can
        // safely de-allocate its memory.
        if ptr.as_ref().strong_count() == 0 {
            Inner::dealloc(ptr);
        } else {
            // Please don't do necromancy
            ptr.as_ref().mark_zombie();
        }
    }
}

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
    unsafe fn drop_data(ptr: NonNull<Self>) {
        ManuallyDrop::drop(&mut (*ptr.as_ptr()).data);
    }

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
}

impl<T> GcPtr<T>
where
    T: Traceable + 'static,
{
    pub fn new(data: T) -> Self {
        unsafe {
            Self {
                ptr: NonNull::new_unchecked(Box::into_raw(Box::new(Inner {
                    strong: Cell::new(1),
                    status: Cell::new(Status::Live),
                    data: ManuallyDrop::new(data),
                }))),
            }
        }
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
    /// This gc pointer must not have had its inner data dropped yet.
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
        NonNull::from(self.ptr.as_ref() as &Inner<dyn Traceable>)
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
        unsafe {
            let inner = self.ptr.as_ref();

            // Technically mark_live is safe to call without this check, but it's going to get
            // inlined anyways and this makes things a little more explicit.
            if inner.is_live() {
                inner.mark_live();
            }

            inner.increment_strong_count();

            Self { ptr: self.ptr }
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

#[cfg(test)]
mod test {
    use std::cell::RefCell;

    use pretty_assertions::assert_eq;

    use crate::{
        collect_full,
        collect_with_options,
        count_roots,
        CollectionType,
        GcPtr,
        GcVisitor,
        Traceable,
    };

    #[test]
    fn acyclic_single_no_garbage() {
        let a = GcPtr::new(10);

        drop(a);
        assert_eq!(count_roots(), 0);
    }

    #[test]
    fn acyclic_chain_no_garbage() {
        struct Int {
            i: GcPtr<u32>,
        }
        impl Traceable for Int {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                visitor(self.i.node())
            }
        }

        let a = GcPtr::new(Int { i: GcPtr::new(10) });

        drop(a);
        assert_eq!(
            count_roots(),
            0,
            "Added trivially acyclic node to root list"
        );
    }

    #[test]
    fn acyclic_tree_young_gen_collects() {
        struct Int {
            i: GcPtr<u32>,
        }
        impl Traceable for Int {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                visitor(self.i.node())
            }
        }

        let a = GcPtr::new(10u32);
        let b = GcPtr::new(Int { i: a.clone() });
        let c = GcPtr::new(Int { i: a.clone() });

        drop(a);

        assert_eq!(*b.i, 10);
        assert_eq!(*c.i, 10);

        assert_eq!(
            count_roots(),
            1,
            "Possibly cyclic node a not added to root list"
        );

        drop(b);

        assert_eq!(count_roots(), 1, "Acyclic node b added to root list");

        drop(c);

        assert_eq!(count_roots(), 1, "Acyclic node c added to root list");

        collect_with_options(CollectionType::YoungOnly);

        assert_eq!(count_roots(), 0, "Failed to cleanup node a");
    }

    #[test]
    fn mono_simple_cycle() {
        struct Cycle {
            gc: Option<GcPtr<RefCell<Cycle>>>,
        }

        impl Traceable for RefCell<Cycle> {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                if let Some(gc) = &self.borrow().gc {
                    visitor(gc.node())
                }
            }
        }

        let a = GcPtr::new(RefCell::new(Cycle { gc: None }));
        a.borrow_mut().gc = Some(a.clone());

        unsafe {
            assert_eq!(
                a.ptr.as_ref().strong_count(),
                2,
                "Node a strong count not incremented"
            );
        }

        drop(a);

        assert_eq!(count_roots(), 1, "Possibly cyclic node a not in roots");

        collect_full();

        assert_eq!(count_roots(), 0);
    }

    #[test]
    fn simple_cycle() {
        struct Cycle {
            gc: Option<GcPtr<RefCell<Cycle>>>,
        }

        impl Traceable for RefCell<Cycle> {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                if let Some(gc) = &self.borrow().gc {
                    visitor(gc.node())
                }
            }
        }

        // A.ptr -> Null
        // A ref 1
        let a = GcPtr::new(RefCell::new(Cycle { gc: None }));

        // B.ptr -> A
        // B ref 1, A ref 2
        let b = GcPtr::new(RefCell::new(Cycle {
            gc: Some(a.clone()),
        }));

        // A.ptr -> B
        // B ref 2, A ref 2
        a.borrow_mut().gc = Some(b.clone());

        unsafe {
            assert_eq!(
                a.ptr.as_ref().strong_count(),
                2,
                "Node a strong count not incremented"
            );

            assert_eq!(
                b.ptr.as_ref().strong_count(),
                2,
                "Node b strong count not incremented"
            );
        }

        drop(b);

        assert_eq!(count_roots(), 1, "Node b not tracked in roots");

        unsafe {
            assert_eq!(
                a.ptr.as_ref().strong_count(),
                2,
                "Node a strong count decremented by drop of child"
            );

            assert_eq!(
                a.borrow().gc.as_ref().unwrap().ptr.as_ref().strong_count(),
                2,
                "Node b strong count not properly handled after drop"
            );
        }

        drop(a);

        assert_eq!(count_roots(), 2, "Node a not tracked in roots");

        collect_full();

        assert_eq!(
            count_roots(),
            0,
            "Cycle not cleaned up and removed from list"
        );
    }

    #[test]
    fn dead_cycle_live_outbound() {
        struct Lives;

        impl Lives {
            #[inline(never)]
            fn alive(&self) -> bool {
                true
            }
        }

        impl Traceable for Lives {
            unsafe fn visit_children(&self, _: &mut GcVisitor) {}
        }

        struct Cycle {
            gc: Option<GcPtr<RefCell<Cycle>>>,
            edge: GcPtr<Lives>,
        }

        impl Traceable for RefCell<Cycle> {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                if let Some(gc) = &self.borrow().gc {
                    visitor(gc.node())
                }
                visitor(self.borrow().edge.node());
            }
        }

        let lives = GcPtr::new(Lives);

        // A.ptr -> Null
        // A ref 1, Lives ref 2
        let a = GcPtr::new(RefCell::new(Cycle {
            gc: None,
            edge: lives.clone(),
        }));

        // B.ptr -> A
        // B ref 1, A ref 2, Lives ref 3
        let b = GcPtr::new(RefCell::new(Cycle {
            gc: Some(a.clone()),
            edge: lives.clone(),
        }));

        // A.ptr -> B
        // B ref 2, A ref 2, Lives ref 3
        a.borrow_mut().gc = Some(b.clone());

        assert_eq!(
            unsafe { lives.ptr.as_ref().strong_count() },
            3,
            "Live node missed a refcount"
        );

        drop(b);

        // B hasn't been cleand up yet, so we should still have its strong refs to 3
        assert_eq!(
            unsafe { lives.ptr.as_ref().strong_count() },
            3,
            "Drop of cyclical parent b node decremented live refcount"
        );

        assert_eq!(count_roots(), 1, "Node b not in list of roots");

        drop(a);

        // A hasn't been cleand up yet, so we should still have its strong refs to 3
        assert_eq!(
            unsafe { lives.ptr.as_ref().strong_count() },
            3,
            "Drop of cyclical parent a node decremented live refcount"
        );

        assert_eq!(count_roots(), 2, "Node a not in list of roots");

        // Collect a & b, this will re-add live for possibly collection.
        collect_full();
        // Make sure we clean up the new & old gens for the last live reference.
        // This shouldn't drop it, just remove it from the list of possible cycles.
        collect_full();

        assert_eq!(count_roots(), 0, "Not all roots were collected");

        assert!(lives.alive());
    }

    #[test]
    fn dead_cycle_dead_outbound() {
        struct Dead;

        impl Traceable for Dead {
            unsafe fn visit_children(&self, _: &mut GcVisitor) {}
        }

        struct Cycle {
            gc: Option<GcPtr<RefCell<Cycle>>>,
            edge: GcPtr<Dead>,
        }

        impl Traceable for RefCell<Cycle> {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                if let Some(gc) = &self.borrow().gc {
                    visitor(gc.node())
                }
                visitor(self.borrow().edge.node());
            }
        }

        let dies = GcPtr::new(Dead);

        let a = GcPtr::new(RefCell::new(Cycle {
            gc: None,
            edge: dies.clone(),
        }));

        let b = GcPtr::new(RefCell::new(Cycle {
            gc: Some(a.clone()),
            edge: dies,
        }));

        a.borrow_mut().gc = Some(b.clone());

        drop(b);

        assert_eq!(count_roots(), 1, "Node b not in list of roots");

        drop(a);

        assert_eq!(count_roots(), 2, "Node a not in list of roots");

        collect_full();

        assert_eq!(count_roots(), 0, "Not all roots were collected");
    }

    #[test]
    fn cycle_live_inbound() {
        struct Cycle {
            gc: Option<GcPtr<RefCell<Cycle>>>,
        }

        impl Traceable for RefCell<Cycle> {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                if let Some(gc) = &self.borrow().gc {
                    visitor(gc.node())
                }
            }
        }

        let a = GcPtr::new(RefCell::new(Cycle { gc: None }));

        let b = GcPtr::new(RefCell::new(Cycle {
            gc: Some(a.clone()),
        }));

        a.borrow_mut().gc = Some(b.clone());

        drop(b);

        assert_eq!(count_roots(), 1, "Node b not tracked in roots");

        // We don't expect anything to be cleaned up yet, since A is still live
        collect_full();
        assert_eq!(
            count_roots(),
            0,
            "Live node b not removed from list of roots"
        );

        // This is trivially true, but it makes it so that miri will yell if we're
        // touching anything dead.
        assert!(a.borrow().gc.as_ref().unwrap().borrow().gc.is_some());
        drop(a);

        assert_eq!(count_roots(), 1, "Node a not tracked in roots");

        collect_full();

        assert_eq!(
            count_roots(),
            0,
            "Cycle not cleaned up and removed from list"
        );
    }

    #[test]
    fn mono_cycle_live_inbound() {
        struct Cycle {
            gc: Option<GcPtr<RefCell<Cycle>>>,
        }

        impl Traceable for RefCell<Cycle> {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                if let Some(gc) = &self.borrow().gc {
                    visitor(gc.node())
                }
            }
        }

        let a = GcPtr::new(RefCell::new(Cycle { gc: None }));
        a.borrow_mut().gc = Some(a.clone());

        let last = a.clone();

        drop(a);

        assert_eq!(count_roots(), 1, "Node a not tracked in roots");

        // We don't expect anything to be cleaned up yet, since last is still live
        collect_full();
        assert_eq!(
            count_roots(),
            0,
            "Live node a not removed from list of roots"
        );

        // This is trivially true, but it makes it so that miri will yell if we're
        // touching anything dead.
        assert!(last.borrow().gc.is_some());
        drop(last);

        assert_eq!(count_roots(), 1, "Node last not tracked in roots");

        collect_full();

        assert_eq!(
            count_roots(),
            0,
            "Cycle not cleaned up and removed from list"
        );
    }

    #[test]
    fn mono_cycle_live_outbound() {
        struct Cycle {
            gc: Option<GcPtr<RefCell<Cycle>>>,
            edge: GcPtr<u32>,
        }

        impl Traceable for RefCell<Cycle> {
            unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                if let Some(gc) = &self.borrow().gc {
                    visitor(gc.node());
                }
                visitor(self.borrow().edge.node());
            }
        }

        let live = GcPtr::new(10);

        let a = GcPtr::new(RefCell::new(Cycle {
            gc: None,
            edge: live.clone(),
        }));
        a.borrow_mut().gc = Some(a.clone());

        let last = a.clone();

        drop(a);

        assert_eq!(count_roots(), 1, "Node a not tracked in roots");

        // We don't expect anything to be cleaned up yet, since last is still live
        collect_full();
        assert_eq!(
            count_roots(),
            0,
            "Live node a not removed from list of roots"
        );

        assert_eq!(*live, 10);
        drop(last);

        assert_eq!(count_roots(), 1, "Node last not tracked in roots");

        collect_full();
        collect_full();

        assert_eq!(
            count_roots(),
            0,
            "Cycle not cleaned up and removed from list"
        );

        // We should still have access to live
        assert_eq!(*live, 10);
    }

    /// There is no feasible way to prevent consumers of the library from
    /// resurrecting references to dead gc values in safe code, so we must not
    /// invoke undefined behavior if a consumer caches a gc pointer (directly or
    /// indirectly) during teardown.
    /// It is acceptable to leak memory in these cases, as it's rather unusual
    /// behavior, as long as we don't allow undefined behavior.
    mod necromancy {
        use std::cell::RefCell;

        use crate::{
            collect_full,
            GcPtr,
            GcVisitor,
            Traceable,
        };

        #[test]
        fn pure_gc_necromancy() {
            struct Zombie {
                cycle: RefCell<Option<GcPtr<Zombie>>>,
                dead: RefCell<Option<GcPtr<Mancer>>>,
            }

            impl Traceable for Zombie {
                unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                    visitor(self.cycle.borrow().as_ref().unwrap().node());
                    visitor(self.dead.borrow().as_ref().unwrap().node());
                }
            }

            thread_local! { static ZOMBIE: RefCell<Option<GcPtr<Zombie>>> = RefCell::new(None) };

            #[derive(Debug)]
            struct Necro {
                gc: RefCell<Option<GcPtr<Mancer>>>,
            }

            impl Drop for Necro {
                fn drop(&mut self) {
                    ZOMBIE.with(|zombie| {
                        *zombie.borrow_mut() = Some(GcPtr::new(Zombie {
                            cycle: RefCell::new(None),
                            dead: RefCell::new(Some(self.gc.borrow().as_ref().unwrap().clone())),
                        }));
                    })
                }
            }

            impl Traceable for Necro {
                unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                    visitor(self.gc.borrow().as_ref().unwrap().node());
                }
            }

            #[derive(Debug)]
            struct Mancer {
                gc: GcPtr<Necro>,
            }

            impl Traceable for Mancer {
                unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
                    visitor(self.gc.node());
                }
            }

            let necro = GcPtr::new(Necro {
                gc: RefCell::new(None),
            });
            let mancer = GcPtr::new(Mancer { gc: necro.clone() });
            *necro.gc.borrow_mut() = Some(mancer);

            drop(necro);

            collect_full();
            let mut resurrected_owner = None;
            ZOMBIE.with(|zombie| {
                resurrected_owner = zombie.borrow().clone();
                *zombie.borrow_mut() = None;
            });

            *resurrected_owner.as_ref().unwrap().cycle.borrow_mut() = resurrected_owner.clone();

            drop(resurrected_owner);

            collect_full();
        }
    }
}
