use std::{
    cell::RefCell,
    num::NonZeroUsize,
    ptr::NonNull,
};

use indexmap::{
    IndexMap,
    IndexSet,
};

use crate::{
    GcPtr,
    Inner,
    Traceable,
};

#[derive(Debug)]
pub struct Node {
    pub(crate) inner_ptr: NonNull<Inner<dyn Traceable>>,
}

pub struct GcVisitor<'cycle> {
    visitor: &'cycle mut dyn FnMut(Node),
}

impl GcVisitor<'_> {
    pub fn visit_node<T: Traceable>(&mut self, node: &GcPtr<T>) {
        (self.visitor)(node.node());
    }
}

/// Controls the style of collection carried out.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionType {
    /// Do a simple pass over the young gen, collecting non-cyclical pointers
    /// and moving old pointers to the old gen. Then perform a cycle-tracing
    /// collection over the old gen.
    Default,
    /// Only run collection for the young gen. This may still move pointers to the old gen if they
    /// qualify based on CollectOptions::old_gen_threshold
    YoungOnly,
}

impl Default for CollectionType {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CollectOptions {
    /// The number of times a pointer may be seen in the young gen before moving it to the old
    /// gen for a full tracing collection. Setting this to zero will cause all pointers to move to
    /// the old gen if they cannot be immediately cleaned up.
    pub old_gen_threshold: usize,
    pub kind: CollectionType,
}

impl CollectOptions {
    pub const DEFAULT: CollectOptions = CollectOptions {
        old_gen_threshold: 5,
        kind: CollectionType::Default,
    };
    pub const TRACE_AND_COLLECT_ALL: CollectOptions = Self::DEFAULT.set_old_gen_threshold(0);
    pub const YOUNG_ONLY: CollectOptions = Self::DEFAULT.set_kind(CollectionType::YoungOnly);

    pub const fn set_kind(self, kind: CollectionType) -> Self {
        let Self {
            old_gen_threshold,
            kind: _,
        } = self;

        Self {
            old_gen_threshold,
            kind,
        }
    }

    pub const fn set_old_gen_threshold(self, threshold: usize) -> Self {
        let Self {
            old_gen_threshold: _,
            kind,
        } = self;

        Self {
            old_gen_threshold: threshold,
            kind,
        }
    }
}

impl Default for CollectOptions {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Status {
    Live,
    RecentlyDecremented,
    Dead,
}

thread_local! { pub(crate) static OLD_GEN: RefCell<IndexSet<NonNull<Inner<dyn Traceable>>>> = RefCell::new(IndexSet::default()) }
thread_local! { pub(crate) static YOUNG_GEN: RefCell<IndexMap<NonNull<Inner<dyn Traceable>>, usize>> = RefCell::new(IndexMap::default()) }

#[doc(hidden)]
pub fn count_roots() -> usize {
    OLD_GEN.with(|old| old.borrow().len()) + YOUNG_GEN.with(|young| young.borrow().len())
}

#[doc(hidden)]
pub fn visit_all_roots(visitor: &mut dyn FnMut(Node)) {
    YOUNG_GEN.with(|young| {
        for (root, _) in young.borrow().iter() {
            visitor(Node { inner_ptr: *root })
        }
    });
    OLD_GEN.with(|old| {
        for root in old.borrow().iter() {
            visitor(Node { inner_ptr: *root })
        }
    });
}

/// Perform a full, cycle-tracing collection of both the old & young gen.
pub fn collect_full() {
    collect_with_options(CollectOptions::TRACE_AND_COLLECT_ALL)
}

/// Perform a normal collection cycle.
pub fn collect() {
    collect_with_options(CollectOptions::default())
}

/// Perform a collection cycle based on `CollectionOptions`.
pub fn collect_with_options(options: CollectOptions) {
    collect_new_gen(options);
    if options.kind != CollectionType::YoungOnly {
        collect_old_gen();
    }
}

fn collect_new_gen(options: CollectOptions) {
    OLD_GEN.with(|old_gen| {
        let mut old_gen = old_gen.borrow_mut();
        YOUNG_GEN.with(|gen| {
            gen.borrow_mut().retain(|ptr, generation| {
                unsafe {
                    if ptr.as_ref().strong_count() == NonZeroUsize::new(1).unwrap() {
                        // This generation is the last remaining owner of the pointer, so we can
                        // safely drop it. It's not possible from safe code for another reference to
                        // this pointer to be generated during drop.
                        // Depending on collector ordering, we can have zombie nodes in the young
                        // generation.
                        Inner::zombie_safe_drop_data_dealloc(*ptr);
                        return false;
                    }
                }

                if *generation < options.old_gen_threshold {
                    *generation += 1;
                    return true;
                }

                old_gen.insert(*ptr);
                false
            })
        });
    });
}

fn collect_old_gen() {
    let candidate_nodes = OLD_GEN.with(|old_gen| {
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
                NonZeroUsize::new(1).unwrap(),
            )
        })
        .collect();

    // Iterate over all nodes reachable from the old gen tracking them in the list of all
    // traced nodes.
    for ptr in candidate_nodes {
        let inner = unsafe { ptr.as_ref() };
        match inner.status.get() {
            Status::Live | Status::Dead => {
                // We'll collect it later if it a new strong reference was created and added
                // to a now dead cycle before being collected. We don't really have any work
                // to do here otherwise.
                // Dead nodes can be here if a buggy trace implementation or bad drop behavior
                // caused a node to not to be improperly cleaned up.
            }
            Status::RecentlyDecremented => {
                // This node had a strong ref dropped recently and might form a cycle, trace
                // it
                trace_children(inner, &mut traced_nodes);
            }
        }
    }

    unsafe {
        // Remove all zombie nodes, their
        traced_nodes.retain(|ptr, _| {
            if ptr.as_ref().status.get() != Status::Dead {
                true
            } else {
                if ptr.as_ref().strong_count() == NonZeroUsize::new(1).unwrap() {
                    // Zombie node made it to the old gen and we're the last remaining reference to
                    // it, we can safely de-allocate it here.
                    Inner::dealloc(*ptr);
                } else {
                    // We need to decrement the strong ref we added when tracing to prevent leaks.
                    // SAFETY: We added a strong ref when we added the node either to the old gen or
                    // the traced set, and we're done with processing the node
                    // after this call. We don't have to worry about concurrent
                    // drops invalidating the reference while it's still
                    // held until end of scope.
                    ptr.as_ref().decrement_strong_count()
                }
                false
            }
        });
    }

    let (live_nodes, mut dead_nodes): (IndexMap<_, _>, _) = traced_nodes
        .into_iter()
        .partition(|(ptr, refs)| unsafe { ptr.as_ref().strong_count() > *refs });

    for (live_node, _) in live_nodes {
        let inner = unsafe { live_node.as_ref() };
        inner.mark_live();
        mark_children_live(inner, &mut dead_nodes);

        // We need to decrement the strong ref we added when tracing to prevent leaks.
        // SAFETY: We added a strong ref when we added the node either to the old gen or the
        // traced set, and we're done with processing the node after this call. We don't
        // have to worry about concurrent drops invalidating the reference while it's still
        // held until end of scope.
        unsafe { inner.decrement_strong_count() };
    }

    for ptr in dead_nodes.keys() {
        unsafe { ptr.as_ref().mark_dead() };
    }

    // SAFETY:
    // We've removed all nodes reachable from still-live nodes, and we've removed all zombie nodes.
    // If we've made a mistake or a broken Traceable implementation has misreported its owned nodes,
    // we'll be marking the node dead prior to dropping to make the data inaccessible. Safe code
    // will not be able to see dropped inner values and any destructors for GcPtrs will not attempt
    // to drop _or deallocate_ the node itself. We also do not deallocate or remove our
    // collector's strong reference (which might cause a drop implementation to de-allocate the
    // node in the face of bugs).
    unsafe {
        for ptr in dead_nodes.keys() {
            // At this point, nodes in our candidate list marked dead are _definitely dead_, so we
            // can go ahead and drop them. During drop, new refs might get added to dead
            // nodes, so we will pass back over the list we dropped and double-check
            // their refcounts before actually de-allocating them.
            Inner::drop_data(*ptr);
        }
    }

    // SAFETY:
    // We examine the reference count of all nodes remaining in dead nodes to ensure that the last
    // remaining strong reference is from the collector itself. We only de-allocate if this is the
    // case, since we can be certain it's not possible for safe code to try to read from the
    // pointer.
    unsafe {
        // De-allocate the dropped nodes. The refcount must be checked here as someone
        // may have stashed a copy during drop.
        for (ptr, _) in dead_nodes {
            // If the strong count is 1, dead_nodes is the last owner of the data and we can
            // safely de-allocate its memory.
            if ptr.as_ref().strong_count() == NonZeroUsize::new(1).unwrap() {
                Inner::dealloc(ptr);
            } else {
                // We must remove the strong count the collector owns in order to prevent leaks.
                ptr.as_ref().decrement_strong_count();
            }
        }
    }
}

fn trace_children(
    inner: &Inner<dyn Traceable>,
    traced_nodes: &mut IndexMap<NonNull<Inner<dyn Traceable>>, NonZeroUsize>,
) {
    inner.data.visit_children(&mut GcVisitor {
        visitor: &mut |node| {
            let ptr = node.inner_ptr;
            let inner = unsafe { ptr.as_ref() };

            match traced_nodes.entry(ptr) {
                indexmap::map::Entry::Occupied(mut known) => {
                    // We've already seen this node. We do a saturating add because it's ok to
                    // undercount references here and usize::max references is kind of a degenerate
                    // case.
                    *known.get_mut() =
                        NonZeroUsize::new(known.get().get().saturating_add(1)).unwrap();
                }
                indexmap::map::Entry::Vacant(new) => {
                    // Visiting the children of this pointer may cause a
                    // reference to it to be dropped, so we must increment the
                    // strong count here.
                    inner.increment_strong_count();

                    // We haven't yet visited this pointer, add it to the list
                    // of seen pointers. We set the initial refcount to 2 here
                    // because we've seen it once and we know our traced nodes
                    // set contains it with one strong ref.
                    new.insert(NonZeroUsize::new(2).unwrap());

                    match inner.status.get() {
                        Status::Live | Status::RecentlyDecremented => {
                            trace_children(inner, traced_nodes);
                        }
                        Status::Dead => {
                            // This is reachable if a previously dead node was
                            // resurrected during a drop and stored as a child
                            // of a cycle. Its inner data is dropped, so we
                            // can't touch it.
                        }
                    }
                }
            }
        },
    });
}

fn mark_children_live(
    inner: &Inner<dyn Traceable>,
    traced_nodes: &mut IndexMap<NonNull<Inner<dyn Traceable>>, NonZeroUsize>,
) {
    inner.data.visit_children(&mut GcVisitor {
        visitor: &mut |node| {
            let inner = unsafe { node.inner_ptr.as_ref() };
            inner.mark_live();

            // We don't want to visit this node when checking for dead nodes later, so we make sure
            // to remove it from the list of traced nodes.
            if traced_nodes.remove(&node.inner_ptr).is_some() {
                // The node is part of a cycle and would be dead if the live root that's
                // holding onto it were to die, but it's not actually dead, remove its children from
                // the list of dead nodes.
                mark_children_live(inner, traced_nodes);

                // We need to decrement the strong ref we added when tracing to prevent leaks.
                // SAFETY: We added a strong ref when we added the node either to the old gen or the
                // traced set, and we're done with processing the node after this call. We don't
                // have to worry about concurrent drops invalidating the reference while it's still
                // held until end of scope.
                unsafe { inner.decrement_strong_count() };
            } else {
                // The node won't be marked dead and collected by the collector, so we don't need to
                // do anything with it.
            }
        },
    });
}
