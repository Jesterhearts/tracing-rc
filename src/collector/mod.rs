use std::{
    cell::{
        Cell,
        RefCell,
    },
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
pub(crate) enum Status {
    Live,
    RecentlyDecremented,
    Dead,
    Zombie,
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
    collect_with_options(CollectOptions {
        old_gen_threshold: 0,
        kind: CollectionType::Default,
    })
}

/// Perform a normal collection cycle.
pub fn collect() {
    collect_with_options(CollectOptions::default())
}

/// Perform a collection cycle based on `CollectionOptions`.
pub fn collect_with_options(options: CollectOptions) {
    collect_new_gen(options);
    if options.kind != CollectionType::YoungOnly {
        unsafe { collect_old_gen() };
    }
}

fn collect_new_gen(options: CollectOptions) {
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

unsafe fn trace_children(
    ptr: NonNull<Inner<dyn Traceable>>,
    traced_nodes: &mut IndexMap<NonNull<Inner<dyn Traceable>>, NonZeroUsize>,
) {
    let mut visitor = GcVisitor {
        visitor: &mut |node| {
            let ptr = node.inner_ptr;
            match traced_nodes.entry(ptr) {
                indexmap::map::Entry::Occupied(mut known) => {
                    // We've already seen this node. We do a saturating add because it's ok to
                    // undercount references here and usize::max references is kind of a degenerate
                    // case.
                    *known.get_mut() =
                        NonZeroUsize::new_unchecked(known.get().get().saturating_add(1));
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
        },
    };

    ptr.as_ref().data.visit_children(&mut visitor);
}

unsafe fn mark_live(
    inner: &Inner<dyn Traceable>,
    traced_nodes: &mut IndexMap<NonNull<Inner<dyn Traceable>>, NonZeroUsize>,
) {
    inner.force_live();

    let mut visitor = GcVisitor {
        visitor: &mut |node| {
            // We don't want to visit this node when checking for dead nodes later, so we make sure
            // to remove it from the list of traced nodes.
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
        },
    };

    inner.data.visit_children(&mut visitor);
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

        let mut visitor = GcVisitor {
            visitor: &mut |node| {
                mark(node.inner_ptr, traced_nodes);
            },
        };

        // The node was part of a cycle, we can handle cleanup at a later stage.
        // For now we need to visit its children because even a dead cycle may have
        // outward bound edges to non-dead nodes and we need to mark them as live if so.
        ptr.as_ref().data.visit_children(&mut visitor);
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
