use std::{
    cell::RefCell,
    num::NonZeroUsize,
    ptr::NonNull,
};

use indexmap::{
    IndexMap,
    IndexSet,
};
use petgraph::{
    csr::{
        Csr,
        NodeIndex,
    },
    Directed,
};

use crate::{
    rc::{
        trace::Trace,
        Buffered,
        Gc,
        Inner,
        SafeInnerView,
        Status,
    },
    CollectOptions,
    CollectionType,
};

#[derive(Debug)]
pub(crate) struct Node {
    pub(super) inner_ptr: NonNull<Inner<dyn Trace>>,
}

/// Visitor provided during tracing of the reachable object graph. You shouldn't need to interact
/// with this as [`Gc::visit_children`] will do the right thing for you, but you may call
/// [`Self::visit_node`] if you prefer.
pub struct GcVisitor<'cycle> {
    visitor: &'cycle mut dyn FnMut(Node),
}

impl GcVisitor<'_> {
    /// Visit an owned [Gc] node.
    pub fn visit_node<T: Trace>(&mut self, node: &Gc<T>) {
        (self.visitor)(node.node());
    }
}

type GraphIndex = NodeIndex<usize>;

type ConnectivityGraph = Csr<NonNull<Inner<dyn Trace>>, (), Directed, GraphIndex>;

type TracedNodeList = IndexMap<NonNull<Inner<dyn Trace>>, (GraphIndex, NonZeroUsize)>;

thread_local! { pub(super) static OLD_GEN: RefCell<IndexSet<NonNull<Inner<dyn Trace>>>> = RefCell::new(IndexSet::default()) }
thread_local! { pub(super) static YOUNG_GEN: RefCell<IndexMap<NonNull<Inner<dyn Trace>>, usize>> = RefCell::new(IndexMap::default()) }

#[doc(hidden)]
pub fn count_roots() -> usize {
    OLD_GEN.with(|old| old.borrow().len()) + YOUNG_GEN.with(|young| young.borrow().len())
}

/// Perform a full, cycle-tracing collection of both the old & young gen.
pub fn collect_full() {
    collect_with_options(CollectOptions::TRACE_AND_COLLECT_ALL);
}

/// Perform a normal collection cycle.
pub fn collect() {
    collect_with_options(CollectOptions::default());
}

/// Perform a collection cycle based on `CollectionOptions`.
pub fn collect_with_options(options: CollectOptions) {
    collect_new_gen(options);
    if options.kind != CollectionType::YoungOnly {
        collect_old_gen();
    }
}

fn collect_new_gen(options: CollectOptions) {
    let mut needs_drop = vec![];
    OLD_GEN.with(|old_gen| {
        let mut old_gen = old_gen.borrow_mut();
        YOUNG_GEN.with(|gen| {
            gen.borrow_mut().retain(|ptr, generation| {
                // SAFETY: The young gen owns a strong reference, it only de-allocates and
                // (possibly) drops if it is the last owner. If an item is no longer possibly dead,
                // it is marked unbuffered and never referenced by the young gen in this function
                // again.
                unsafe {
                    debug_assert_eq!(ptr.as_ref().buffered.get(), Buffered::YoungGen);
                    if ptr.as_ref().strong.get() == NonZeroUsize::new(1).unwrap() {
                        needs_drop.push(*ptr);
                        return false;
                    }

                    if ptr.as_ref().status.get() != Status::RecentlyDecremented {
                        // It is alive or dead, either way we won't need to trace its children.
                        // If it's alive, we'll get another chance to clean it up.
                        // If it's dead, it can't have children so the destructor should handle
                        // cleanup eventually.
                        ptr.as_ref().buffered.set(Buffered::Unbuffered);
                        ptr.as_ref().decrement_strong_count();
                        return false;
                    }
                }

                if *generation < options.old_gen_threshold {
                    *generation += 1;
                    return true;
                }

                // SAFETY: ptr is not null and not dangling.
                unsafe { ptr.as_ref().buffered.set(Buffered::OldGen) };
                old_gen.insert(*ptr);
                false
            });
        });
    });

    for ptr in needs_drop {
        // SAFETY: The young generation was the last remaining owner of the pointer, so we can
        // safely drop it. It's not possible from safe code for another reference to
        // this pointer to be generated during drop.
        // Depending on collector ordering, we can have zombie nodes in the young
        // generation.
        unsafe { Inner::zombie_safe_drop_data_dealloc(ptr) }
    }
}

fn collect_old_gen() {
    let candidate_nodes = OLD_GEN.with(|old_gen| {
        let mut old_gen = old_gen.borrow_mut();

        old_gen
            .drain(..)
            .collect::<IndexSet<NonNull<Inner<dyn Trace>>>>()
    });

    let mut traced_nodes = TracedNodeList::with_capacity(candidate_nodes.len());
    let mut connectivity_graph = ConnectivityGraph::default();

    for node in candidate_nodes.iter().copied() {
        let node_ix = connectivity_graph.add_node(node);
        traced_nodes.insert(node, (node_ix, NonZeroUsize::new(1).unwrap()));
    }

    // Iterate over all nodes reachable from the old gen tracking them in the list of all
    // traced nodes.
    for ptr in candidate_nodes {
        let inner = unsafe { ptr.as_ref() };

        debug_assert_eq!(inner.buffered.get(), Buffered::OldGen);
        inner.buffered.set(Buffered::Trace);

        let node_ix = traced_nodes[&ptr].0;
        match inner.status.get() {
            Status::Live | Status::Dead => {
                // We'll collect it later if it a new strong reference was created and added
                // to a now dead cycle before being collected. We don't really have any work
                // to do here otherwise.
                // Dead nodes can be here if a buggy trace implementation or bad drop behavior
                // caused a node to be improperly cleaned up.
            }
            Status::RecentlyDecremented => {
                // This node had a strong ref dropped recently and might form a cycle, trace
                // it
                trace_children(inner, node_ix, &mut traced_nodes, &mut connectivity_graph);
            }
        }
    }

    let (live_nodes, mut dead_nodes): (TracedNodeList, _) = traced_nodes
        .into_iter()
        .partition(|(ptr, (_, refs))| unsafe { ptr.as_ref().strong.get() > *refs });

    for &(node_index, _) in live_nodes.values() {
        filter_live_node_children(&connectivity_graph, node_index, &mut dead_nodes);
    }

    for (node, _) in live_nodes {
        // SAFETY: We added a strong ref when we added the node either to the old gen or the
        // traced set, and we're done with processing the node after this call.
        unsafe {
            unbuffer_from_trace(node);
        };
    }

    // SAFETY:
    // We only attempt to de-allocate already dead nodes here, since those will have had their
    // inner data dropped or leaked and can't have cycles. If we don't have exclusive ownership of
    // the dead node, we remove the node from our list after we unbuffer and reduce our strong
    // count.
    // If a node can't be marked as dead, we also remove it from our list of nodes to drop, as we
    // know there's an outstanding borrow of the data somewhere on the stack and we can't drop our
    // data out from under them.
    unsafe {
        dead_nodes.retain(|ptr, _| {
            if ptr.as_ref().status.get() == Status::Dead {
                if ptr.as_ref().strong.get() == NonZeroUsize::new(1).unwrap() {
                    // Zombie node made it to the old gen and we're the last remaining reference to
                    // it, we can safely de-allocate it here.
                    Inner::dealloc(*ptr);
                } else {
                    unbuffer_from_trace(*ptr);
                }
                false
            } else if ptr.as_ref().try_mark_dead() {
                true
            } else {
                // Since try_mark_dead returns false if we fail to mark a node dead, we will filter
                // out nodes that were erroneously marked dead here.
                unbuffer_from_trace(*ptr);
                false
            }
        });
    }

    // SAFETY: We know our node is nonnull and not dangling. We only examine the buffer status of
    // the node here.
    unsafe {
        // We might have nodes that were buffered in the old/young gen during tracing. We track that
        // membership, so we know if the old/young gen added a reference to keep the node alive.
        // Once we get here, we want to pull those now dead nodes out of the collection lists.
        for node in dead_nodes.keys() {
            match node.as_ref().buffered.get() {
                Buffered::YoungGen => YOUNG_GEN.with(|gen| {
                    gen.borrow_mut().swap_remove(node);
                }),
                Buffered::OldGen => OLD_GEN.with(|gen| {
                    gen.borrow_mut().swap_remove(node);
                }),
                Buffered::Trace => {
                    // We don't need to steal it from one of the generations, since it's in the
                    // trace buffer
                }
                Buffered::Unbuffered => unreachable!("All traced nodes should be marked buffered"),
            }
        }
    }

    // SAFETY:
    // We've removed all nodes reachable from still-live nodes, and we've removed all zombie nodes.
    // If there was an outstanding borrow to the node, we'll have filtered it out of our dead list
    // already above. If we've made a mistake or a broken Trace implementation has
    // misreported its owned nodes, we've aleady marked the node dead prior to this point, so we
    // can drop the data without worrying about safe code being able to see dropped inner values
    // and any destructors for Gcs will not attempt to drop _or deallocate_ the node itself. We
    // also do not deallocate or remove our collector's strong reference (which might cause a
    // drop implementation to de-allocate the node in the face of bugs).
    unsafe {
        for (ptr, _) in dead_nodes.iter() {
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
        for (ptr, _) in dead_nodes {
            if ptr.as_ref().strong.get() == NonZeroUsize::new(1).unwrap() {
                // If the strong count is 1, dead_nodes is the last owner of the data and we can
                // safely de-allocate its memory.
                Inner::dealloc(ptr);
            } else {
                // There was a bug in a Trace implementation, and this node isn't really dead. Drop
                // our strong count so we can hopefully clean it up later.
                ptr.as_ref().buffered.set(Buffered::Unbuffered);
                ptr.as_ref().decrement_strong_count();
            }
        }
    }
}

fn trace_children(
    parent: &Inner<dyn Trace>,
    parent_ix: GraphIndex,
    traced_nodes: &mut TracedNodeList,
    connectivity_graph: &mut ConnectivityGraph,
) {
    let parent = if let Ok(parent) = parent.data.try_borrow_mut() {
        parent
    } else {
        // If we can't get mutable access to child, it must be exclusively borrowed by somebody
        // somewhere on the stack or from within this trace implementation.
        //
        // If it's our borrow, we're already visiting that nodes children, so there's no reason for
        // us to do so again.
        //
        // If it's someone else, it must be live and not visiting its
        // children will undercount any nodes it owns, which is guaranteed to prevent us
        // from deciding those nodes are dead.
        return;
    };

    parent.visit_children(&mut GcVisitor {
        visitor: &mut |node| {
            let ptr = node.inner_ptr;

            match traced_nodes.entry(ptr) {
                indexmap::map::Entry::Occupied(mut seen) => {
                    // We've already seen this node. We do a saturating add because it's ok to
                    // undercount references here and usize::max references is kind of a degenerate
                    // case.
                    seen.get_mut().1 =
                        NonZeroUsize::new(seen.get().1.get().saturating_add(1)).unwrap();
                    connectivity_graph.add_edge(parent_ix, seen.get().0, ());
                }
                indexmap::map::Entry::Vacant(unseen) => {
                    // SAFETY: ptr is not null & not dangling.
                    // We don't examine its data if its dead.
                    let inner = unsafe { ptr.as_ref() };
                    if inner.buffered.get() == Buffered::Unbuffered {
                        // Visiting the children of this pointer may cause a
                        // reference to it to be dropped, so we must increment the
                        // strong count here.
                        SafeInnerView::from(inner).increment_strong_count();
                        inner.buffered.set(Buffered::Trace);
                    } else {
                        // It's either old or young gen, we can steal it from the buffer later.
                        debug_assert_ne!(inner.buffered.get(), Buffered::Trace);
                    }

                    // We haven't yet visited this pointer, add it to the list
                    // of seen pointers. We set the initial refcount to 2 here
                    // because we've seen it once and we know our traced nodes
                    // set contains it with one strong ref.
                    let child_ix = connectivity_graph.add_node(ptr);
                    unseen.insert((child_ix, NonZeroUsize::new(2).unwrap()));
                    connectivity_graph.add_edge(parent_ix, child_ix, ());

                    match inner.status.get() {
                        Status::Live | Status::RecentlyDecremented => {
                            trace_children(inner, child_ix, traced_nodes, connectivity_graph);
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

fn filter_live_node_children(
    graph: &ConnectivityGraph,
    node: NodeIndex<usize>,
    dead_nodes: &mut TracedNodeList,
) {
    for &child in graph.neighbors_slice(node) {
        let node = graph[child];
        if dead_nodes.swap_remove(&node).is_some() {
            // SAFETY: We added a strong ref when we added the node either to the old gen or the
            // traced set, and we're done with processing the after this block. We aren't making any
            // calls out to the inner data so we're not worried about it being invalidated under us
            // while cleaning things up.
            unsafe {
                unbuffer_from_trace(node);
            };

            filter_live_node_children(graph, child, dead_nodes);
        }
    }
}

/// # Safety:
/// - The caller of this function must not attempt to access self after calling this function.
unsafe fn unbuffer_from_trace(ptr: NonNull<Inner<dyn Trace>>) {
    if ptr.as_ref().buffered.get() == Buffered::Trace {
        ptr.as_ref().buffered.set(Buffered::Unbuffered);
        ptr.as_ref().decrement_strong_count();
    } else {
        // The node was buffered in an old/young gen context.
        debug_assert_ne!(ptr.as_ref().buffered.get(), Buffered::Unbuffered);
    }
}
