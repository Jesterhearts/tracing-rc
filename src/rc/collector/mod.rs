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
        traceable::Traceable,
        Gc,
        Inner,
    },
    CollectOptions,
    CollectionType,
    Status,
};

#[derive(Debug)]
pub(crate) struct Node {
    pub(super) inner_ptr: NonNull<Inner<dyn Traceable>>,
}

/// Visitor provided during tracing of the reachable object graph. You shouldn't need to interact
/// with this as [Gc::visit_children](`Gc<T>::visit_children`) will do the right thing for
/// you, but you may call [`Self::visit_node`] if you prefer.
pub struct GcVisitor<'cycle> {
    visitor: &'cycle mut dyn FnMut(Node),
}

impl GcVisitor<'_> {
    /// Visit an owned [Gc] node.
    pub fn visit_node<T: Traceable>(&mut self, node: &Gc<T>) {
        (self.visitor)(node.node());
    }
}

type GraphIndex = NodeIndex<usize>;

type ConnectivityGraph = Csr<NonNull<Inner<dyn Traceable>>, (), Directed, GraphIndex>;

type TracedNodeList = IndexMap<NonNull<Inner<dyn Traceable>>, (GraphIndex, NonZeroUsize)>;

thread_local! { pub(super) static OLD_GEN: RefCell<IndexSet<NonNull<Inner<dyn Traceable>>>> = RefCell::new(IndexSet::default()) }
thread_local! { pub(super) static YOUNG_GEN: RefCell<IndexMap<NonNull<Inner<dyn Traceable>>, usize>> = RefCell::new(IndexMap::default()) }

#[doc(hidden)]
pub fn count_roots() -> usize {
    OLD_GEN.with(|old| old.borrow().len()) + YOUNG_GEN.with(|young| young.borrow().len())
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
        .partition(|(ptr, (_, refs))| unsafe { ptr.as_ref().strong_count() > *refs });

    for (node_index, _) in live_nodes.values() {
        filter_live_node_children(&connectivity_graph, *node_index, &mut dead_nodes);
    }

    for (node, _) in live_nodes {
        // SAFETY: We added a strong ref when we added the node either to the old gen or the
        // traced set, and we're done with processing the node after this call.
        // We don't drop the strong count until after we're done marking the node as live.
        unsafe {
            node.as_ref().mark_live();

            // We need to decrement the strong ref we added when tracing to prevent leaks.
            node.as_ref().unbuffer_from_collector();
        };
    }

    unsafe {
        dead_nodes.retain(|ptr, _| {
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
                    // after this call.
                    ptr.as_ref().unbuffer_from_collector();
                }
                false
            }
        });
    }

    for (ptr, _) in dead_nodes.iter() {
        unsafe { ptr.as_ref().mark_dead() };
    }

    // SAFETY:
    // We've removed all nodes reachable from still-live nodes, and we've removed all zombie nodes.
    // If we've made a mistake or a broken Traceable implementation has misreported its owned nodes,
    // we'll be marking the node dead prior to dropping to make the data inaccessible. Safe code
    // will not be able to see dropped inner values and any destructors for Gcs will not attempt
    // to drop _or deallocate_ the node itself. We also do not deallocate or remove our
    // collector's strong reference (which might cause a drop implementation to de-allocate the
    // node in the face of bugs).
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
        // De-allocate the dropped nodes. The refcount must be checked here as someone
        // may have stashed a copy during drop.
        for (ptr, _) in dead_nodes {
            // If the strong count is 1, dead_nodes is the last owner of the data and we can
            // safely de-allocate its memory.
            if ptr.as_ref().strong_count() == NonZeroUsize::new(1).unwrap() {
                Inner::dealloc(ptr);
            } else {
                // We must remove the strong count the collector owns in order to prevent leaks.
                ptr.as_ref().unbuffer_from_collector();
            }
        }
    }
}

fn trace_children(
    parent: &Inner<dyn Traceable>,
    parent_ix: GraphIndex,
    traced_nodes: &mut TracedNodeList,
    connectivity_graph: &mut ConnectivityGraph,
) {
    parent.data.visit_children(&mut GcVisitor {
        visitor: &mut |node| {
            let ptr = node.inner_ptr;
            let inner = unsafe { ptr.as_ref() };

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
                    // Visiting the children of this pointer may cause a
                    // reference to it to be dropped, so we must increment the
                    // strong count here.
                    inner.increment_strong_count();

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
    for child in graph.neighbors_slice(node) {
        let node = graph[*child];
        if dead_nodes.swap_remove(&node).is_some() {
            // We need to decrement the strong ref we added when tracing to prevent leaks.
            // SAFETY: We added a strong ref when we added the node either to the old gen or the
            // traced set, and we're done with processing the after this block. We aren't making any
            // calls out to the inner data so we're not worried about it being invalidated under us
            // while cleaning things up.
            unsafe {
                // remove hints that the node might be dead in case we have a copy in the old/young
                // gen.
                node.as_ref().mark_live();
                node.as_ref().unbuffer_from_collector()
            };

            filter_live_node_children(graph, *child, dead_nodes);
        }
    }
}
