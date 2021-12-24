use std::{
    collections::{
        HashMap,
        VecDeque,
    },
    num::NonZeroUsize,
    sync::{
        Arc,
        Weak,
    },
};

use dashmap::{
    DashMap,
    DashSet,
};
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use parking_lot::{
    const_mutex,
    Mutex,
};
use petgraph::{
    csr::{
        Csr,
        NodeIndex,
    },
    visit::IntoNodeIdentifiers,
    Directed,
};

use crate::{
    impl_node,
    sync::{
        Agc,
        AtomicInner,
        Status,
        Trace,
    },
    CollectOptions,
    CollectionType,
};

impl_node!(WeakNode { inner_ptr: Weak<AtomicInner<dyn Trace>> }, upgrade(ptr) => Weak::upgrade(ptr));
impl_node!(StrongNode { inner_ptr: Arc<AtomicInner<dyn Trace>> }, upgrade(ptr) => ptr);

impl TryFrom<&WeakNode> for StrongNode {
    type Error = ();

    fn try_from(weak: &WeakNode) -> Result<Self, Self::Error> {
        weak.upgrade()
            .map(|strong| StrongNode { inner_ptr: strong })
            .ok_or(())
    }
}

pub(super) static YOUNG_GEN: Lazy<DashMap<WeakNode, usize>> = Lazy::new(DashMap::default);
pub(super) static OLD_GEN: Lazy<DashSet<WeakNode>> = Lazy::new(DashSet::default);

static COLLECTION_MUTEX: Mutex<()> = const_mutex(());

/// Visitor provided during tracing of the reachable object graph. You shouldn't need to interact
/// with this as [`Agc::visit_children`] will do the right thing for you.
pub struct GcVisitor<'cycle> {
    visitor: &'cycle mut dyn FnMut(Arc<AtomicInner<dyn Trace>>),
}

impl GcVisitor<'_> {
    /// Visit an owned [`Agc`] node.
    pub fn visit_node<T: Trace + 'static>(&mut self, node: &Agc<T>) {
        (self.visitor)(node.ptr.clone());
    }
}

type GraphIndex = NodeIndex<usize>;

type ConnectivityGraph = Csr<(), (), Directed, GraphIndex>;

type TracedNodeList = IndexMap<StrongNode, NonZeroUsize>;

#[doc(hidden)]
pub fn count_roots() -> usize {
    YOUNG_GEN.len() + OLD_GEN.len()
}

/// Perform a full, cycle-tracing collection of both the old & young gen.
/// See [`collect`] for more details on the implementation of collection.
pub fn collect_full() {
    collect_with_options(CollectOptions::TRACE_AND_COLLECT_ALL);
}

/// Perform a normal collection cycle.
///
/// - Only one thread at a time is permitted to collect the old generation. If another thread
///   attempts to start collection while the old gen is being processed, it will return early
///   without performing any garbage collection.
/// - Collection is optimized for latency over throughput. The goal is minimal pause times, not
///   maximizing the speed with which memory is released.
/// - Collection is not incremental, it will process all of the possible pointers available at the
///   time collection starts.
/// - It may take several calls to `collect` or [`collect_full`] before all garbage is cleaned up
///   depending on the specific location of values in the old/young gen, even if no additional
///   garbage is produced between calls; however, is likely that only weak references to `Agc`
///   values will remain after a single pass.
pub fn collect() {
    collect_with_options(CollectOptions::default());
}

/// Perform a collection cycle based on [`CollectOptions`].
/// See [`collect`] for more details on the implementation of collection.
pub fn collect_with_options(options: CollectOptions) {
    collect_new_gen(options);
    if options.kind != CollectionType::YoungOnly {
        collect_old_gen();
    }
}

fn collect_new_gen(options: CollectOptions) {
    let mut to_old_gen = vec![];
    YOUNG_GEN.retain(|node, generation| {
        if node.strong_count() == 0 {
            return false;
        }

        if *generation < options.old_gen_threshold {
            *generation += 1;
            return true;
        }

        to_old_gen.push(node.clone());
        false
    });

    for node in to_old_gen {
        OLD_GEN.insert(node);
    }
}

fn collect_old_gen() {
    let _guard = if let Some(guard) = COLLECTION_MUTEX.try_lock() {
        guard
    } else {
        // Some other thread is running collection on the old gen.
        return;
    };

    let mut traced_nodes = IndexMap::with_capacity(OLD_GEN.len());
    OLD_GEN.retain(|node| {
        if let Ok(strong) = TryInto::<StrongNode>::try_into(node) {
            if strong.status.load(atomic::Ordering::Acquire) == Status::RecentlyDecremented {
                traced_nodes.insert(strong, NonZeroUsize::new(1).unwrap());
            }
        }
        false
    });

    let mut connectivity_graph = ConnectivityGraph::with_nodes(traced_nodes.len());

    let mut pending_nodes = connectivity_graph
        .node_identifiers()
        .collect::<VecDeque<_>>();

    while let Some(node_ix) = pending_nodes.pop_front() {
        trace_children(
            node_ix,
            &mut pending_nodes,
            &mut traced_nodes,
            &mut connectivity_graph,
        );
    }

    let mut live_nodes = vec![];
    let mut dead_nodes = HashMap::default();

    for (node_ix, (node, refs)) in traced_nodes.into_iter().enumerate() {
        if Arc::strong_count(&node) > refs.get() {
            // Even if this node becomes dead between the read of the strong count above and the
            // cmp/mark as live, we won't leak memory as the node will get added to the
            // young gen.
            node.status.store(Status::Live, atomic::Ordering::Release);
            live_nodes.push(node_ix);
        } else if node.status.load(atomic::Ordering::Acquire) != Status::Traced {
            // This node may be dead, but something accessed it internals between
            // the time we traced its children and now. We can't know if the child graph
            // is still the same, so we can't attempt to drop its children. If the node became dead
            // between tracing and access to its children, the node will end up placed in the young
            // gen and can be collected later.
            live_nodes.push(node_ix);
        } else {
            dead_nodes.insert(node_ix, node);
        }
    }

    for node_index in live_nodes {
        filter_live_node_children(&connectivity_graph, node_index, &mut dead_nodes);
    }

    for node in dead_nodes.values() {
        node.status.store(Status::Dead, atomic::Ordering::Release);
    }

    for (_, node) in dead_nodes {
        AtomicInner::drop_data(&node);
    }
}

fn trace_children(
    parent_ix: GraphIndex,
    pending_nodes: &mut VecDeque<GraphIndex>,
    traced_nodes: &mut TracedNodeList,
    connectivity_graph: &mut ConnectivityGraph,
) {
    let pin_parent = traced_nodes.get_index(parent_ix).unwrap().0.clone();
    let parent = {
        if let Some(_guard) = pin_parent.data.try_write() {
            // This must be guarded by an exclusive lock to prevent altering of the child graph
            // without marking the node dirty.
            pin_parent
                .status
                .store(Status::Traced, atomic::Ordering::Release);
        } else {
            // If we can't get exclusive access to parent, it must be locked by some active thread.
            //
            // It must be live and not visiting its children will undercount any nodes it owns,
            // which is guaranteed to prevent us from deciding those nodes are dead.
            return;
        };

        pin_parent.data.read_recursive()
    };

    parent.visit_children(&mut GcVisitor {
        visitor: &mut |node| {
            match traced_nodes.entry(node.into()) {
                indexmap::map::Entry::Occupied(mut seen) => {
                    // We've already seen this node. We do a saturating add because it's ok to
                    // undercount references here and usize::max references is kind of a degenerate
                    // case.
                    *seen.get_mut() =
                        NonZeroUsize::new(seen.get().get().saturating_add(1)).unwrap();

                    connectivity_graph.add_edge(parent_ix, seen.index(), ());
                }
                indexmap::map::Entry::Vacant(unseen) => {
                    let child_ix = connectivity_graph.add_node(());
                    debug_assert_eq!(unseen.index(), child_ix);

                    connectivity_graph.add_edge(parent_ix, child_ix, ());

                    // 1 for the graph strong reference, 1 for the seen reference.
                    unseen.insert(NonZeroUsize::new(2).unwrap());
                    pending_nodes.push_back(child_ix);
                }
            };
        },
    });
}

fn filter_live_node_children(
    graph: &ConnectivityGraph,
    node: GraphIndex,
    dead_nodes: &mut HashMap<GraphIndex, StrongNode>,
) {
    for child in graph.neighbors_slice(node) {
        if dead_nodes.remove(child).is_some() {
            filter_live_node_children(graph, *child, dead_nodes);
        }
    }
}
