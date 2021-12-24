use std::{
    cell::RefCell,
    collections::HashMap,
    num::NonZeroUsize,
    rc::{
        Rc,
        Weak,
    },
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
    impl_node,
    rc::{
        trace::Trace,
        Gc,
        Inner,
        Status,
    },
    CollectOptions,
    CollectionType,
};

impl_node!(WeakNode { inner_ptr: Weak<Inner<dyn Trace>> }, upgrade(ptr) => Weak::upgrade(ptr));
impl_node!(StrongNode { inner_ptr: Rc<Inner<dyn Trace>> }, upgrade(ptr) => ptr);

impl TryFrom<WeakNode> for StrongNode {
    type Error = ();

    fn try_from(weak: WeakNode) -> Result<Self, Self::Error> {
        weak.upgrade()
            .map(|strong| StrongNode { inner_ptr: strong })
            .ok_or(())
    }
}

/// Visitor provided during tracing of the reachable object graph. You shouldn't need to interact
/// with this as [`Gc::visit_children`] will do the right thing for you.
pub struct GcVisitor<'cycle> {
    visitor: &'cycle mut dyn FnMut(Rc<Inner<dyn Trace>>),
}

impl GcVisitor<'_> {
    /// Visit an owned [`Gc`] node.
    pub fn visit_node<T: Trace + 'static>(&mut self, node: &Gc<T>) {
        (self.visitor)(node.ptr.clone());
    }
}

type GraphIndex = NodeIndex<usize>;

type ConnectivityGraph = Csr<(), (), Directed, GraphIndex>;

type TracedNodeList = IndexMap<StrongNode, NonZeroUsize>;

thread_local! { pub(super) static OLD_GEN: RefCell<IndexSet<WeakNode>> = RefCell::new(IndexSet::default()) }
thread_local! { pub(super) static YOUNG_GEN: RefCell<IndexMap<WeakNode, usize>> = RefCell::new(IndexMap::default()) }

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

/// Perform a collection cycle based on [`CollectOptions`].
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
            gen.borrow_mut().retain(|node, generation| {
                let strong_node = if let Some(node) = node.upgrade() {
                    node
                } else {
                    return false;
                };
                if strong_node.status.get() != Status::RecentlyDecremented {
                    // It is alive or dead, either way we won't need to trace its children.
                    // If it's alive, we'll get another chance to clean it up.
                    // If it's dead, it can't have children so the destructor should handle
                    // cleanup eventually.
                    return false;
                }

                if *generation < options.old_gen_threshold {
                    *generation += 1;
                    return true;
                }

                old_gen.insert(node.clone());
                false
            });
        });
    });
}

fn collect_old_gen() {
    let mut traced_nodes: TracedNodeList = OLD_GEN.with(|old_gen| {
        old_gen
            .borrow_mut()
            .drain(..)
            .filter_map(|weak| {
                weak.try_into()
                    .ok()
                    .map(|strong| (strong, NonZeroUsize::new(1).unwrap()))
            })
            .collect()
    });

    let mut connectivity_graph = ConnectivityGraph::with_nodes(traced_nodes.len());

    // Iterate over all nodes reachable from the old gen tracking them in the list of all
    // traced nodes.
    // We iterate over the initial list of nodes here, since all new nodes are added afterwards.
    for node_ix in (0..connectivity_graph.node_count()).map(GraphIndex::from) {
        match traced_nodes.get_index(node_ix).unwrap().0.status.get() {
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
                trace_children(node_ix, &mut traced_nodes, &mut connectivity_graph);
            }
        }
    }

    let mut live_nodes = vec![];
    let mut dead_nodes = HashMap::default();

    for (node_ix, (node, refs)) in traced_nodes.into_iter().enumerate() {
        if Rc::strong_count(&node) > refs.get() {
            live_nodes.push(node_ix);
        } else {
            dead_nodes.insert(node_ix, node);
        }
    }

    for node_index in live_nodes {
        filter_live_node_children(&connectivity_graph, node_index, &mut dead_nodes);
    }

    for node in dead_nodes.values() {
        node.status.set(Status::Dead);
    }

    for (_, node) in dead_nodes {
        node.drop_data();
    }
}

fn trace_children(
    parent_ix: GraphIndex,
    traced_nodes: &mut TracedNodeList,
    connectivity_graph: &mut ConnectivityGraph,
) {
    let pin_parent = traced_nodes.get_index(parent_ix).unwrap().0.clone();
    let parent = if let Ok(parent) = pin_parent.data.try_borrow_mut() {
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
                    debug_assert_eq!(child_ix, unseen.index());

                    connectivity_graph.add_edge(parent_ix, child_ix, ());

                    // 1 for the graph strong reference, 1 for the seen reference.
                    unseen.insert(NonZeroUsize::new(2).unwrap());
                    trace_children(child_ix, traced_nodes, connectivity_graph);
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
