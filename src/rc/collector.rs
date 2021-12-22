use std::{
    cell::RefCell,
    collections::HashMap,
    num::NonZeroUsize,
    ops::Deref,
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
    visit::IntoNodeReferences,
    Directed,
};

use crate::{
    rc::{
        trace::Trace,
        Gc,
        Inner,
        Status,
    },
    CollectOptions,
    CollectionType,
};

macro_rules! impl_node {
    ($name:ident, $inner:ident::upgrade($varname:ident) => $upg:expr) => {
        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("Node")
                    .field("strong", &$inner::strong_count(&self.inner_ptr))
                    .field("inner_ptr", &{
                        let $varname = &self.inner_ptr;
                        $upg
                    })
                    .finish()
            }
        }

        impl Deref for $name {
            type Target = $inner<Inner<dyn Trace>>;

            fn deref(&self) -> &Self::Target {
                &self.inner_ptr
            }
        }

        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                state.write_usize($inner::as_ptr(&self.inner_ptr) as *const Inner<()> as usize)
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                $inner::ptr_eq(&self.inner_ptr, &other.inner_ptr)
            }
        }

        impl Eq for $name {}
    };
}

#[derive(Clone)]
pub(crate) struct WeakNode {
    pub(super) inner_ptr: Weak<Inner<dyn Trace>>,
}

impl_node!(WeakNode, Weak::upgrade(ptr) => Weak::upgrade(ptr));

#[derive(Clone)]
pub(crate) struct StrongNode {
    pub(super) inner_ptr: Rc<Inner<dyn Trace>>,
}

impl TryFrom<WeakNode> for StrongNode {
    type Error = ();

    fn try_from(weak: WeakNode) -> Result<Self, Self::Error> {
        weak.upgrade()
            .map(|strong| StrongNode { inner_ptr: strong })
            .ok_or(())
    }
}

impl_node!(StrongNode, Rc::upgrade(ptr) => ptr);

#[derive(Debug, Clone, Copy)]
struct NodeKey(*const Inner<dyn Trace>);

impl std::hash::Hash for NodeKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const Inner<()> as usize)
    }
}

impl PartialEq for NodeKey {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0 as *const Inner<()>, other.0 as *const Inner<()>)
    }
}

impl Eq for NodeKey {}

impl From<&Rc<Inner<dyn Trace>>> for NodeKey {
    fn from(rc: &Rc<Inner<dyn Trace>>) -> Self {
        Self(Rc::as_ptr(rc))
    }
}

/// Visitor provided during tracing of the reachable object graph. You shouldn't need to interact
/// with this as [`Gc::visit_children`] will do the right thing for you, but you may call
/// [`Self::visit_node`] if you prefer.
pub struct GcVisitor<'cycle> {
    visitor: &'cycle mut dyn FnMut(Rc<Inner<dyn Trace>>),
}

impl GcVisitor<'_> {
    /// Visit an owned [Gc] node.
    pub fn visit_node<T: Trace + 'static>(&mut self, node: &Gc<T>) {
        (self.visitor)(node.ptr.clone());
    }
}

type GraphIndex = NodeIndex<usize>;

type ConnectivityGraph = Csr<StrongNode, (), Directed, GraphIndex>;

type TracedNodeList = IndexMap<NodeKey, NonZeroUsize>;

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
    let mut connectivity_graph = OLD_GEN.with(|old_gen| {
        let mut graph = ConnectivityGraph::default();
        for node in old_gen
            .borrow_mut()
            .drain(..)
            .filter_map(|weak| weak.try_into().ok())
        {
            graph.add_node(node);
        }
        graph
    });

    let mut traced_nodes = connectivity_graph
        .node_references()
        .map(|(_, node)| (NodeKey::from(node.deref()), NonZeroUsize::new(1).unwrap()))
        .collect();

    // Iterate over all nodes reachable from the old gen tracking them in the list of all
    // traced nodes.
    // We iterate over the initial list of nodes here, since all new nodes are added afterwards.
    for node_ix in (0..connectivity_graph.node_count()).map(GraphIndex::from) {
        match connectivity_graph[node_ix].status.get() {
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

    for (node_ix, (key, refs)) in traced_nodes.into_iter().enumerate() {
        let node = &connectivity_graph[node_ix];
        if Rc::strong_count(node) > refs.get() {
            live_nodes.push(node_ix);
        } else {
            dead_nodes.insert(key, node_ix);
        }
    }

    for node_index in live_nodes {
        filter_live_node_children(&connectivity_graph, node_index, &mut dead_nodes);
    }

    for &node_ix in dead_nodes.values() {
        connectivity_graph[node_ix].status.set(Status::Dead);
    }

    for (_, node_ix) in dead_nodes {
        Inner::drop_data(&connectivity_graph[node_ix]);
    }
}

fn trace_children(
    parent_ix: GraphIndex,
    traced_nodes: &mut TracedNodeList,
    connectivity_graph: &mut ConnectivityGraph,
) {
    let pin_parent = connectivity_graph[parent_ix].clone();
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
            let child_ix = match traced_nodes.entry(NodeKey::from(&node)) {
                indexmap::map::Entry::Occupied(mut seen) => {
                    // We've already seen this node. We do a saturating add because it's ok to
                    // undercount references here and usize::max references is kind of a degenerate
                    // case.
                    *seen.get_mut() =
                        NonZeroUsize::new(seen.get().get().saturating_add(1)).unwrap();

                    connectivity_graph.add_edge(
                        parent_ix,
                        traced_nodes.get_index_of(&NodeKey::from(&node)).unwrap(),
                        (),
                    );
                    return;
                }
                indexmap::map::Entry::Vacant(unseen) => {
                    let child_ix = connectivity_graph.add_node(StrongNode { inner_ptr: node });
                    // 1 for the graph strong reference, 1 for the seen reference.
                    unseen.insert(NonZeroUsize::new(2).unwrap());
                    connectivity_graph.add_edge(parent_ix, child_ix, ());

                    child_ix
                }
            };

            trace_children(child_ix, traced_nodes, connectivity_graph);
        },
    });
}

fn filter_live_node_children(
    graph: &ConnectivityGraph,
    node: GraphIndex,
    dead_nodes: &mut HashMap<NodeKey, GraphIndex>,
) {
    for &child in graph.neighbors_slice(node) {
        if dead_nodes
            .remove(&NodeKey::from(graph[child].deref()))
            .is_some()
        {
            filter_live_node_children(graph, child, dead_nodes);
        }
    }
}
