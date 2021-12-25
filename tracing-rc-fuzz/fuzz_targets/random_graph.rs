#![no_main]
use std::ops::Deref;

use arbitrary::{
    Arbitrary,
    Result,
    Unstructured,
};
use indoc::formatdoc;
use libfuzzer_sys::fuzz_target;
use petgraph::{
    graph::NodeIndex,
    stable_graph::StableGraph,
    Directed,
    Direction,
};
use tracing_rc::rc::{
    count_roots,
    *,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Arbitrary)]
enum NodeData {
    None,
    Borrow,
}

struct Graph {
    graph: StableGraph<NodeData, (), Directed, usize>,
}

impl std::fmt::Debug for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let build_nodes = self
            .graph
            .node_indices()
            .map(|id| formatdoc!("let node_{} = Gc::new(Cycle::default());", id.index()))
            .collect::<Vec<_>>()
            .join("\n");

        let build_edges = self
            .graph
            .edge_indices()
            .map(|edge| {
                let (lhs, rhs) = self.graph.edge_endpoints(edge).unwrap();
                format!(
                    "node_{}.borrow_mut().neighbors.push(node_{}.clone());",
                    lhs.index(),
                    rhs.index()
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        let borrowed_clones = self
            .graph
            .node_indices()
            .filter_map(|node| match self.graph[node] {
                NodeData::None => None,
                NodeData::Borrow => Some(formatdoc! {"
                        let node_{id}_stack = node_{id}.clone();
                        let node_{id}_borrow = node_{id}_stack.borrow();",
                    id = node.index()
                }),
            })
            .collect::<Vec<_>>()
            .join("\n");

        let drop_nodes = self
            .graph
            .node_indices()
            .map(|id| formatdoc!("drop(node_{});", id.index()))
            .collect::<Vec<_>>()
            .join("\n");

        let drop_borrows = self
            .graph
            .node_indices()
            .filter_map(|node| match self.graph[node] {
                NodeData::None => None,
                NodeData::Borrow => Some(formatdoc!("drop(node_{}_borrow);", node.index())),
            })
            .collect::<Vec<_>>()
            .join("\n");

        let drop_clones = self
            .graph
            .node_indices()
            .filter_map(|node| match self.graph[node] {
                NodeData::None => None,
                NodeData::Borrow => Some(formatdoc!("drop(node_{}_stack);", node.index())),
            })
            .collect::<Vec<_>>()
            .join("\n");

        f.debug_struct("Graph")
            .field("graph", &self.graph)
            .field(
                "test function",
                &formatdoc! { "
                        #[test]
                        fn fuzzfound() {{
                            {nodes}

                            {edges}

                            {borrows}

                            {drops}

                            collect_full();

                            {drop_borrows}

                            {drop_clones}

                            collect_full();
                        }}
                        ",
                        nodes = build_nodes,
                        edges = build_edges,
                        borrows = borrowed_clones,
                        drops = drop_nodes,
                        drop_borrows = drop_borrows,
                        drop_clones = drop_clones,
                },
            )
            .finish()
    }
}

impl Deref for Graph {
    type Target = StableGraph<NodeData, (), Directed, usize>;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl AsRef<StableGraph<NodeData, (), Directed, usize>> for Graph {
    fn as_ref(&self) -> &StableGraph<NodeData, (), Directed, usize> {
        &**self
    }
}

impl<'a> Arbitrary<'a> for Graph {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let node_count = usize::from(u8::arbitrary(u)? / 16);
        if node_count == 0 {
            return Ok(Graph {
                graph: StableGraph::default(),
            });
        }

        let mut graph = StableGraph::with_capacity(node_count, 0);
        for _ in 0..node_count {
            graph.add_node(NodeData::arbitrary(u)?);
        }

        for _ in 0..node_count {
            let edges = u8::arbitrary(u)? / 64;
            for _ in 0..edges {
                let a = u8::arbitrary(u)?;
                let b = u8::arbitrary(u)?;
                graph.add_edge(
                    NodeIndex::new(usize::from(a).min(node_count - 1)),
                    NodeIndex::new(usize::from(b).min(node_count - 1)),
                    (),
                );
            }
        }

        Ok(Graph { graph })
    }
}

#[derive(Debug)]
struct GraphNode {
    data: Box<NodeData>,
    neighbors: Vec<Gc<GraphNode>>,
}

impl GraphNode {
    fn new(data: NodeData) -> Self {
        Self {
            data: Box::new(data),
            neighbors: Default::default(),
        }
    }
}

impl Trace for GraphNode {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for node in self.neighbors.iter() {
            node.visit_children(visitor);
        }
    }
}

fuzz_target!(|graph: Graph| {
    let mut nodes = Vec::with_capacity(graph.node_count());
    for idx in (0..graph.node_count()).into_iter().map(NodeIndex::new) {
        nodes.push(Gc::new(GraphNode::new(graph[idx])));
    }

    for idx in graph.node_indices() {
        let node = &nodes[idx.index()];
        for neighbor in graph.neighbors_directed(idx, Direction::Outgoing) {
            node.borrow_mut()
                .neighbors
                .push(nodes[neighbor.index()].clone());
        }
    }

    let mut to_borrow = vec![];

    for idx in graph.node_indices() {
        if *nodes[idx.index()].borrow().data == NodeData::Borrow {
            to_borrow.push(nodes[idx.index()].clone());
        }
    }

    let mut borrows = vec![];
    for borrow in to_borrow.iter() {
        borrows.push(borrow.borrow());
    }

    drop(nodes);

    collect_full();

    for borrow in borrows.iter() {
        assert_eq!(*borrow.data, NodeData::Borrow);
    }

    drop(borrows);
    drop(to_borrow);

    while count_roots() != 0 {
        collect_full();
    }
});
