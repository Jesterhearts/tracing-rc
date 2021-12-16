#![no_main]
use std::ops::Deref;

use arbitrary::{
    Arbitrary,
    Result,
    Unstructured,
};
use libfuzzer_sys::fuzz_target;
use petgraph::{
    graph::NodeIndex,
    stable_graph::StableGraph,
    Directed,
    Direction,
};
use tracing_rc::rc::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Arbitrary)]
enum ReportBehavior {
    Once,
    Twice,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Arbitrary)]
enum NodeData {
    None,
    Borrow,
}

#[derive(Debug)]
struct Graph {
    graph: StableGraph<NodeData, ReportBehavior, Directed, usize>,
}

impl Deref for Graph {
    type Target = StableGraph<NodeData, ReportBehavior, Directed, usize>;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

impl AsRef<StableGraph<NodeData, ReportBehavior, Directed, usize>> for Graph {
    fn as_ref(&self) -> &StableGraph<NodeData, ReportBehavior, Directed, usize> {
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
                    ReportBehavior::arbitrary(u)?,
                );
            }
        }

        Ok(Graph { graph })
    }
}

#[derive(Debug)]
struct GraphNode {
    data: Box<NodeData>,
    behaviors: Vec<ReportBehavior>,
    neighbors: Vec<Gc<GraphNode>>,
}

impl GraphNode {
    fn new(data: NodeData) -> Self {
        Self {
            data: Box::new(data),
            behaviors: Default::default(),
            neighbors: Default::default(),
        }
    }
}

impl Trace for GraphNode {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        assert_eq!(self.behaviors.len(), self.neighbors.len());
        for (behavior, node) in self.behaviors.iter().zip(self.neighbors.iter()) {
            node.visit_children(visitor);
            if *behavior == ReportBehavior::Twice {
                node.visit_children(visitor);
            }
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
        for (neighbor, edge) in graph
            .neighbors_directed(idx, Direction::Outgoing)
            .zip(graph.edges_directed(idx, Direction::Outgoing))
        {
            node.borrow_mut()
                .neighbors
                .push(nodes[neighbor.index()].clone());
            node.borrow_mut().behaviors.push(*edge.weight());
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

    collect_full();
    collect_full();
});
