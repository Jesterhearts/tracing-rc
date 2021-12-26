use tracing_rc::{
    rc::{
        collect_full,
        Gc,
    },
    Trace,
};

#[derive(Trace)]
struct GraphNode<T: 'static> {
    #[trace(ignore)]
    data: T,
    edge: Option<Gc<GraphNode<T>>>,
}

fn main() {
    {
        let node_a = Gc::new(GraphNode {
            data: 10,
            edge: None,
        });
        let node_b = Gc::new(GraphNode {
            data: 11,
            edge: None,
        });
        let node_c = Gc::new(GraphNode {
            data: 12,
            edge: Some(node_a.clone()),
        });

        node_a.borrow_mut().edge = Some(node_b.clone());
        node_b.borrow_mut().edge = Some(node_c);

        let a = node_a.borrow();
        let b = a.edge.as_ref().unwrap().borrow();
        let c = b.edge.as_ref().unwrap().borrow();

        assert_eq!(a.data, c.edge.as_ref().unwrap().borrow().data);
        // all of the nodes go out of scope at this point and would normally be leaked.
    }

    // In this simple example, we always have cycles and our program is complete after this,
    // so we can't take advantage of the young generation picking up acyclic pointers without
    // tracing.
    collect_full();

    // All leaked nodes have been cleaned up!
}
