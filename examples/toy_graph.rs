use std::cell::RefCell;

use tracing_rc::rc::{
    collect_full,
    Gc,
    GcVisitor,
    Traceable,
};

struct GraphNode<T: 'static> {
    data: T,
    edge: Option<Gc<RefCell<GraphNode<T>>>>,
}

impl<T> Traceable for GraphNode<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        self.edge.visit_children(visitor);
    }
}

fn main() {
    {
        let node_a = Gc::new(RefCell::new(GraphNode {
            data: 10,
            edge: None,
        }));
        let node_b = Gc::new(RefCell::new(GraphNode {
            data: 11,
            edge: None,
        }));
        let node_c = Gc::new(RefCell::new(GraphNode {
            data: 12,
            edge: Some(node_a.clone()),
        }));

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
