use std::cell::RefCell;

use tracing_rc::{
    collect_full,
    GcPtr,
    Traceable,
};

struct GraphNode<T: 'static> {
    data: T,
    edge: Option<GcPtr<RefCell<GraphNode<T>>>>,
}

impl<T> Traceable for GraphNode<T> {
    fn visit_children(&self, visitor: &mut tracing_rc::GcVisitor) {
        self.edge.visit_children(visitor);
    }
}

fn main() {
    {
        let node_a = GcPtr::new(RefCell::new(GraphNode {
            data: 10,
            edge: None,
        }));
        let node_b = GcPtr::new(RefCell::new(GraphNode {
            data: 11,
            edge: Some(node_a.clone()),
        }));
        let node_c = GcPtr::new(RefCell::new(GraphNode {
            data: 12,
            edge: Some(node_b),
        }));

        node_a.borrow_mut().edge = Some(node_c);

        let a = node_a.borrow();
        let b = a.edge.as_ref().unwrap().borrow();
        let c = b.edge.as_ref().unwrap().borrow();

        assert_eq!(a.data, c.edge.as_ref().unwrap().borrow().data);
        // all of the nodes go out of scope at this point and would normally be leaked.
    }

    // In this simple example, we always have cycles our program is complete after this, so we can't
    // take advantage of the young generation picking up acyclic pointers without tracing.
    collect_full();

    // All leaked nodes have been cleaned up!
}
