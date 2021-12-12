use std::cell::RefCell;

use criterion::{
    black_box,
    criterion_group,
    criterion_main,
    BenchmarkId,
    Criterion,
};
use tracing_rc::rc::{
    collect_full,
    Gc,
    GcVisitor,
    Traceable,
};

struct SinglyLinked {
    parent: Option<Gc<RefCell<SinglyLinked>>>,
}

impl Traceable for SinglyLinked {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        self.parent.visit_children(visitor)
    }
}

struct DoublyLinked {
    parent: Option<Gc<RefCell<DoublyLinked>>>,
    child: Option<Gc<RefCell<DoublyLinked>>>,
}

impl Traceable for DoublyLinked {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        self.parent.visit_children(visitor);
        self.child.visit_children(visitor);
    }
}

fn simple_cycles(c: &mut Criterion) {
    let mut group = c.benchmark_group("Simple cycles");
    for cycle_length in [1, 10, 100, 1000] {
        group.bench_with_input(
            BenchmarkId::new("Singly-linked cycles", cycle_length),
            &cycle_length,
            |b, &length| {
                b.iter(|| {
                    {
                        let first = Gc::new(RefCell::new(SinglyLinked { parent: None }));
                        let mut next = first.clone();

                        for _ in 0..(length - 1) {
                            next = Gc::new(RefCell::new(SinglyLinked { parent: Some(next) }));
                        }
                        first.borrow_mut().parent = Some(next);
                        black_box(first);
                    }
                    collect_full();
                });
            },
        );
        group.bench_with_input(
            BenchmarkId::new("Doubly-linked cycles", cycle_length),
            &cycle_length,
            |b, &length| {
                b.iter(|| {
                    {
                        let first = Gc::new(RefCell::new(DoublyLinked {
                            child: None,
                            parent: None,
                        }));
                        let mut next = first.clone();

                        for _ in 0..(length - 1) {
                            let parent = next.clone();
                            next = Gc::new(RefCell::new(DoublyLinked {
                                parent: Some(next),
                                child: None,
                            }));
                            parent.borrow_mut().child = Some(next.clone());
                        }
                        next.borrow_mut().child = Some(first.clone());
                        first.borrow_mut().parent = Some(next);

                        black_box(first);
                    }
                    collect_full();
                });
            },
        );
    }
}

criterion_group!(cycle_collection, simple_cycles);
criterion_main!(cycle_collection);
