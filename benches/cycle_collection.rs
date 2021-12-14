use std::time::{
    Duration,
    Instant,
};

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
    parent: Option<Gc<SinglyLinked>>,
}

impl Traceable for SinglyLinked {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        self.parent.visit_children(visitor)
    }
}

struct DoublyLinked {
    parent: Option<Gc<DoublyLinked>>,
    child: Option<Gc<DoublyLinked>>,
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
                b.iter_custom(|iters| {
                    let mut total = Duration::ZERO;

                    for _ in 0..iters {
                        {
                            let first = Gc::new(SinglyLinked { parent: None });
                            let mut next = first.clone();

                            for _ in 0..(length - 1) {
                                next = Gc::new(SinglyLinked { parent: Some(next) });
                            }
                            first.borrow_mut().parent = Some(next);
                            black_box(first);
                        }
                        let start = Instant::now();
                        collect_full();
                        total += start.elapsed();
                    }

                    total
                });
            },
        );
        group.bench_with_input(
            BenchmarkId::new("Doubly-linked cycles", cycle_length),
            &cycle_length,
            |b, &length| {
                b.iter_custom(|iters| {
                    let mut total = Duration::ZERO;
                    for _ in 0..iters {
                        {
                            let first = Gc::new(DoublyLinked {
                                child: None,
                                parent: None,
                            });
                            let mut next = first.clone();

                            for _ in 0..(length - 1) {
                                let parent = next.clone();
                                next = Gc::new(DoublyLinked {
                                    parent: Some(next),
                                    child: None,
                                });
                                parent.borrow_mut().child = Some(next.clone());
                            }
                            next.borrow_mut().child = Some(first.clone());
                            first.borrow_mut().parent = Some(next);

                            black_box(first);
                        }
                        let start = Instant::now();
                        collect_full();
                        total += start.elapsed();
                    }

                    total
                });
            },
        );
    }
}

#[derive(Default)]
struct Maximal {
    neighbors: Vec<Gc<Maximal>>,
}

impl Traceable for Maximal {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        self.neighbors.visit_children(visitor);
    }
}

fn maximally_dense(c: &mut Criterion) {
    let mut group = c.benchmark_group("Maximal density cycles");
    for cycle_length in [10, 35, 85, 110] {
        group.bench_with_input(
            BenchmarkId::from_parameter(cycle_length),
            &cycle_length,
            |b, &length| {
                b.iter_custom(|iters| {
                    let mut total = Duration::ZERO;

                    for _ in 0..iters {
                        {
                            let mut nodes = Vec::with_capacity(length);
                            for _ in 0..length {
                                nodes.push(Gc::new(Maximal::default()));
                            }

                            for node in nodes.iter() {
                                node.borrow_mut().neighbors = nodes.clone();
                            }

                            black_box(nodes);
                        }

                        let start = Instant::now();
                        collect_full();
                        total += start.elapsed()
                    }
                    total
                });
            },
        );
    }
}

#[cfg(not(debug_assertions))]
criterion_group!(cycle_collection, simple_cycles, maximally_dense);
#[cfg(not(debug_assertions))]
criterion_main!(cycle_collection);

#[cfg(debug_assertions)]
fn main() {}
