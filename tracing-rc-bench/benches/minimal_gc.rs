use std::{
    rc::Rc,
    time::{
        Duration,
        Instant,
    },
};

use criterion::{
    black_box,
    criterion_group,
    criterion_main,
    BenchmarkId,
    Criterion,
};
use tracing_rc::{
    rc::{
        collect_with_options,
        Gc,
    },
    CollectOptions,
};

fn no_gc_required(c: &mut Criterion) {
    let mut group = c.benchmark_group("Zero gc");
    group.bench_function("Rc sanity", |b| b.iter(|| Rc::new(black_box(20))));
    group.bench_function("Gc sanity", |b| b.iter(|| Gc::new(black_box(20))));
    group.bench_function("Rc single parent", |b| {
        b.iter(|| Rc::new(Rc::new(black_box(20))))
    });
    group.bench_function("Gc single parent", |b| {
        b.iter(|| {
            Gc::new(Gc::new(black_box(20)));
        })
    });
}

fn young_gen_only_gc(c: &mut Criterion) {
    let mut group = c.benchmark_group("Acyclic young gen");

    for child_count in [1, 10, 100, 1000] {
        group.bench_with_input(
            BenchmarkId::new("Rc cloned child", child_count),
            &child_count,
            |b, &count| {
                b.iter_custom(|iters| {
                    let mut total = Duration::ZERO;

                    for _ in 0..iters {
                        let mut vec = Vec::with_capacity(count);
                        for _ in 0..count {
                            vec.push(Rc::new(count));
                        }

                        let start = Instant::now();
                        {
                            (0..count).for_each(|i| {
                                Rc::new(black_box(vec[i].clone()));
                            });
                        }

                        black_box(vec);
                        total += start.elapsed()
                    }

                    total
                })
            },
        );

        group.bench_with_input(
            BenchmarkId::new("Gc cloned child", child_count),
            &child_count,
            |b, &count| {
                b.iter_custom(|iters| {
                    let mut total = Duration::ZERO;

                    for _ in 0..iters {
                        let mut vec = Vec::with_capacity(count);
                        for _ in 0..count {
                            vec.push(Gc::new(count));
                        }

                        let start = Instant::now();
                        {
                            (0..count).for_each(|i| {
                                Gc::new(black_box(vec[i].clone()));
                            });
                        }
                        black_box(vec);

                        collect_with_options(CollectOptions::YOUNG_ONLY);
                        total += start.elapsed()
                    }

                    total
                })
            },
        );
    }
}

criterion_group!(minimal_gc, no_gc_required, young_gen_only_gc);
criterion_main!(minimal_gc);
