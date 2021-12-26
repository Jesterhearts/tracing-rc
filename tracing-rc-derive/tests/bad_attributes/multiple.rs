use tracing_rc_derive::Trace;

#[derive(Trace)]
struct Fails {
    #[trace]
    #[trace]
    item: i64,
}

fn main() {}
