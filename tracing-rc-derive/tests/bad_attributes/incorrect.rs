use tracing_rc_derive::Trace;

#[derive(Trace)]
struct Fails {
    #[trace(not_an_arg)]
    item: i64,
}

fn main() {}
