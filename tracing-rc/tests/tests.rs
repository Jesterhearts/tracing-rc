mod rc {
    mod acyclic;
    mod cycles;
    mod fuzzer_found;
    mod necromancy;
}

/// These tests must be run with `test-threads=1`
#[cfg(feature = "sync")]
mod sync {
    mod cycles;
    mod fuzzer_found;
}

#[cfg(not(miri))]
mod bad_tracing {
    mod rc;
}
