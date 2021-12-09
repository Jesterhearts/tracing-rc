# tracing-rc
Cycle collecting reference counted pointers for Rust with a safe api.


TODO: This does not yet expose a safe api, but I'm pretty confident at this point that a safe api
_is_ possible and still reasonably performant and effective at capturing leaked cycles.


# Notes on Safety
Because any implementation of the `Traceable` trait and custom `Drop` implementations of objects
owned by a garbage collected pointer can run arbitrary code, the collector is not able to trust that
reference counts will remain stable between calls to trace or drop. Client code may attempt to
produce new copies of gc pointers or tear down existing copies at any point.
