#![deny(missing_docs)]

//! Cycle collecting reference counted pointers for Rust with a safe api.
//!
//! This crate is most useful when you have a data structure with inter-dependent nodes of
//! arbitrary lifetimes and no clear parent-children relationships.
//!
//! If you can be certain that the layout of your data will be acyclic, or [`std::rc::Weak`]
//! would be sufficient to prevent leaks, this type is probably not a good fit. If you know all
//! of your data will be dropped after a certain phase of your program completes, you will
//! probably prefer arena allocation through a crate like `typed-arena`, `generational-arena`,
//! etc.
//!
//! # Basic Example
//! ```
//! # use tracing_rc::rc::{
//! #     collect_full,
//! #     Gc,
//! #     GcVisitor,
//! #     Trace,
//! # };
//! #
//! struct GraphNode<T: 'static> {
//!     data: T,
//!     edge: Option<Gc<GraphNode<T>>>,
//! }
//!
//! impl<T> Trace for GraphNode<T> {
//!     fn visit_children(&self, visitor: &mut GcVisitor) {
//!         self.edge.visit_children(visitor);
//!     }
//! }
//!
//! # fn main() {
//! {
//!     let node_a = Gc::new(GraphNode {
//!         data: 10,
//!         edge: None,
//!     });
//!     let node_b = Gc::new(GraphNode {
//!         data: 11,
//!         edge: None,
//!     });
//!     let node_c = Gc::new(GraphNode {
//!         data: 12,
//!         edge: Some(node_a.clone()),
//!     });
//!
//!     node_a.borrow_mut().edge = Some(node_b.clone());
//!     node_b.borrow_mut().edge = Some(node_c);
//!
//!     let a = node_a.borrow();
//!     let b = a.edge.as_ref().unwrap().borrow();
//!     let c = b.edge.as_ref().unwrap().borrow();
//!
//!     assert_eq!(a.data, c.edge.as_ref().unwrap().borrow().data);
//!     // all of the nodes go out of scope at this point and would normally be leaked.
//! }
//!
//! // In this simple example, we always have cycles and our program is complete after this,
//! // so we can't take advantage of the young generation picking up acyclic pointers without
//! // tracing.
//! collect_full();
//!
//! // All leaked nodes have been cleaned up!
//! # }
//! ```

/// A non-sync cycle-collecting reference-counted smart pointer.
pub mod rc;

/// Controls the style of collection carried out.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum CollectionType {
    /// Do a simple pass over the young gen, collecting non-cyclical pointers
    /// and moving old pointers to the old gen. Then perform a cycle-tracing
    /// collection over the old gen.
    Default,
    /// Only run collection for the young gen. This may still move pointers to the old gen if they
    /// qualify based on [`CollectOptions::old_gen_threshold`]
    YoungOnly,
}

impl Default for CollectionType {
    fn default() -> Self {
        Self::Default
    }
}

/// Provides settings which control how cycle-collection is performed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub struct CollectOptions {
    /// The number of times a pointer may be seen in the young gen before moving it to the old
    /// gen for a full tracing collection. Setting this to zero will cause all pointers to move to
    /// the old gen if they cannot be immediately cleaned up.
    pub old_gen_threshold: usize,
    /// The kind of collection to perform, e.g. just the young gen, or full tracing of both old &
    /// young gen.
    pub kind: CollectionType,
}

impl CollectOptions {
    /// The default options for cycle collection. Items remain in the young gen for 5 cycles, and
    /// both old and young gen will be process for each collection. These options will be used when
    /// calling `collect`
    pub const DEFAULT: Self = Self {
        old_gen_threshold: 5,
        kind: CollectionType::Default,
    };
    /// Forces tracing collection for all items currently awaiting cleanup.
    pub const TRACE_AND_COLLECT_ALL: Self = Self::DEFAULT.set_old_gen_threshold(0);
    /// Only runs collection for the young generation. This will still move old items to the old
    /// gen.
    pub const YOUNG_ONLY: Self = Self::DEFAULT.set_kind(CollectionType::YoungOnly);

    /// Alter the [`CollectionType`] performed when calling `collect_with_options`.
    #[must_use]
    pub const fn set_kind(self, kind: CollectionType) -> Self {
        let Self {
            old_gen_threshold,
            kind: _,
        } = self;

        Self {
            old_gen_threshold,
            kind,
        }
    }

    /// Alter the number of times an item may be seen in the young generation before being moved to
    /// the old generation and traced.
    #[must_use]
    pub const fn set_old_gen_threshold(self, threshold: usize) -> Self {
        let Self {
            old_gen_threshold: _,
            kind,
        } = self;

        Self {
            old_gen_threshold: threshold,
            kind,
        }
    }
}

impl Default for CollectOptions {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Status {
    Live,
    RecentlyDecremented,
    Dead,
}
