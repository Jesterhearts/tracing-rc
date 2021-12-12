pub mod rc;

/// Controls the style of collection carried out.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionType {
    /// Do a simple pass over the young gen, collecting non-cyclical pointers
    /// and moving old pointers to the old gen. Then perform a cycle-tracing
    /// collection over the old gen.
    Default,
    /// Only run collection for the young gen. This may still move pointers to the old gen if they
    /// qualify based on CollectOptions::old_gen_threshold
    YoungOnly,
}

impl Default for CollectionType {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CollectOptions {
    /// The number of times a pointer may be seen in the young gen before moving it to the old
    /// gen for a full tracing collection. Setting this to zero will cause all pointers to move to
    /// the old gen if they cannot be immediately cleaned up.
    pub old_gen_threshold: usize,
    pub kind: CollectionType,
}

impl CollectOptions {
    pub const DEFAULT: CollectOptions = CollectOptions {
        old_gen_threshold: 5,
        kind: CollectionType::Default,
    };
    pub const TRACE_AND_COLLECT_ALL: CollectOptions = Self::DEFAULT.set_old_gen_threshold(0);
    pub const YOUNG_ONLY: CollectOptions = Self::DEFAULT.set_kind(CollectionType::YoungOnly);

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
