use crate::sync::GcVisitor;

/// The `Sync` version of the [`rc::Trace`](crate::rc::Trace) trait.
pub trait Trace: Send + Sync {
    /// Visit the children of this type. The implementation should call `visit_children` on each
    /// owned value which implements [`Trace`].
    fn visit_children(&self, visitor: &mut GcVisitor);
}

/// Implements a no-op [`Trace`] for a type.
///
/// This will cause memory leaks if it is used to implement tracing on a type which ends up
/// participating in a cycle. This is useful for types that are e.g. used as a key in
/// [`std::collections::HashMap`], but are not actually `Agc` pointers.
#[macro_export]
macro_rules! sync_empty_trace {
    ($t:path) => {
        impl $crate::sync::Trace for $t {
            #[inline]
            fn visit_children(&self, _: &mut $crate::sync::GcVisitor) {}
        }
    };
    ($first:path, $($rest:path),+) => {
        sync_empty_trace!($first);
        sync_empty_trace!($($rest),+);
    };
}

sync_empty_trace!(f32, f64);
sync_empty_trace!(i8, i16, i32, i64, isize, i128);
sync_empty_trace!(u8, u16, u32, u64, usize, u128);
sync_empty_trace!(bool, char);
sync_empty_trace!(std::string::String);

impl Trace for () {
    fn visit_children(&self, _: &mut GcVisitor) {}
}

impl<T: Trace> Trace for std::option::Option<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        if let Some(inner) = self {
            inner.visit_children(visitor);
        }
    }
}

impl<T: Trace> Trace for std::vec::Vec<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<T: Trace> Trace for std::boxed::Box<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        T::visit_children(self, visitor);
    }
}

impl<T: Trace, const S: usize> Trace for [T; S] {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<T: Trace> Trace for [T] {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<V: Trace> Trace for std::collections::BinaryHeap<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<K: Trace, V: Trace> Trace for std::collections::BTreeMap<K, V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for (k, v) in self.iter() {
            k.visit_children(visitor);
            v.visit_children(visitor);
        }
    }
}

impl<V: Trace> Trace for std::collections::BTreeSet<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<K: Trace, V: Trace, S: std::hash::BuildHasher + Send + Sync> Trace
    for std::collections::HashMap<K, V, S>
{
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for (k, v) in self.iter() {
            k.visit_children(visitor);
            v.visit_children(visitor);
        }
    }
}

impl<V: Trace, S: std::hash::BuildHasher + Send + Sync> Trace for std::collections::HashSet<V, S> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<V: Trace> Trace for std::collections::LinkedList<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<V: Trace> Trace for std::collections::VecDeque<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<T: Trace> Trace for std::sync::Mutex<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        if let Ok(guard) = self.try_lock() {
            guard.visit_children(visitor);
        }
    }
}

impl<T: Trace> Trace for std::sync::RwLock<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        if let Ok(guard) = self.try_read() {
            guard.visit_children(visitor);
        }
    }
}
