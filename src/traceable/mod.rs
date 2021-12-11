use crate::GcVisitor;

/// Must be implemented for any value which will be stored inside of a Gc.
///
/// While this is implemented for many of Rust's basic types, it's not
/// recommended that you store them in a Gc, as there is still a real
/// cost to doing so. You're probably better off using std::rc.
pub trait Traceable {
    /// Visit the gc pointers owned by this type.
    ///
    /// It is recommended that you simply call visit_children(visitor) on each value owned by the
    /// implementor which may participate in a reference cycle. The default implementation for
    /// `Gc` will appropriately notify the collector when it is visited. You may also pass your
    /// struct's owned `Gc` values directly to the visitor.
    ///
    /// Impromper implementation of this trait will not cause undefined behavior, however, if you
    /// fail to report a value you may leak memory and if you report a value you don't own (or
    /// report a value more than once), you may cause the collector to clean it up prematurely.
    /// Attemting to access a value which has been cleaned up will cause a panic, but will not cause
    /// undefined behavior.
    ///
    /// - You should not report Gcs owned by the inner contents of Gcs.
    /// ```
    /// use tracing_rc::{
    ///     Gc,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// struct MyStruct {
    ///     ptr: Gc<MyStruct>,
    ///     other_ptr: Gc<MyStruct>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor.visit_node(&self.ptr);
    ///         // This is also acceptable
    ///         self.other_ptr.visit_children(visitor);
    ///
    ///         // This will not cause undefined behavior, but it is wrong and may cause panics.
    ///         self.ptr.ptr.visit_children(visitor);
    ///     }
    /// }
    /// ```
    /// - You should not report a unique Gc instance twice.
    /// ```
    /// use tracing_rc::{
    ///     Gc,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// struct MyStruct {
    ///     ptr: Gc<usize>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor.visit_node(&self.ptr);
    ///
    ///         // This is wrong and may cause panics.
    ///         visitor.visit_node(&self.ptr);
    ///     }
    /// }
    /// ```
    /// - You should not report Gcs that are not owned by your object.
    ///     - It is acceptable skip reporting, although doing so will result in memory leaks.
    /// ```
    /// use tracing_rc::{
    ///     Gc,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// thread_local! { static GLOBAL_PTR: Gc<usize> = Gc::new(10)}
    ///
    /// struct MyStruct {
    ///     ptr: Gc<MyStruct>,
    ///     leaks: Gc<usize>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor.visit_node(&self.ptr);
    ///
    ///         // Leaving this line commented out will leak, which is safe.
    ///         // Uncommenting it is safe and will allow leaks to be cleaned up.
    ///         // visitor(self.leaks.node());
    ///
    ///         // This is wrong and will cause GLOBAL_PTR to be cleaned up. If anything tries to
    ///         // access GLOBAL_PTR without checking if it is still alive, a panic will occur.
    ///         GLOBAL_PTR.with(|ptr| visitor.visit_node(ptr));
    ///     }
    /// }
    /// ```
    fn visit_children(&self, visitor: &mut GcVisitor);
}

/// This will cause memory leaks if it is used to implement tracing on a type which ends up
/// participating in a cycle. You probably don't want to implement this, since if you know that
/// your type cannot participate in a cycle, you should probably just use std::rc.
#[macro_export]
macro_rules!  empty_traceable {
    ($t:path) => {
        impl Traceable for $t {
            fn visit_children(&self, _: &mut GcVisitor) {}
        }
    };
    ($first:path, $($rest:path),+) => {
        empty_traceable!($first);
        empty_traceable!($($rest),+);
    };
}

empty_traceable!(f32, f64);
empty_traceable!(i8, i16, i32, i64, isize, i128);
empty_traceable!(u8, u16, u32, u64, usize, u128);
empty_traceable!(bool, char);
empty_traceable!(std::string::String);

impl<T: Traceable> Traceable for std::cell::RefCell<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        T::visit_children(&self.borrow(), visitor);
    }
}

impl<T: Traceable> Traceable for std::option::Option<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        if let Some(inner) = self {
            inner.visit_children(visitor);
        }
    }
}

impl<T: Traceable> Traceable for std::vec::Vec<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<T: Traceable> Traceable for std::boxed::Box<T> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        T::visit_children(self, visitor)
    }
}

impl<T: Traceable, const S: usize> Traceable for [T; S] {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<T: Traceable> Traceable for [T] {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::BinaryHeap<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<K: Traceable, V: Traceable> Traceable for std::collections::BTreeMap<K, V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for (k, v) in self.iter() {
            k.visit_children(visitor);
            v.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::BTreeSet<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<K: Traceable, V: Traceable> Traceable for std::collections::HashMap<K, V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for (k, v) in self.iter() {
            k.visit_children(visitor);
            v.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::HashSet<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::LinkedList<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::VecDeque<V> {
    fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}
