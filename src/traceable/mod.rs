use crate::GcVisitor;

/// Must be implemented for any value which will be stored inside of a GcPtr.
///
/// While this is implemented for many of Rust's basic types, it's not
/// recommended that you store them in a GcPtr, as there is still a real
/// cost to doing so. You're probably better off using std::rc.
pub trait Traceable {
    /// Visit the gc pointers owned by this type or its decendents.
    ///
    /// This function is marked unsafe as improper implementation can lead to
    /// undefined behavior.
    ///
    /// # Safety
    /// - You MUST NOT report GcPtrs owned by the inner contents of GcPtrs.
    /// ```
    /// use tracing_rc::{
    ///     GcPtr,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// struct MyStruct {
    ///     ptr: GcPtr<MyStruct>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor(self.ptr.node());
    ///
    ///         // This is bad and will cause undefined behavior
    ///         // visitor(self.ptr.ptr.node());
    ///     }
    /// }
    /// ```
    /// - You MUST NOT report a unique GcPtr instance twice.
    /// ```
    /// use tracing_rc::{
    ///     GcPtr,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// struct MyStruct {
    ///     ptr: GcPtr<usize>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor(self.ptr.node());
    ///
    ///         // This is bad and will cause undefined behavior
    ///         // visitor(self.ptr.node());
    ///     }
    /// }
    /// ```
    /// - You MUST NOT report GcPtrs that are not owned by your object or its decendents.
    ///     - It is acceptable skip reporting, although doing so will result in memory leaks.
    /// ```
    /// use tracing_rc::{
    ///     GcPtr,
    ///     GcVisitor,
    ///     Traceable,
    /// };
    ///
    /// thread_local! { static GLOBAL_PTR: GcPtr<usize> = GcPtr::new(10)}
    ///
    /// struct MyStruct {
    ///     ptr: GcPtr<MyStruct>,
    ///     leaks: GcPtr<usize>,
    /// }
    ///
    /// impl Traceable for MyStruct {
    ///     unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
    ///         // This is normal and ok.
    ///         visitor(self.ptr.node());
    ///
    ///         // Leaving this line commented out will leak, which is safe.
    ///         // Uncommenting it is safe and will allow leaks to be cleaned up.
    ///         // visitor(self.leaks.node());
    ///
    ///         // This is bad and will cause undefined behavior
    ///         // GLOBAL_PTR.with(|ptr| visitor(ptr.node()));
    ///     }
    /// }
    /// ```
    unsafe fn visit_children(&self, visitor: &mut GcVisitor);
}

/// This is safe to use, as it does not visit any children and thust cannot
/// cause a pointer to be dropped erroneously. It will cause memory leaks if it
/// is used to implement tracing on a type which ends up participating in a
/// cycle.
///
/// Despite its safety, you probably don't want to implement this anyways, since
/// if you know that your type cannot participate in a cycle, you should
/// probably just use std::rc.
#[macro_export]
macro_rules!  empty_traceable {
    ($t:path) => {
        impl Traceable for $t {
            unsafe fn visit_children(&self, _: &mut GcVisitor) {}
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
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        T::visit_children(&self.borrow(), visitor);
    }
}

impl<T: Traceable> Traceable for std::option::Option<T> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        if let Some(inner) = self {
            inner.visit_children(visitor);
        }
    }
}

impl<T: Traceable> Traceable for std::vec::Vec<T> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<T: Traceable> Traceable for std::boxed::Box<T> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        T::visit_children(self, visitor)
    }
}

impl<T: Traceable, const S: usize> Traceable for [T; S] {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<T: Traceable> Traceable for [T] {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for elem in self.iter() {
            elem.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::BinaryHeap<V> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<K: Traceable, V: Traceable> Traceable for std::collections::BTreeMap<K, V> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for (k, v) in self.iter() {
            k.visit_children(visitor);
            v.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::BTreeSet<V> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<K: Traceable, V: Traceable> Traceable for std::collections::HashMap<K, V> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for (k, v) in self.iter() {
            k.visit_children(visitor);
            v.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::HashSet<V> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::LinkedList<V> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}

impl<V: Traceable> Traceable for std::collections::VecDeque<V> {
    unsafe fn visit_children(&self, visitor: &mut GcVisitor) {
        for v in self.iter() {
            v.visit_children(visitor);
        }
    }
}
