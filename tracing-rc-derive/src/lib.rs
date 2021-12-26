#![deny(missing_docs)]
//! Procedural macro for deriving a `Trace` implementation for the `tracing_rc` crate.

use proc_macro2::TokenStream;
use quote::quote;

const TRACE_FIELD_IDENT: &str = "trace";
const IGNORE_FIELD_IDENT: &str = "ignore";

fn trace_derive(mut s: synstructure::Structure) -> TokenStream {
    let mut errors = vec![];

    let mut has_specific_field = false;

    // All fields marked #[trace] but not #[trace(ignore)]
    let mut specific_fields = s.clone();
    let specific_fields = specific_fields.filter(|bi| {
        if bi.ast().attrs.iter().any(|attr| {
            attr.path.is_ident(TRACE_FIELD_IDENT) && attr.parse_args::<syn::Meta>().is_err()
        }) {
            has_specific_field = true;
            true
        } else {
            false
        }
    });

    let mut has_ignored = false;

    // All fields excluding #[trace(ignore)]
    let default_fields = s.filter(|bi| {
        let mut seen_attr = false;
        let mut should_keep = true;

        for attr in bi.ast().attrs.iter() {
            should_keep = if attr.path.is_ident(TRACE_FIELD_IDENT) {
                match attr.parse_args() {
                    Err(_) => true,
                    Ok(syn::Meta::Path(path)) => {
                        if !path.is_ident(IGNORE_FIELD_IDENT) {
                            errors.push(syn::parse::Error::new(
                                attr.pound_token.span,
                                "Only `#[trace]` and `#[trace(ignore)]` are supported on fields.",
                                ).to_compile_error(),
                            );
                        }

                        has_ignored = true;
                        false
                    }
                    _ => {
                        errors.push(
                            syn::parse::Error::new(
                                attr.pound_token.span,
                                "Only `#[trace]` and `#[trace(ignore)]` are supported on fields.",
                            )
                            .to_compile_error(),
                        );
                        false
                    }
                }
            } else {
                true
            };

            if seen_attr {
                errors.push(
                    syn::parse::Error::new(
                        attr.pound_token.span,
                        "Cannot specify multiple `#[trace]` attributes.",
                    )
                    .to_compile_error(),
                );
            }
            seen_attr = true;
        }

        should_keep
    });

    let (body, s) = if has_specific_field {
        (
            specific_fields.each(|bi| {
                quote! {
                    tracing_rc::rc::Trace::visit_children(#bi, visitor);
                }
            }),
            specific_fields,
        )
    } else {
        (
            default_fields.each(|bi| {
                quote! {
                    tracing_rc::rc::Trace::visit_children(#bi, visitor);
                }
            }),
            default_fields,
        )
    };

    let trace_impl = s.gen_impl(quote! {
        extern crate tracing_rc;

        gen impl tracing_rc::rc::Trace for @Self {
            fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                match *self { #body }
            }
        }
    });

    quote! {
        #trace_impl

        #(#errors)*
    }
}

synstructure::decl_derive! {
    [Trace, attributes(trace)] =>
    /// Derives a `Trace` implementation for a type.
    ///
    /// By default, this will call `Trace::visit_children` on all members of the type.
    /// - If you wish to only visit specific members, you may annotate them with `#[trace]`, and
    ///   only those members will be traced (other fields will be ignored).
    /// - If you wish to _ignore_ a specific field, you may annote it with `#[trace(ignore)]`.
    ///
    /// # Examples
    /// - Default:
    /// ```
    /// # use tracing_rc_derive::Trace;
    /// # use tracing_rc::rc::Gc;
    /// #[derive(Trace)]
    /// struct MyType {
    ///     default_traced_1: Gc<u64>,
    ///     default_traced_2: Gc<u64>,
    /// }
    /// ```
    /// - Tracing specific fields:
    /// ```
    /// # use tracing_rc_derive::Trace;
    /// # use tracing_rc::rc::Gc;
    /// #[derive(Trace)]
    /// struct MyType {
    ///     #[trace]
    ///     gc_field: Gc<u64>,
    ///
    ///     default_ignored: i64,
    /// }
    /// ```
    ///
    /// - Ignoring specific fields:
    /// ```
    /// # use tracing_rc_derive::Trace;
    /// # use tracing_rc::rc::Gc;
    /// #[derive(Trace)]
    /// struct MyType {
    ///     default_traced: Gc<u64>,
    ///
    ///     #[trace(ignore)]
    ///     ignored: i64,
    /// }
    /// ```
    ///
    trace_derive
}

#[test]
fn simple_impls_trace_default_all() {
    synstructure::test_derive! {
        trace_derive {
            #[derive(Trace)]
            struct Simple {
                a: tracing_rc::rc::Gc<i64>,

                b: i64,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_tracing_rc_rc_Trace_FOR_Simple: () = {
                extern crate tracing_rc;

                impl tracing_rc::rc::Trace for Simple {
                    fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                        match *self {
                            Simple {
                                a: ref __binding_0,
                                b: ref __binding_1,
                            } => {
                                { tracing_rc::rc::Trace::visit_children(__binding_0, visitor); }
                                { tracing_rc::rc::Trace::visit_children(__binding_1, visitor); }
                            }
                        }
                    }
                }
            };
        }
        no_build
    };
}

#[test]
fn simple_impls_trace_specific_field() {
    synstructure::test_derive! {
        trace_derive {
            #[derive(Trace)]
            struct Simple {
                #[trace]
                a: tracing_rc::rc::Gc<i64>,

                b: i64,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_tracing_rc_rc_Trace_FOR_Simple: () = {
                extern crate tracing_rc;

                impl tracing_rc::rc::Trace for Simple {
                    fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                        match *self {
                            Simple {
                                a: ref __binding_0,
                                ..
                            } => {
                                { tracing_rc::rc::Trace::visit_children(__binding_0, visitor); }
                            }
                        }
                    }
                }
            };
        }
        no_build
    };
}

#[test]
fn simple_impls_trace_specific_field_ignore() {
    synstructure::test_derive! {
        trace_derive {
            #[derive(Trace)]
            struct Simple {
                a: tracing_rc::rc::Gc<i64>,

                #[trace(ignore)]
                b: i64,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_tracing_rc_rc_Trace_FOR_Simple: () = {
                extern crate tracing_rc;

                impl tracing_rc::rc::Trace for Simple {
                    fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                        match *self {
                            Simple {
                                a: ref __binding_0,
                                ..
                            } => {
                                { tracing_rc::rc::Trace::visit_children(__binding_0, visitor); }
                            }
                        }
                    }
                }
            };
        }
        no_build
    };
}

#[test]
fn enum_impls_trace() {
    synstructure::test_derive! {
        trace_derive {
            #[derive(Trace)]
            enum AnEnum {
                None,
                Some(#[trace] tracing_rc::rc::Gc<i64>),
                StructLike {
                    non_gc: i64,
                    #[trace]
                    gc_field: tracing_rc::rc::Gc<i64>,
                },
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_tracing_rc_rc_Trace_FOR_AnEnum: () = {
                extern crate tracing_rc;

                impl tracing_rc::rc::Trace for AnEnum {
                    fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                        match *self {
                            AnEnum::None => {}
                            AnEnum::Some(ref __binding_0,) => {
                                { tracing_rc::rc::Trace::visit_children(__binding_0, visitor); }
                            }
                            AnEnum::StructLike{gc_field: ref __binding_1, ..} => {
                                { tracing_rc::rc::Trace::visit_children(__binding_1, visitor); }
                            }
                        }
                    }
                }
            };
        }
        no_build
    };
}

#[test]
fn generics_impl_trace_default_all() {
    synstructure::test_derive! {
        trace_derive {
            #[derive(Trace)]
            struct HasGenerics<T: Trace, U: SomeTrait> {
                a: Gc<T>,
                b: U,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_tracing_rc_rc_Trace_FOR_HasGenerics: () = {
                extern crate tracing_rc;

                impl<T: Trace, U: SomeTrait> tracing_rc::rc::Trace for HasGenerics<T, U>
                where
                    Gc<T>: tracing_rc::rc::Trace,
                    T: tracing_rc::rc::Trace,
                    U: tracing_rc::rc::Trace
                {
                    fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                        match *self {
                            HasGenerics{
                                a: ref __binding_0,
                                b: ref __binding_1,
                            } => {
                                { tracing_rc::rc::Trace::visit_children(__binding_0, visitor); }
                                { tracing_rc::rc::Trace::visit_children(__binding_1, visitor); }
                            }
                        }
                    }
                }
            };
        }
        no_build
    };
}

#[test]
fn generics_impl_trace_default_specific() {
    synstructure::test_derive! {
        trace_derive {
            #[derive(Trace)]
            struct HasGenerics<T: Trace, U: SomeTrait> {
                #[trace]
                gc_field: Gc<T>,

                non_gc: U,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_tracing_rc_rc_Trace_FOR_HasGenerics: () = {
                extern crate tracing_rc;

                impl<T: Trace, U: SomeTrait> tracing_rc::rc::Trace for HasGenerics<T, U>
                where
                    Gc<T>: tracing_rc::rc::Trace,
                    T: tracing_rc::rc::Trace
                {
                    fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                        match *self {
                            HasGenerics{gc_field: ref __binding_0, ..} => {
                                { tracing_rc::rc::Trace::visit_children(__binding_0, visitor); }
                            }
                        }
                    }
                }
            };
        }
        no_build
    };
}

#[test]
fn generics_impl_trace_default_specific_ignore() {
    synstructure::test_derive! {
        trace_derive {
            #[derive(Trace)]
            struct HasGenerics<T: Trace, U: SomeTrait> {
                gc_field: Gc<T>,

                #[trace(ignore)]
                non_gc: U,
            }
        }
        expands to {
            #[allow(non_upper_case_globals)]
            const _DERIVE_tracing_rc_rc_Trace_FOR_HasGenerics: () = {
                extern crate tracing_rc;

                impl<T: Trace, U: SomeTrait> tracing_rc::rc::Trace for HasGenerics<T, U>
                where
                    Gc<T>: tracing_rc::rc::Trace,
                    T: tracing_rc::rc::Trace
                {
                    fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                        match *self {
                            HasGenerics{gc_field: ref __binding_0, ..} => {
                                { tracing_rc::rc::Trace::visit_children(__binding_0, visitor); }
                            }
                        }
                    }
                }
            };
        }
        no_build
    };
}
