use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    filter_default_and_ignored_fields,
    filter_specific_fields,
};

pub(crate) fn trace_derive(mut all_fields: synstructure::Structure) -> TokenStream {
    let mut specific_fields = all_fields.clone();
    let has_specific_field = filter_specific_fields(&mut specific_fields);

    let errors = filter_default_and_ignored_fields(&mut all_fields);

    let (body, mut s) = if has_specific_field {
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
            all_fields.each(|bi| {
                quote! {
                    tracing_rc::rc::Trace::visit_children(#bi, visitor);
                }
            }),
            all_fields,
        )
    };

    let trace_impl = s.underscore_const(true).unbound_impl(
        quote!(tracing_rc::rc::Trace),
        quote! {
                fn visit_children(&self, visitor: &mut tracing_rc::rc::GcVisitor) {
                    match *self { #body }
                }
        },
    );

    quote! {
        #trace_impl

        #(#errors)*
    }
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
            const _: () = {
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
            const _: () = {
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
            const _: () = {
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
            const _: () = {
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
            const _: () = {
                extern crate tracing_rc;

                impl<T: Trace, U: SomeTrait> tracing_rc::rc::Trace for HasGenerics<T, U> {
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
            const _: () = {
                extern crate tracing_rc;

                impl<T: Trace, U: SomeTrait> tracing_rc::rc::Trace for HasGenerics<T, U> {
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
            const _: () = {
                extern crate tracing_rc;

                impl<T: Trace, U: SomeTrait> tracing_rc::rc::Trace for HasGenerics<T, U> {
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
