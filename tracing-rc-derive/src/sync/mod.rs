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
                    tracing_rc::sync::Trace::visit_children(#bi, visitor);
                }
            }),
            specific_fields,
        )
    } else {
        (
            all_fields.each(|bi| {
                quote! {
                    tracing_rc::sync::Trace::visit_children(#bi, visitor);
                }
            }),
            all_fields,
        )
    };

    let trace_impl = s.underscore_const(true).unbound_impl(
        quote!(tracing_rc::sync::Trace),
        quote! {
                fn visit_children(&self, visitor: &mut tracing_rc::sync::GcVisitor) {
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
                a: tracing_rc::sync::Agc<i64>,

                b: i64,
            }
        }
        expands to {
            const _: () = {
                extern crate tracing_rc;

                impl tracing_rc::sync::Trace for Simple {
                    fn visit_children(&self, visitor: &mut tracing_rc::sync::GcVisitor) {
                        match *self {
                            Simple {
                                a: ref __binding_0,
                                b: ref __binding_1,
                            } => {
                                { tracing_rc::sync::Trace::visit_children(__binding_0, visitor); }
                                { tracing_rc::sync::Trace::visit_children(__binding_1, visitor); }
                            }
                        }
                    }
                }
            };
        }
        no_build
    };
}
