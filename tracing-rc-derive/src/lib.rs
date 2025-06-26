#![deny(missing_docs)]
//! Procedural macro for deriving a `Trace` implementation for the `tracing_rc` crate.

use proc_macro2::TokenStream;

const TRACE_FIELD_IDENT: &str = "trace";
const IGNORE_FIELD_IDENT: &str = "ignore";

mod rc;
#[cfg(feature = "sync")]
mod sync;

fn filter_default_and_ignored_fields(
    default_fields: &mut synstructure::Structure,
) -> Vec<TokenStream> {
    let mut errors = vec![];

    default_fields.filter(|bi| {
        let mut seen_attr = false;
        let mut should_keep = true;

        for attr in bi.ast().attrs.iter() {
            should_keep = if attr.path().is_ident(TRACE_FIELD_IDENT) {
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

    errors
}

fn filter_specific_fields(specific_fields: &mut synstructure::Structure) -> bool {
    let mut has_specific_field = false;
    specific_fields.filter(|bi| {
        if bi.ast().attrs.iter().any(|attr| {
            attr.path().is_ident(TRACE_FIELD_IDENT) && attr.parse_args::<syn::Meta>().is_err()
        }) {
            has_specific_field = true;
            true
        } else {
            false
        }
    });

    has_specific_field
}

synstructure::decl_derive! {
    [Trace, attributes(trace)] =>
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
    rc::trace_derive
}

#[cfg(feature = "sync")]
synstructure::decl_derive! {
    [SyncTrace, attributes(trace)] =>
    /// By default, this will call `Trace::visit_children` on all members of the type.
    /// - If you wish to only visit specific members, you may annotate them with `#[trace]`, and
    ///   only those members will be traced (other fields will be ignored).
    /// - If you wish to _ignore_ a specific field, you may annote it with `#[trace(ignore)]`.
    ///
    /// # Examples
    /// - Default:
    /// ```
    /// # use tracing_rc_derive::SyncTrace;
    /// # use tracing_rc::sync::Agc;
    /// #[derive(SyncTrace)]
    /// struct MyType {
    ///     default_traced_1: Agc<u64>,
    ///     default_traced_2: Agc<u64>,
    /// }
    /// ```
    /// - Tracing specific fields:
    /// ```
    /// # use tracing_rc_derive::SyncTrace;
    /// # use tracing_rc::sync::Agc;
    /// #[derive(SyncTrace)]
    /// struct MyType {
    ///     #[trace]
    ///     gc_field: Agc<u64>,
    ///
    ///     default_ignored: i64,
    /// }
    /// ```
    ///
    /// - Ignoring specific fields:
    /// ```
    /// # use tracing_rc_derive::SyncTrace;
    /// # use tracing_rc::sync::Agc;
    /// #[derive(SyncTrace)]
    /// struct MyType {
    ///     default_traced: Agc<u64>,
    ///
    ///     #[trace(ignore)]
    ///     ignored: i64,
    /// }
    /// ```
    ///
    sync::trace_derive
}
