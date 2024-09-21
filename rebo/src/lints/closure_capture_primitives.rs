use diagnostic::Diagnostics;
use rebo::lints::visitor::Visitor;
use rebo::Type;
use rebo::typeck::TypeVar;
use crate::common::Spanned;
use crate::{ErrorCode, MetaInfo};
use crate::common::BlockStack;
use crate::parser::ExprFunctionDefinition;

pub struct ClosureCapturePrimitives;

impl Visitor for ClosureCapturePrimitives {
    fn visit_function_definition(&self, diagnostic: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, fun: &ExprFunctionDefinition) {
        let ExprFunctionDefinition { sig, captures, body: _, span: _ } = fun;
        if sig.name.is_some() {
            return;
        }
        for binding in captures {
            if binding.mutable.is_none() {
                continue;
            }
            let typ = &meta_info.types[&TypeVar::from_spanned(binding.ident)];
            match typ {
                Type::Specific(spec) if !spec.is_primitive() => {
                    continue
                },
                _ => (),
            }
            diagnostic.error(ErrorCode::ClosureCapturesMutablePrimitive)
                .with_error_label(sig.span_(), format!("this function captures `{}`, which is a mutable primitive", binding.ident.ident))
                .with_info_label(binding.ident.span_(), "declared here")
                .with_note("primitives can't be captured mutably in closures as their value is copied into the closure")
                .with_note("if you want to modify a primitive from outside or within a closure, wrap it in a struct")
                .emit();
        }
    }
}