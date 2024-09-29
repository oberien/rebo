use crate::lints::visitor::Visitor;
use crate::parser::ExprYield;
use diagnostic::Diagnostics;
use crate::common::{BlockStack, MetaInfo};
use crate::common::Spanned;
use crate::error_codes::ErrorCode;

pub struct ImplBlockLints;

impl Visitor for ImplBlockLints {
    fn visit_yield(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, ()>, expr: &ExprYield) {
        // ExprYield within generators has already been transformed within the parser upon
        // encountering the generator function definition.
        // Any ExprYield we encounter here is outside of a generator and thus wrong.

        diagnostics.error(ErrorCode::YieldOutsideOfGenerator)
            .with_error_label(expr.diagnostics_span(), "yield may only be used within a generator")
            .emit();
    }
}
