use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, BlockStack, Spanned};
use crate::error_codes::ErrorCode;
use indexmap::IndexMap;
use crate::parser::ExprEnumDefinition;

pub struct EnumDefInitLints;

impl Visitor for EnumDefInitLints {
    fn visit_enum_definition(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, expr: &ExprEnumDefinition) {
        let mut map = IndexMap::new();
        for variant in &expr.variants {
            if let Some(old_span) = map.insert(variant.name.ident, variant.name.span_with_id()) {
                diagnostics.error(ErrorCode::DuplicateEnumVariant)
                    .with_error_label(variant.name.diagnostics_span(), "duplicate enum variant")
                    .with_info_label(old_span.diagnostics_span(), "previously defined here")
                    .emit()
            }
        }
        // TODO: proper enum recursion recursing into all usertypes (structs, enums, ...)
    }
}