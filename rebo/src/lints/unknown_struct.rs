use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, BlockStack};
use crate::parser::ExprStructInitialization;
use crate::error_codes::ErrorCode;

pub struct UnknownStruct;

impl Visitor for UnknownStruct {
    fn visit_struct_initialization(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, init: &ExprStructInitialization) {
        let ExprStructInitialization { name, .. } = init;
        if meta_info.struct_types.get(name.ident).is_none() {
            let similar = crate::util::similar_name(name.ident, meta_info.struct_types.keys());
            let mut diag = diagnostics.error(ErrorCode::UnknownStruct)
                .with_error_label(name.span, "this struct doesn't exist");
            if let Some(similar) = similar {
                diag = diag.with_info_label(name.span, format!("did you mean `{}`", similar));
            }
            diag.emit();
        }
    }
}
