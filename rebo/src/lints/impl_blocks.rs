use crate::lints::visitor::Visitor;
use crate::parser::{ExprImplBlock, Spanned};
use diagnostic::Diagnostics;
use crate::common::MetaInfo;
use crate::error_codes::ErrorCode;

pub struct ImplBlockLints;

impl Visitor for ImplBlockLints {
    fn visit_impl_block(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, expr: &ExprImplBlock) {
        let ExprImplBlock { name, functions, .. } = expr;

        if functions.is_empty() {
            diagnostics.warning(ErrorCode::EmptyImplBlock)
                .with_error_label(expr.span(), "this impl is empty")
                .emit();
        }

        if meta_info.user_types.get(name.ident).is_none() {
            let similar = crate::util::similar_name(name.ident, meta_info.user_types.keys());
            let mut diag = diagnostics.error(ErrorCode::UnknownImplBlockTarget)
                .with_error_label(name.span, "can't find this type");
            if let Some(similar) = similar {
                diag = diag.with_info_label(name.span, format!("did you mean `{}`", similar));
            }
            diag.emit();
        }
    }
}