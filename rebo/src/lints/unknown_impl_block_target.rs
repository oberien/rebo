use crate::lints::visitor::Visitor;
use crate::parser::ExprImplBlock;
use diagnostic::Diagnostics;
use crate::common::MetaInfo;
use crate::error_codes::ErrorCode;

pub struct UnknownImplBlockTarget;

impl Visitor for UnknownImplBlockTarget {
    fn visit_impl_block(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, expr: &ExprImplBlock) {
        let ExprImplBlock { name, .. } = expr;
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