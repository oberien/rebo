use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{BlockStack, MetaInfo};
use crate::common::Spanned;
use crate::parser::{ExprStructDefinition, ExprType, ExprTypeUserType};
use crate::error_codes::ErrorCode;

pub struct UnknownStructFieldType;

impl Visitor for UnknownStructFieldType {
    fn visit_struct_definition(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, def: &ExprStructDefinition) {
        let ExprStructDefinition { fields, .. } = def;

        for (field, _colon, typ) in fields {
            if let ExprType::UserType(ExprTypeUserType { name, .. }) = typ {
                if meta_info.user_types.get(name.ident).is_none() {
                    let mut diag = diagnostics.error(ErrorCode::UnknownStructFieldType)
                        .with_error_label(typ.span_(), format!("unknown type `{}` of field `{}`", name.ident, field.ident));
                    if let Some(similar) = crate::util::similar_name(name.ident, meta_info.user_types.keys().copied()) {
                        diag = diag.with_info_label(typ.span_(), format!("did you mean `{}`", similar));
                    }
                    diag.emit();
                }
            }
        }
    }
}
