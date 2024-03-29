use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, BlockStack};
use crate::parser::{ExprStructDefinition, ExprType, Spanned};
use crate::error_codes::ErrorCode;

pub struct UnknownStructFieldType;

impl Visitor for UnknownStructFieldType {
    fn visit_struct_definition(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, def: &ExprStructDefinition) {
        let ExprStructDefinition { fields, .. } = def;

        for (field, _colon, typ) in fields {
            if let ExprType::UserType(s, _generics) = typ {
                if meta_info.user_types.get(s.ident).is_none() {
                    let mut diag = diagnostics.error(ErrorCode::UnknownStructFieldType)
                        .with_error_label(typ.span(), format!("unknown type `{}` of field `{}`", s.ident, field.ident));
                    if let Some(similar) = crate::util::similar_name(s.ident, meta_info.user_types.keys().copied()) {
                        diag = diag.with_info_label(typ.span(), format!("did you mean `{}`", similar));
                    }
                    diag.emit();
                }
            }
        }
    }
}
