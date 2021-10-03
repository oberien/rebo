use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::MetaInfo;
use crate::parser::ExprStructInitialization;
use crate::error_codes::ErrorCode;
use indexmap::set::IndexSet;

pub struct StructInitializationFields;

impl Visitor for StructInitializationFields {
    fn visit_struct_initialization(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, init: &ExprStructInitialization) {
        let ExprStructInitialization { name, fields, .. } = init;
        if let Some(typ) = meta_info.struct_types.get(name.ident) {
            let def_span = meta_info.user_types[name.ident].span();
            let mut expected_fields: IndexSet<_> = typ.fields.iter().map(|(name, _typ)| name.as_str()).collect();
            for (field, _colon, _expr) in fields {
                if !expected_fields.remove(field.ident) {
                    let similar = crate::util::similar_name(field.ident, expected_fields.iter());
                    let mut diag = diagnostics.error(ErrorCode::UnknownFieldInit)
                        .with_error_label(field.span, format!("unknown field `{}`", field.ident))
                        .with_info_label(def_span, "defined here");
                    if let Some(similar) = similar {
                        diag = diag.with_info_label(field.span, format!("did you mean `{}`", similar));
                    }
                    diag.emit();
                }
            }
            if !expected_fields.is_empty() {
                let mut diag = diagnostics.error(ErrorCode::MissingField);
                for field in expected_fields {
                    diag = diag.with_error_label(name.span, format!("missing field `{}`", field));
                }
                diag.emit();
            }
        }
    }
}
