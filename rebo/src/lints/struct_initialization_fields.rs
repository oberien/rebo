use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{BlockStack, MetaInfo};
use crate::parser::ExprStructInitialization;
use crate::error_codes::ErrorCode;
use indexmap::set::IndexSet;
use crate::common::Spanned;

pub struct StructInitializationFields;

impl Visitor for StructInitializationFields {
    fn visit_struct_initialization(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, init: &ExprStructInitialization) {
        let ExprStructInitialization { name, fields, .. } = init;
        if name.ident == "List" {
            diagnostics.error(ErrorCode::ListStructInitialization)
                .with_error_label(init.diagnostics_span(), "List can't be initialized")
                .with_info_label(init.diagnostics_span(), "use `List::new()` instead")
                .emit();
            return;
        }

        if let Some(typ) = meta_info.struct_types.get(name.ident) {
            let def_diagnostics_span = meta_info.user_types[name.ident].diagnostics_span();
            let mut expected_fields: IndexSet<_> = typ.fields.iter().map(|(name, _typ)| name.as_str()).collect();
            for (field, _colon, _expr) in fields {
                if !expected_fields.remove(field.ident) {
                    let similar = crate::util::similar_name(field.ident, expected_fields.iter());
                    let mut diag = diagnostics.error(ErrorCode::UnknownFieldInit)
                        .with_error_label(field.diagnostics_span(), format!("unknown field `{}`", field.ident))
                        .with_info_label(def_diagnostics_span, "defined here");
                    if let Some(similar) = similar {
                        diag = diag.with_info_label(field.diagnostics_span(), format!("did you mean `{}`", similar));
                    }
                    diag.emit();
                }
            }
            if !expected_fields.is_empty() {
                let mut diag = diagnostics.error(ErrorCode::MissingField);
                for field in expected_fields {
                    diag = diag.with_error_label(name.diagnostics_span(), format!("missing field `{}`", field));
                }
                diag.emit();
            }
        }
    }
}
