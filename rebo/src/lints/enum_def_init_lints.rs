use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, UserType, BlockStack};
use crate::error_codes::ErrorCode;
use indexmap::IndexMap;
use crate::parser::{ExprEnumDefinition, ExprEnumInitialization};

pub struct EnumDefInitLints;

impl Visitor for EnumDefInitLints {
    fn visit_enum_definition(&self, diagnostics: &Diagnostics, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, expr: &ExprEnumDefinition) {
        let mut map = IndexMap::new();
        for variant in &expr.variants {
            if let Some(old_span) = map.insert(variant.name.ident, variant.name.span) {
                diagnostics.error(ErrorCode::DuplicateEnumVariant)
                    .with_error_label(variant.name.span, "duplicate enum variant")
                    .with_info_label(old_span, "previously defined here")
                    .emit()
            }
        }
        // TODO: proper enum recursion recursing into all usertypes (structs, enums, ...)
    }

    fn visit_enum_initialization(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, ei: &ExprEnumInitialization) {
        let ExprEnumInitialization { enum_name, variant_name, .. } = ei;
        let enum_def = meta_info.user_types.get(enum_name.ident);
        let enum_def = match enum_def {
            Some(UserType::Enum(enum_def)) => enum_def,
            Some(UserType::Struct(_)) | None => {
                let similar = crate::util::similar_name(enum_name.ident, meta_info.enum_types.keys());
                let mut diag = diagnostics.error(ErrorCode::UnknownEnum)
                    .with_error_label(enum_name.span, "can't find this enum");
                if let Some(similar) = similar {
                    diag = diag.with_info_label(enum_name.span, format!("did you mean `{}`", similar));
                }
                diag.emit();
                return
            }
        };
        let variant = enum_def.variants.iter().find(|variant| variant.name.ident == variant_name.ident);
        let variant = match variant {
            Some(variant) => variant,
            None => {
                let variant_name_iter = enum_def.variants.iter().map(|variant| variant.name.ident);
                let similar = crate::util::similar_name(variant_name.ident, variant_name_iter);
                let mut diag = diagnostics.error(ErrorCode::UnknownEnumVariant)
                    .with_error_label(variant_name.span, "can't find this enum variant");
                if let Some(similar) = similar {
                    diag = diag.with_info_label(variant_name.span, format!("did you mean `{}`", similar));
                }
                diag.emit();
                return
            }
        };
        if let Some(fields) = &variant.fields {
            diagnostics.error(ErrorCode::InvalidNumberOfEnumVariantFields)
                .with_error_label(variant_name.span, format!("expected {} fields but got none", fields.1.len()))
                .with_info_label(variant.name.span, "enum variant defined here")
                .emit();
        }
    }
}