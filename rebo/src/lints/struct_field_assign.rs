use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{BlockStack, MetaInfo};
use crate::common::Spanned;
use crate::parser::{ExprAssign, ExprAssignLhs, ExprFieldAccess};
use crate::error_codes::ErrorCode;
use crate::typeck::TypeVar;
use crate::typeck::types::{SpecificType, Type};

pub struct StructFieldAssign;

impl Visitor for StructFieldAssign {
    fn visit_assign(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, ()>, expr: &ExprAssign) {
        let ExprAssign { lhs, .. } = expr;
        if let ExprAssignLhs::FieldAccess(expr) = lhs {
            check_non_struct_field_access(diagnostics, meta_info, expr);
        }
    }
}
fn check_non_struct_field_access(diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, expr: &ExprFieldAccess) {
    let ExprFieldAccess { variable, fields, .. } = expr;
    let mut typ = meta_info.types[&TypeVar::from_spanned(variable)].clone();
    let mut diagnostics_span = variable.diagnostics_span();
    for field in fields {
        let struct_name = match &typ {
            Type::Top | Type::Bottom => return,
            Type::Specific(SpecificType::Struct(name, _)) => name,
            _ => {
                diagnostics.error(ErrorCode::NonStructFieldAccess)
                    .with_error_label(diagnostics_span, format!("`{}` is of type `{}`, which is not a struct", diagnostics.resolve_span(diagnostics_span), typ))
                    .emit();
                return
            }
        };
        if struct_name == "struct" {
            diagnostics.error(ErrorCode::UnknownStruct)
                .with_error_label(diagnostics_span, "can't infer this struct type")
                .emit();
            return;
        }
        let struct_type = &meta_info.struct_types[struct_name.as_str()];
        let field_type = struct_type.get_field(field.ident);
        match field_type {
            Some(field_typ) => {
                typ = field_typ.clone();
                diagnostics_span = field.diagnostics_span();
            }
            None => diagnostics.error(ErrorCode::UnknownFieldAccess)
                .with_error_label(diagnostics_span, format!("tried to access non-existent field `{}` of `struct {}`", field.ident, struct_name))
                .emit(),
        }
    }
}
