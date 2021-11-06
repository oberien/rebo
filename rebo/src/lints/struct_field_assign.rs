use std::ops::Deref;
use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::MetaInfo;
use crate::parser::{ExprFieldAccess, ExprAssign, ExprAssignLhs, Spanned};
use crate::error_codes::ErrorCode;
use crate::typeck::TypeVar;
use crate::typeck::types::{Type, SpecificType};

pub struct StructFieldAssign;

impl Visitor for StructFieldAssign {
    fn visit_assign(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, expr: &ExprAssign) {
        let ExprAssign { lhs, .. } = expr;
        if let ExprAssignLhs::FieldAccess(expr) = lhs {
            check_non_struct_field_access(diagnostics, meta_info, expr);
        }
    }
}
fn check_non_struct_field_access(diagnostics: &Diagnostics, meta_info: &MetaInfo, expr: &ExprFieldAccess) {
    let ExprFieldAccess { variable, fields, .. } = expr;
    let mut typ = meta_info.types[&TypeVar::new(variable.binding.ident.span)].clone();
    let mut span = variable.span();
    for field in fields {
        let struct_name = match &typ {
            Type::Top | Type::Bottom => return,
            Type::Specific(SpecificType::Struct(name, _)) => name,
            _ => {
                diagnostics.error(ErrorCode::NonStructFieldAccess)
                    .with_error_label(span, format!("`{}` is of type `{}`, which is not a struct", diagnostics.resolve_span(span), typ))
                    .emit();
                return
            }
        };
        let struct_type = &meta_info.struct_types[struct_name.deref()];
        let field_type = struct_type.get_field(field.ident);
        match field_type {
            Some(field_typ) => {
                typ = field_typ.clone();
                span = field.span;
            }
            None => diagnostics.error(ErrorCode::UnknownFieldAccess)
                .with_error_label(span, format!("tried to access non-existent field `{}` of `struct {}`", field.ident, struct_name))
                .emit(),
        }
    }
}
