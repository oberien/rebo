use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::MetaInfo;
use crate::parser::{ExprAssign, ExprAssignLhs, ExprFieldAccess, Spanned};
use crate::error_codes::ErrorCode;

pub struct ImmutableAssign;

impl Visitor for ImmutableAssign {
    fn visit_assign(&self, diagnostics: &Diagnostics, _: &MetaInfo, assign: &ExprAssign) {
        let ExprAssign { lhs, .. } = assign;
        let variable = match lhs {
            ExprAssignLhs::Variable(variable) => variable,
            ExprAssignLhs::FieldAccess(ExprFieldAccess { variable, .. }) => variable,
        };
        if variable.binding.mutable.is_none() {
            diagnostics.error(ErrorCode::ImmutableAssign)
                .with_error_label(variable.span(), format!("variable `{}` is assigned to even though it's not declared as mutable", variable.binding.ident.ident))
                .with_info_label(variable.binding.ident.span, format!("`{}` defined here", variable.binding.ident.ident))
                .with_info_label(variable.binding.ident.span, format!("help: try using `mut {}` here", variable.binding.ident.ident))
                .emit();
        }
    }
}