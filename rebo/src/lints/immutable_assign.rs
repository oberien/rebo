use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, BlockStack};
use crate::parser::{ExprAddAssign, ExprAssign, ExprAssignLhs, ExprBoolAndAssign, ExprBoolOrAssign, ExprDivAssign, ExprFieldAccess, ExprMulAssign, ExprSubAssign, Spanned};
use crate::error_codes::ErrorCode;

pub struct ImmutableAssign;

impl Visitor for ImmutableAssign {
    fn visit_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, assign: &ExprAssign) {
        check_mutable_assign(&assign.lhs, diagnostics);
    }
    fn visit_add_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, assign: &ExprAddAssign) {
        check_mutable_assign(&assign.lhs, diagnostics);
    }
    fn visit_sub_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, assign: &ExprSubAssign) {
        check_mutable_assign(&assign.lhs, diagnostics);
    }
    fn visit_mul_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, assign: &ExprMulAssign) {
        check_mutable_assign(&assign.lhs, diagnostics);
    }
    fn visit_div_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, assign: &ExprDivAssign) {
        check_mutable_assign(&assign.lhs, diagnostics);
    }
    fn visit_bool_and_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, assign: &ExprBoolAndAssign) {
        check_mutable_assign(&assign.lhs, diagnostics);
    }
    fn visit_bool_or_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, assign: &ExprBoolOrAssign) {
        check_mutable_assign(&assign.lhs, diagnostics);
    }
}

fn check_mutable_assign(lhs: &ExprAssignLhs, diagnostics: &Diagnostics<ErrorCode>) {
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