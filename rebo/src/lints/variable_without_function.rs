use diagnostic::Diagnostics;
use rebo::parser::ExprAccess;
use crate::lints::visitor::Visitor;
use crate::{ErrorCode, MetaInfo};
use crate::common::BlockStack;
use crate::common::Spanned;
use crate::parser::{ExprAddAssign, ExprAssign, ExprAssignLhs, ExprBoolAndAssign, ExprBoolOrAssign, ExprDivAssign, ExprFieldAccess, ExprMulAssign, ExprSubAssign, ExprVariable};

pub struct VariableWithoutFunction;

impl Visitor for VariableWithoutFunction {
    fn visit_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, ExprAssign { lhs, .. }: &ExprAssign) {
        visit_assign_lhs(diagnostics, lhs);
    }
    fn visit_access(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, access: &ExprAccess) {
        let ExprAccess { variable, .. } = access;
        check_variable(diagnostics, variable);
    }
    fn visit_add_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, ExprAddAssign { lhs, .. }: &ExprAddAssign) {
        visit_assign_lhs(diagnostics, lhs);
    }
    fn visit_sub_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, ExprSubAssign { lhs, .. }: &ExprSubAssign) {
        visit_assign_lhs(diagnostics, lhs);
    }
    fn visit_mul_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, ExprMulAssign { lhs, .. }: &ExprMulAssign) {
        visit_assign_lhs(diagnostics, lhs);
    }
    fn visit_div_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, ExprDivAssign { lhs, .. }: &ExprDivAssign) {
        visit_assign_lhs(diagnostics, lhs);
    }
    fn visit_bool_and_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, ExprBoolAndAssign { lhs, .. }: &ExprBoolAndAssign) {
        visit_assign_lhs(diagnostics, lhs);
    }
    fn visit_bool_or_assign(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, ExprBoolOrAssign { lhs, .. }: &ExprBoolOrAssign) {
        visit_assign_lhs(diagnostics, lhs);
    }
}

fn visit_assign_lhs(diagnostics: &Diagnostics<ErrorCode>, lhs: &ExprAssignLhs) {
    match lhs {
        ExprAssignLhs::FieldAccess(ExprFieldAccess { variable, .. })
        | ExprAssignLhs::Variable(variable) => {
            check_variable(diagnostics, variable)
        }
    }
}

fn check_variable(diagnostics: &Diagnostics<ErrorCode>, variable: &ExprVariable) {
    if !variable.binding.ident.ident.contains("::") {
        return;
    }
    diagnostics.error(ErrorCode::NonVariableAssign)
        .with_error_label(variable.diagnostics_span(), "tried to assign to this non-variable")
        .emit();
}