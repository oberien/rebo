use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{BlockStack, MetaInfo};
use crate::common::Spanned;
use crate::parser::{Expr, ExprIfElse, ExprMatch, ExprWhile};
use crate::error_codes::ErrorCode;

pub struct UnnecessaryParens;

impl Visitor for UnnecessaryParens {
    fn visit_if_else(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, ()>, expr: &ExprIfElse) {
        check_unnecessary_parens(diagnostics, expr.condition, ErrorCode::UnnecessaryIfConditionParenthesis);
    }
    fn visit_match(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, ()>, expr: &ExprMatch) {
        check_unnecessary_parens(diagnostics, expr.expr, ErrorCode::UnnecessaryMatchTargetParenthesis);
    }
    fn visit_while(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, _: &BlockStack<'_, ()>, expr: &ExprWhile) {
        check_unnecessary_parens(diagnostics, expr.condition, ErrorCode::UnnecessaryWhileConditionParenthesis);
    }
}
fn check_unnecessary_parens(diagnostics: &Diagnostics<ErrorCode>, expr: &Expr, error_code: ErrorCode) {
    if let Expr::Parenthesized(_) = expr {
        diagnostics.warning(error_code)
            .with_info_label(expr.diagnostics_span(), "in this expression")
            .with_note("remove the parenthesis")
            .emit();
    }
}

