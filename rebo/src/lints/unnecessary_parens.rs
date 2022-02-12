use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, BlockStack};
use crate::parser::{ExprIfElse, ExprMatch, ExprWhile, Expr, Spanned};
use crate::error_codes::ErrorCode;

pub struct UnnecessaryParens;

impl Visitor for UnnecessaryParens {
    fn visit_if_else(&self, diagnostics: &Diagnostics, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, expr: &ExprIfElse) {
        check_unnecessary_parens(diagnostics, expr.condition, ErrorCode::UnnecessaryIfConditionParenthesis);
    }
    fn visit_match(&self, diagnostics: &Diagnostics, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, expr: &ExprMatch) {
        check_unnecessary_parens(diagnostics, expr.expr, ErrorCode::UnnecessaryMatchTargetParenthesis);
    }
    fn visit_while(&self, diagnostics: &Diagnostics, _: &MetaInfo, _: &BlockStack<'_, '_, ()>, expr: &ExprWhile) {
        check_unnecessary_parens(diagnostics, expr.condition, ErrorCode::UnnecessaryWhileConditionParenthesis);
    }
}
fn check_unnecessary_parens(diagnostics: &Diagnostics, expr: &Expr, error_code: ErrorCode) {
    if let Expr::Parenthesized(_) = expr {
        diagnostics.warning(error_code)
            .with_info_label(expr.span(), "in this expression")
            .with_note("remove the parenthesis")
            .emit();
    }
}

