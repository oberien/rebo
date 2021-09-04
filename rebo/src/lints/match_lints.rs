use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, Type, SpecificType};
use crate::parser::{ExprMatch, Spanned};
use crate::error_codes::ErrorCode;
use crate::typeck2::TypeVar;

pub struct MatchLints;

impl Visitor for MatchLints {
    fn visit_match(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, expr: &ExprMatch) {
        let ExprMatch { expr, arms, .. } = expr;

        if arms.is_empty() {
            diagnostics.warning(ErrorCode::EmptyMatch)
                .with_error_label(expr.span(), "this match has an empty body")
                .emit();
        }

        for (_pattern, _arrow, expr) in arms {
            match meta_info.types[&TypeVar::new(expr.span())] {
                Type::Specific(SpecificType::Float) => {
                    diagnostics.error(ErrorCode::FloatMatch)
                        .with_error_label(expr.span(), "")
                        .emit();
                }
                Type::Specific(SpecificType::Struct(_, _)) => {
                    diagnostics.error(ErrorCode::StructMatch)
                        .with_error_label(expr.span(), "")
                        .emit();
                }
                _ => (),
            }

        }
    }
}