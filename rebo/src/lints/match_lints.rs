use crate::lints::visitor::Visitor;
use diagnostic::{Diagnostics, Span};
use crate::common::{MetaInfo, Type, SpecificType};
use crate::parser::{ExprMatch, Spanned, ExprMatchPattern, ExprLiteral, ExprInteger, ExprBool, ExprString};
use crate::error_codes::ErrorCode;
use std::fmt::Debug;
use indexmap::set::IndexSet;
use indexmap::map::{IndexMap, Entry};
use std::hash::Hash;
use crate::lexer::{TokenInteger, TokenBool, TokenDqString};
use itertools::Itertools;

pub struct MatchLints;

impl Visitor for MatchLints {
    fn visit_match(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, expr: &ExprMatch) {
        let match_span = expr.span();
        let ExprMatch { expr, arms, .. } = expr;

        if arms.is_empty() {
            diagnostics.warning(ErrorCode::EmptyMatch)
                .with_error_label(expr.span(), "this match has an empty body")
                .emit();
        }

        let typ = &meta_info.types[&expr.span()];
        match typ {
            Type::Top => return,
            Type::Bottom => unreachable!("Type::Bottom after typeck"),
            Type::Varargs => unreachable!("Type::Varargs as match-expr"),
            Type::Specific(SpecificType::Float) => {
                diagnostics.error(ErrorCode::FloatMatch)
                    .with_error_label(expr.span(), "")
                    .emit();
                return;
            }
            Type::Specific(SpecificType::Struct(_)) => {
                diagnostics.error(ErrorCode::StructMatch)
                    .with_error_label(expr.span(), "")
                    .emit();
                return;
            }
            Type::Specific(SpecificType::Unit) => {
                let mut checker = VariantChecker::new(diagnostics, match_span, Some(&[()]));
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Unit(_)) => {
                            checker.insert((), pattern.span());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span());
                        }
                        ExprMatchPattern::Literal(_) => (),
                    }
                }
            }
            Type::Specific(SpecificType::Integer) => {
                let mut checker = VariantChecker::new(diagnostics, match_span, None);
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Integer(ExprInteger { int: TokenInteger { value, .. } })) => {
                            checker.insert(value, pattern.span());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span());
                        }
                        ExprMatchPattern::Literal(_) => (),
                    }
                }
            }
            Type::Specific(SpecificType::Bool) => {
                let mut checker = VariantChecker::new(diagnostics, match_span, Some(&[true, false]));
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Bool(ExprBool { b: TokenBool { value, .. }, .. })) => {
                            checker.insert(*value, pattern.span());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span());
                        }
                        ExprMatchPattern::Literal(_) => (),
                    }
                }
            }
            Type::Specific(SpecificType::String) => {
                let mut checker = VariantChecker::new(diagnostics, match_span, None);
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::String(ExprString { string: TokenDqString { string, .. } })) => {
                            checker.insert(string, pattern.span());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span());
                        }
                        ExprMatchPattern::Literal(_) => (),
                    }
                }
            }
        }
    }
}

struct VariantChecker<'i, T: Debug> {
    diagnostics: &'i Diagnostics,
    match_span: Span,
    catchall: Option<Span>,
    cases: IndexMap<T, Span>,
    required: IndexSet<T>,
    had_required: bool,
}
impl<'i, T: Clone + Hash + Eq + Debug> VariantChecker<'i, T> {
    fn new(diagnostics: &'i Diagnostics, match_span: Span, required: Option<&[T]>) -> Self {
        Self {
            diagnostics,
            match_span,
            catchall: None,
            cases: IndexMap::new(),
            had_required: required.is_some(),
            required: required.into_iter().flatten().cloned().collect(),
        }
    }
    fn insert(&mut self, value: T, value_span: Span) {
        if let Some(previous_span) = self.catchall {
            self.diag(value_span, previous_span);
        }
        self.required.remove(&value);
        match self.cases.entry(value) {
            Entry::Vacant(vacant) => {
                vacant.insert(value_span);
            },
            Entry::Occupied(occupied) => {
                let previous_span = *occupied.get();
                self.diag(value_span, previous_span)
            },
        }
    }
    fn catchall(&mut self, catchall_span: Span) {
        if let Some(previous_span) = self.catchall {
            self.diag(catchall_span, previous_span);
        } else {
            self.catchall = Some(catchall_span);
            self.required.clear();
        }
    }
    fn diag(&self, value_span: Span, previous_span: Span) {
        self.diagnostics.warning(ErrorCode::UnreachableMatchArm)
            .with_error_label(value_span, "this pattern is unreachable")
            .with_info_label(previous_span, "already covered here")
            .emit();
    }
}
impl<'i, T: Debug> Drop for VariantChecker<'i, T> {
    fn drop(&mut self) {
        if self.catchall.is_none() && !self.had_required {
            self.diagnostics.error(ErrorCode::MatchNoCatchall)
                .with_error_label(self.match_span, "this match doesn't have a catchall case")
                .with_note("consider adding a `_ => ...` or `foo => ...` case")
                .emit();
        }

        if !self.required.is_empty() {
            let joined = self.required.iter().map(|req| format!("`{:?}`", req)).join(", ");
            self.diagnostics.error(ErrorCode::NonExhaustiveMatch)
                .with_error_label(self.match_span, format!("cases {} not covered", joined))
                .emit();
        }
    }
}
