use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{BlockStack, MetaInfo, SpanWithId, UserType};
use crate::parser::{ExprBool, ExprInteger, ExprLiteral, ExprMatch, ExprMatchPattern, ExprString};
use crate::error_codes::ErrorCode;
use std::fmt::{Debug, Formatter};
use indexmap::set::IndexSet;
use indexmap::map::{Entry, IndexMap};
use std::hash::Hash;
use crate::lexer::{TokenBool, TokenDqString, TokenInteger};
use itertools::Itertools;
use crate::common::Spanned;
use crate::typeck::types::{SpecificType, Type};
use crate::typeck::TypeVar;

pub struct MatchLints;

impl Visitor for MatchLints {
    fn visit_match(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, ()>, expr: &ExprMatch) {
        let match_span = expr.span_with_id();
        let ExprMatch { expr, arms, .. } = expr;

        let typ = &meta_info.types[&TypeVar::from_spanned(expr)];
        match typ {
            Type::Top => (),
            Type::Bottom => unreachable!("Type::Bottom after typeck"),
            Type::UntypedVarargs => unreachable!("Type::UntypedVarargs as match-expr"),
            Type::TypedVarargs(_) => unreachable!("Type::TypedVarargs as match-expr"),
            Type::Specific(SpecificType::Any(span)) => unreachable!("Any after typeck: any<{}>", span.id()),
            Type::Specific(SpecificType::Float) => {
                diagnostics.error(ErrorCode::FloatMatch)
                    .with_error_label(expr.diagnostics_span(), "")
                    .emit();
            }
            Type::Specific(SpecificType::Function(_)) => {
                diagnostics.error(ErrorCode::FunctionMatch)
                    .with_error_label(expr.diagnostics_span(), "")
                    .emit();
            }
            Type::Specific(SpecificType::Struct(_, _)) => {
                diagnostics.error(ErrorCode::StructMatch)
                    .with_error_label(expr.diagnostics_span(), "")
                    .emit();
            }
            Type::Specific(SpecificType::Unit) => {
                let mut checker = VariantChecker::new(diagnostics, match_span, Some(&[()]));
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Unit(_)) => {
                            checker.insert((), pattern.span_with_id());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span_with_id());
                        }
                        ExprMatchPattern::Literal(_)
                        | ExprMatchPattern::Variant(_) => (),
                    }
                }
            }
            Type::Specific(SpecificType::Integer) => {
                let mut checker = VariantChecker::new(diagnostics, match_span, None);
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Integer(ExprInteger { int: TokenInteger { value, .. } })) => {
                            checker.insert(value, pattern.span_with_id());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span_with_id());
                        }
                        ExprMatchPattern::Literal(_)
                        | ExprMatchPattern::Variant(_) => (),
                    }
                }
            }
            Type::Specific(SpecificType::Bool) => {
                let mut checker = VariantChecker::new(diagnostics, match_span, Some(&[true, false]));
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Bool(ExprBool { b: TokenBool { value, .. }, .. })) => {
                            checker.insert(*value, pattern.span_with_id());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span_with_id());
                        }
                        ExprMatchPattern::Literal(_)
                        | ExprMatchPattern::Variant(_) => (),
                    }
                }
            }
            Type::Specific(SpecificType::String) => {
                let mut checker = VariantChecker::new(diagnostics, match_span, None);
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::String(ExprString { string: TokenDqString { string, .. } })) => {
                            checker.insert(string, pattern.span_with_id());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span_with_id());
                        }
                        ExprMatchPattern::Literal(_)
                        | ExprMatchPattern::Variant(_) => (),
                    }
                }
            }
            Type::Specific(SpecificType::Enum(name, _)) => {
                let e = match meta_info.enum_types.get(name.as_str()) {
                    Some(e) => e,
                    None => {
                        let similar = crate::util::similar_name(name, meta_info.enum_types.keys());
                        let mut diag = diagnostics.error(ErrorCode::UnknownEnum)
                            .with_error_label(expr.diagnostics_span(), format!("unknown enum `{name}` in match"));
                        if let Some(similar) = similar {
                            diag = diag.with_info_label(expr.diagnostics_span(), format!("did you mean `{}`", similar));
                        }
                        diag.emit();
                        return
                    }
                };
                let variants: Vec<_> = e.variants.iter()
                    .map(|(name, _)| RequiredEnumVariant(name.as_str()))
                    .collect();
                let mut checker = VariantChecker::new(diagnostics, match_span, Some(&variants));
                for (pattern, _arrow, _expr) in arms {
                    match pattern {
                        ExprMatchPattern::Literal(_) => (),
                        ExprMatchPattern::Variant(variant) => {
                            let enum_type = match meta_info.enum_types.get(variant.enum_name.ident) {
                                Some(enum_type) => enum_type,
                                None => {
                                    let similar = crate::util::similar_name(variant.enum_name.ident, meta_info.enum_types.keys());
                                    let mut diag = diagnostics.error(ErrorCode::UnknownEnum)
                                        .with_error_label(variant.enum_name.diagnostics_span(), "unknown enum in match pattern");
                                    if let Some(similar) = similar {
                                        diag = diag.with_info_label(variant.enum_name.diagnostics_span(), format!("did you mean `{}`", similar));
                                    }
                                    diag.emit();
                                    continue
                                },
                            };
                            let variant_field_num = enum_type.variants.iter()
                                .map(|(name, variant)| (name, variant.num_fields()))
                                .find(|(name, _num_fields)| *name == variant.variant_name.ident)
                                .map(|(_name, num_fields)| num_fields);
                            let variant_field_num = match variant_field_num {
                                Some(num) => num,
                                None => {
                                    let variant_names = enum_type.variants.iter().map(|(name, _variant)| name);
                                    let similar = crate::util::similar_name(variant.variant_name.ident, variant_names);
                                    let mut diag = diagnostics.error(ErrorCode::UnknownEnumVariant)
                                        .with_error_label(variant.variant_name.diagnostics_span(), "unknown enum variant in match pattern");
                                    if let Some(similar) = similar {
                                        diag = diag.with_info_label(variant.variant_name.diagnostics_span(), format!("did you mean `{}`", similar));
                                    }
                                    diag.emit();
                                    continue
                                }
                            };

                            checker.insert(RequiredEnumVariant(variant.variant_name.ident), variant.variant_name.span_with_id());
                            let actual_field_num = variant.fields.as_ref()
                                .map(|(_open, fields, _close)| fields.len())
                                .unwrap_or(0);
                            if actual_field_num != variant_field_num {
                                let enum_def = match &meta_info.user_types[variant.enum_name.ident] {
                                    UserType::Enum(enum_def) => enum_def,
                                    _ => unreachable!(),
                                };
                                let variant_def = enum_def.variants.iter()
                                    .find(|v| v.name.ident == variant.variant_name.ident)
                                    .unwrap();
                                diagnostics.error(ErrorCode::InvalidNumberOfEnumVariantFields)
                                    .with_error_label(variant.variant_name.diagnostics_span(), format!("expected {} fields but got {}", variant_field_num, actual_field_num))
                                    .with_info_label(variant_def.name.diagnostics_span(), "variant defined here")
                                    .emit()
                            }
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span_with_id());
                        }
                    }
                }
            }
            Type::Specific(SpecificType::Generic(_)) => todo!(),
        }
    }
}

#[derive(Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
struct RequiredEnumVariant<'i>(&'i str);
impl<'i> Debug for RequiredEnumVariant<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

struct VariantChecker<'i, T: Debug> {
    diagnostics: &'i Diagnostics<ErrorCode>,
    match_span: SpanWithId,
    catchall: Option<SpanWithId>,
    cases: IndexMap<T, SpanWithId>,
    required: IndexSet<T>,
    had_required: bool,
}
impl<'i, T: Clone + Hash + Eq + Debug> VariantChecker<'i, T> {
    fn new(diagnostics: &'i Diagnostics<ErrorCode>, match_span: SpanWithId, required: Option<&[T]>) -> Self {
        Self {
            diagnostics,
            match_span,
            catchall: None,
            cases: IndexMap::new(),
            had_required: required.is_some(),
            required: required.into_iter().flatten().cloned().collect(),
        }
    }
    fn insert(&mut self, value: T, value_span: SpanWithId) {
        if let Some(previous_span) = self.catchall {
            self.diag(value_span, previous_span);
        }
        self.required.shift_remove(&value);
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
    fn catchall(&mut self, catchall_span: SpanWithId) {
        if let Some(previous_span) = self.catchall {
            self.diag(catchall_span, previous_span);
        } else if self.had_required && self.required.is_empty() {
            self.diagnostics.warning(ErrorCode::UnreachableMatchArm)
                .with_error_label(catchall_span.diagnostics_span(), "this pattern is unreachable")
                .with_info_label(catchall_span.diagnostics_span(), "all possible cases are already covered")
                .emit();
        } else {
            self.catchall = Some(catchall_span);
            self.required.clear();
        }
    }
    fn diag(&self, value_span: SpanWithId, previous_span: SpanWithId) {
        self.diagnostics.warning(ErrorCode::UnreachableMatchArm)
            .with_error_label(value_span.diagnostics_span(), "this pattern is unreachable")
            .with_info_label(previous_span.diagnostics_span(), "already covered here")
            .emit();
    }
}
impl<'i, T: Debug> Drop for VariantChecker<'i, T> {
    fn drop(&mut self) {
        if self.catchall.is_none() && !self.had_required {
            self.diagnostics.error(ErrorCode::MatchNoCatchall)
                .with_error_label(self.match_span.diagnostics_span(), "this match doesn't have a catchall case")
                .with_note("consider adding a `_ => ...` or `foo => ...` case")
                .emit();
        }

        if !self.required.is_empty() {
            let joined = self.required.iter().map(|req| format!("`{:?}`", req)).join(", ");
            self.diagnostics.error(ErrorCode::NonExhaustiveMatch)
                .with_error_label(self.match_span.diagnostics_span(), format!("cases {} not covered", joined))
                .emit();
        }
    }
}
