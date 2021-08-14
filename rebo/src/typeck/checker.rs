use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::typeck::TypeVar;
use crate::common::{SpecificType, Type};
use std::collections::{BTreeMap, HashMap, hash_map::Entry};
use itertools::Itertools;
use crate::parser::{Expr, ExprFormatString, ExprFormatStringPart, ExprBind, ExprAssign, ExprBoolNot, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolAnd, ExprBoolOr, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprBlock, ExprParenthesized, ExprIfElse, ExprWhile, ExprMatch, ExprFunctionCall, ExprFunctionDefinition, ExprStructInitialization, Spanned, ExprMatchPattern, ExprLiteral, ExprInteger, ExprString, ExprBool, BlockBody};
use crate::lexer::{TokenInteger, TokenDqString, TokenBool};
use indexmap::set::IndexSet;
use std::hash::Hash;
use std::fmt::Debug;

pub struct Checker<'i> {
    diagnostics: &'i Diagnostics,
    solved: BTreeMap<TypeVar, (Span, Type)>,
    restrictions: Vec<(TypeVar, Vec<SpecificType>)>,
}

impl<'i> Checker<'i> {
    pub fn new(diagnostics: &'i Diagnostics, solved: BTreeMap<TypeVar, (Span, Type)>, restrictions: Vec<(TypeVar, Vec<SpecificType>)>) -> Self {
        Checker { diagnostics, solved, restrictions }
    }
    pub fn check(self, exprs: &[&Expr<'_, '_>]) {
        // check match arms including unreachable pattern warnings
        for expr in exprs {
            self.check_match_variants(expr);
        }

        // check restrictions
        for (var, restrictions) in self.restrictions {
            let (span, solved) = &self.solved[&var];
            if !restrictions.iter().any(|r| solved.try_unify(&Type::Specific(r.clone())).is_ok()) {
                let one_of = if restrictions.len() == 1 { "" } else { "one of " };
                self.diagnostics.error(ErrorCode::TypeConflict)
                    .with_error_label(*span, format!("inferred type is {}", solved))
                    .with_info_label(var.span, format!("but expected {}`{}`", one_of, restrictions.iter().join("`, `")))
                    .emit();
            }
        }
    }

    fn check_match_variants(&self, expr: &Expr<'_, '_>) {
        match expr {
            Expr::Literal(_) => (),
            Expr::FormatString(ExprFormatString { parts, .. }) => {
                for part in parts {
                    match part {
                        ExprFormatStringPart::Escaped(_) => (),
                        ExprFormatStringPart::Str(_) => (),
                        ExprFormatStringPart::FmtArg(expr) => self.check_match_variants(expr),
                    }
                }
            }
            Expr::Bind(ExprBind { expr, .. }) => self.check_match_variants(expr),
            Expr::Assign(ExprAssign { expr, .. }) => self.check_match_variants(expr),
            Expr::BoolNot(ExprBoolNot { expr, .. }) => self.check_match_variants(expr),
            Expr::Add(ExprAdd { a, b, .. })
            | Expr::Sub(ExprSub { a, b, .. })
            | Expr::Mul(ExprMul { a, b, .. })
            | Expr::Div(ExprDiv { a, b, .. })
            | Expr::BoolAnd(ExprBoolAnd { a, b, .. })
            | Expr::BoolOr(ExprBoolOr { a, b, .. })
            | Expr::LessThan(ExprLessThan { a, b, .. })
            | Expr::LessEquals(ExprLessEquals { a, b, .. })
            | Expr::Equals(ExprEquals { a, b, .. })
            | Expr::NotEquals(ExprNotEquals { a, b, .. })
            | Expr::GreaterEquals(ExprGreaterEquals { a, b, .. })
            | Expr::GreaterThan(ExprGreaterThan { a, b, .. }) => {
                self.check_match_variants(a);
                self.check_match_variants(b);
            }
            Expr::Block(ExprBlock { body: BlockBody { exprs, .. }, .. }) => {
                for expr in exprs {
                    self.check_match_variants(expr);
                }
            }
            Expr::Variable(_) => (),
            Expr::FieldAccess(_) => (),
            Expr::Parenthesized(ExprParenthesized { expr, .. }) => self.check_match_variants(expr),
            Expr::IfElse(ExprIfElse { condition, then, else_ifs, els, .. }) => {
                self.check_match_variants(condition);
                for expr in &then.body.exprs {
                    self.check_match_variants(expr);
                }
                for (_else, _if, cond, block) in else_ifs {
                    self.check_match_variants(cond);
                    for expr in &block.body.exprs {
                        self.check_match_variants(expr);
                    }
                }
                if let Some((_else, block)) = els {
                    for expr in &block.body.exprs {
                        self.check_match_variants(expr);
                    }
                }
            },
            Expr::Match(ExprMatch { expr, arms, .. }) => {
                let expr_type_var = TypeVar::new(expr.span());
                let (_typ_span, typ) = self.solved.get(&expr_type_var).unwrap_or_else(|| panic!("no type found for `{}`", self.diagnostics.resolve_span(expr.span())));
                match typ {
                    Type::Bottom | Type::Varargs | Type::Top => unreachable!("expr type not inferred correctly"),
                    Type::Specific(specific) => {
                        let pattern_iter = arms.iter().map(|(pattern, _arrow, _expr)| pattern);
                        self.check_specific_type_match_variants(expr.span(), specific, pattern_iter);
                        for (_pattern, _arrow, expr) in arms {
                            self.check_match_variants(expr);
                        }
                    },
                }
            }
            Expr::While(ExprWhile { condition, block, .. }) => {
                self.check_match_variants(condition);
                for expr in &block.body.exprs {
                    self.check_match_variants(expr);
                }
            }
            Expr::FunctionCall(ExprFunctionCall { args, .. }) => {
                for expr in args {
                    self.check_match_variants(expr);
                }
            }
            Expr::FunctionDefinition(ExprFunctionDefinition { body, .. }) => {
                for expr in &body.body.exprs {
                    self.check_match_variants(expr);
                }
            }
            Expr::StructDefinition(_) => (),
            Expr::StructInitialization(ExprStructInitialization { fields, .. }) => {
                for (_ident, _colon, expr) in fields {
                    self.check_match_variants(expr);
                }
            }
        }
    }

    fn check_specific_type_match_variants(&self, match_span: Span, typ: &SpecificType, patterns: impl Iterator<Item = &'i ExprMatchPattern<'i>>) {
        match typ {
            SpecificType::Unit => {
                let mut checker = VariantChecker::new(self.diagnostics, match_span, Some(&[()]));
                for pattern in patterns {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Unit(_)) => {
                            checker.insert((), pattern.span());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span());
                        }
                        ExprMatchPattern::Literal(_) => unreachable!("match: unit expr with non-unit pattern"),
                    }
                }
            }
            SpecificType::Integer => {
                let mut checker = VariantChecker::new(self.diagnostics, match_span, None);
                for pattern in patterns {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Integer(ExprInteger { int: TokenInteger { value, .. } })) => {
                            checker.insert(value, pattern.span());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span());
                        }
                        ExprMatchPattern::Literal(_) => unreachable!("match: int expr with non-int pattern"),
                    }
                }
            }
            SpecificType::Float => {
                self.diagnostics.error(ErrorCode::FloatMatch)
                    .with_error_label(match_span, "")
                    .emit();
            }
            SpecificType::Bool => {
                let mut checker = VariantChecker::new(self.diagnostics, match_span, Some(&[true, false]));
                for pattern in patterns {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::Bool(ExprBool { b: TokenBool { value, .. }, .. })) => {
                            checker.insert(*value, pattern.span());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span());
                        }
                        ExprMatchPattern::Literal(_) => unreachable!("match: bool expr with non-bool pattern"),
                    }
                }
            }
            SpecificType::String => {
                let mut checker = VariantChecker::new(self.diagnostics, match_span, None);
                for pattern in patterns {
                    match pattern {
                        ExprMatchPattern::Literal(ExprLiteral::String(ExprString { string: TokenDqString { string, .. } })) => {
                            checker.insert(string, pattern.span());
                        }
                        ExprMatchPattern::Binding(_)
                        | ExprMatchPattern::Wildcard(_) => {
                            checker.catchall(pattern.span());
                        }
                        ExprMatchPattern::Literal(_) => unreachable!("match: string expr with non-string pattern"),
                    }
                }
            }
            SpecificType::Function(_) => {
                todo!()
            }
            SpecificType::Struct(_name) => {
                self.diagnostics.error(ErrorCode::StructMatch)
                    .with_error_label(match_span, "")
                    .emit();
            }
        }
    }
}

struct VariantChecker<'i, T: Debug> {
    diagnostics: &'i Diagnostics,
    match_span: Span,
    catchall: Option<Span>,
    cases: HashMap<T, Span>,
    required: IndexSet<T>,
    had_required: bool,
}
impl<'i, T: Clone + Hash + Eq + Debug> VariantChecker<'i, T> {
    fn new(diagnostics: &'i Diagnostics, match_span: Span, required: Option<&[T]>) -> Self {
        Self {
            diagnostics,
            match_span,
            catchall: None,
            cases: HashMap::new(),
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
