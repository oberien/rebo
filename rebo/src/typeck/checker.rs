use diagnostic::{Diagnostics, Span};

use crate::error_codes::ErrorCode;
use crate::typeck::BindingTypes;
use crate::parser::{Expr, ExprType};
use crate::common::{SpecificType, Type};

pub struct Checker<'a, 'i> {
    diagnostics: &'i Diagnostics,
    binding_types: &'a BindingTypes<'i>,
}

impl<'a, 'i> Checker<'a, 'i> {
    pub fn new(diagnostics: &'i Diagnostics, binding_types: &'a BindingTypes<'i>) -> Self {
        Checker { diagnostics, binding_types }
    }
    pub fn check(&self, exprs: &[&Expr]) {
        for expr in exprs {
            self.get_type(expr);
        }
    }
    fn get_type(&self, expr: &Expr) -> (Type, Span) {
        use ExprType::*;
        let typ = match expr.typ {
            Unit => Type::Specific(SpecificType::Unit),
            Variable(binding) => self.binding_types.get(binding).unwrap().clone().0,
            Integer(_) => Type::Specific(SpecificType::Integer),
            Float(_) => Type::Specific(SpecificType::Float),
            Bool(_) => Type::Specific(SpecificType::Bool),
            String(_) => Type::Specific(SpecificType::String),
            Bind(binding, expr) | Assign((binding, _), expr) => {
                self.get_type(expr);
                self.binding_types.get(binding).unwrap().clone().0
            },
            Equals(..) => Type::Specific(SpecificType::Bool),
            Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) => {
                let (a_typ, _) = self.get_type(a);
                let (b_typ, _) = self.get_type(b);
                match (&a_typ, &b_typ) {
                    (Type::Specific(SpecificType::Integer), Type::Specific(SpecificType::Integer)) => Type::Specific(SpecificType::Integer),
                    (Type::Specific(SpecificType::Float), Type::Specific(SpecificType::Float)) => Type::Specific(SpecificType::Float),
                    _ => {
                        self.diagnostics.error(ErrorCode::IncompatibleMathTypes)
                            .with_error_label(expr.span, "in this math operation")
                            .with_info_label(a.span, format!("this has type `{}`", a_typ))
                            .with_info_label(b.span, format!("while this has type `{}`", b_typ))
                            .with_note("math operations are only supported for integers with integers and floats with floats")
                            .with_note("help: try casting with `... as integer` or `... as float`")
                            .emit();
                        Type::Bottom
                    },
                }
            },
            BoolAnd(a, b) | BoolOr(a, b) => {
                let (a_typ, _) = self.get_type(a);
                let (b_typ, _) = self.get_type(b);
                for &(ref t, span) in &[(a_typ, a.span), (b_typ, b.span)] {
                    if !t.is_unifyable_with(&Type::Specific(SpecificType::Bool)) {
                        self.diagnostics.error(ErrorCode::InvalidBoolExprType)
                            .with_info_label(expr.span, "in this boolean operation")
                            .with_error_label(span, "this expression must have type `bool`")
                            .with_info_label(span, format!("but it has type `{}`", t))
                            .emit()
                    }
                }
                Type::Specific(SpecificType::Bool)
            }
            BoolNot(inner) => {
                match self.get_type(inner).0 {
                    Type::Specific(SpecificType::Bool) => (),
                    t => {
                        self.diagnostics.error(ErrorCode::InvalidBoolNotType)
                            .with_error_label(inner.span, "this expression should have type `bool`")
                            .with_info_label(Span::new(expr.span.file, expr.span.start, expr.span.start+1), "because the boolean not operator is applied to it")
                            .with_info_label(inner.span, format!("but this expression has type `{}`", t))
                            .emit();
                    }
                }
                Type::Specific(SpecificType::Bool)
            }
            Statement(e) => {
                self.get_type(e);
                Type::Specific(SpecificType::Unit)
            },
            Parenthezised(e) => self.get_type(e).0,
            FunctionCall((fun, f_span), ref args) => {
                // check that we are actually calling a function
                let f = match self.binding_types.get(fun).unwrap() {
                    (Type::Specific(SpecificType::Function(f)), _) => f,
                    (t, _) => {
                        self.diagnostics.error(ErrorCode::NotAFunction)
                            .with_error_label(expr.span, "in this function call")
                            .with_info_label(f_span, format!("`{}` is of type `{}`", fun.ident, t))
                            .with_info_label(fun.span, format!("`{}` defined here", fun.ident))
                            .emit();
                        return (Type::Bottom, expr.span);
                    },
                };
                // check argument length
                let is_varargs = match f.args.last() {
                    Some(Type::Varargs) => true,
                    _ => false,
                };
                if (is_varargs && args.len() < f.args.len() - 1) || (!is_varargs && args.len() != f.args.len()) {
                    self.diagnostics.error(ErrorCode::InvalidNumberOfArguments)
                        .with_error_label(expr.span, "in this function call")
                        .with_info_label(expr.span, format!("got {} arguments, expected {} arguments", args.len(), f.args.len()))
                        .with_info_label(fun.span, "function defined here")
                        .emit();
                }
                // check argument types
                let expected_arg_types = match is_varargs {
                    true => f.args.iter().chain(Some(Type::Varargs).iter().cycle()),
                    false => f.args.iter().chain(None.iter().cycle()),
                };
                let arg_types = args.iter().map(|expr| (self.get_type(expr).0, expr.span));
                for (expected, (actual, actual_span)) in expected_arg_types.zip(arg_types) {
                    if !expected.is_unifyable_with(&actual) {
                        self.diagnostics.error(ErrorCode::InvalidArgumentType)
                            .with_error_label(actual_span, format!("expected type `{}`, got type `{}`", expected, actual))
                            .emit();
                    }
                }
                f.ret.clone()
            },
            Block(ref exprs) => {
                let mut typ = Type::Specific(SpecificType::Unit);
                for expr in exprs {
                    typ = self.get_type(expr).0;
                }
                typ
            }
        };
        (typ, expr.span)
    }
}

