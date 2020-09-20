use crate::diagnostics::{Diagnostics, Span, ErrorCode};
use crate::typeck::{BindingTypes, Type};
use crate::parser::{Expr, ExprType};

pub struct Checker<'a, 'i> {
    diagnostics: &'i Diagnostics<'i>,
    binding_types: &'a BindingTypes<'i>,
}

impl<'a, 'i> Checker<'a, 'i> {
    pub fn new(diagnostics: &'i Diagnostics<'i>, binding_types: &'a BindingTypes<'i>) -> Self {
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
            Unit => Type::Unit,
            Variable(binding) => self.binding_types.get(binding).unwrap().clone().0,
            Integer(_) => Type::Integer,
            Float(_) => Type::Float,
            String(_) => Type::String,
            Bind(binding, expr) | Assign((binding, _), expr) => {
                self.get_type(expr);
                self.binding_types.get(binding).unwrap().clone().0
            },
            Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) => {
                let (a_typ, _) = self.get_type(a);
                let (b_typ, _) = self.get_type(b);
                match (&a_typ, &b_typ) {
                    (Type::Integer, Type::Integer) => Type::Integer,
                    (Type::Float, Type::Float) => Type::Float,
                    _ => {
                        self.diagnostics.error(ErrorCode::IncompatibleMathTypes)
                            .with_error_label(expr.span, "in this math operation")
                            .with_info_label(a.span, format!("this has type `{}`", a_typ))
                            .with_info_label(b.span, format!("while this has type `{}`", b_typ))
                            .with_note("math operations are only supported for integers with integers and floats with floats")
                            .with_note("help: try casting with `... as integer` or `... as float`")
                            .emit();
                        Type::Any
                    },
                }
            },
            Statement(e) => {
                self.get_type(e);
                Type::Unit
            },
            FunctionCall((fun, f_span), ref args) => {
                // check that we are actually calling a function
                let f = match self.binding_types.get(fun).unwrap() {
                    (Type::Function(f), _) => f,
                    (t, _) => {
                        self.diagnostics.error(ErrorCode::NotAFunction)
                            .with_error_label(expr.span, "in this function call")
                            .with_info_label(f_span, format!("`{}` is of type `{}`", fun.ident, t))
                            .with_info_label(fun.span, format!("`{}` defined here", fun.ident))
                            .emit();
                        return (Type::Any, expr.span);
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
                    if *expected != actual {
                        self.diagnostics.error(ErrorCode::InvalidArgumentType)
                            .with_error_label(actual_span, format!("expected type `{}`, got type `{}`", expected, actual))
                            .emit();
                    }
                }
                f.ret.clone()
            },
            Block(ref exprs) => {
                let mut typ = Type::Unit;
                for expr in exprs {
                    typ = self.get_type(expr).0;
                }
                typ
            }
        };
        (typ, expr.span)
    }
}

