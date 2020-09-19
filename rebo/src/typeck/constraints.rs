use std::fmt;

use crate::parser::{Expr, ExprType::*, Binding};
use crate::typeck::{Type, BindingTypes};
use crate::diagnostics::Span;

#[derive(Debug)]
pub enum Constraint<'i> {
    /// Two bindings have the same type
    Eq(Binding<'i>, Binding<'i>, Span),
    /// A binding has a specific type
    Type(Binding<'i>, Type, Span),
    /// A binding has the same type as the return type of a function-binding
    RetOf(Binding<'i>, Binding<'i>, Span),
}
impl<'i> fmt::Display for Constraint<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Eq(a, b, _) => write!(f, "{} ({}) = {} ({})", a.ident, a.id, b.ident, b.id),
            Constraint::Type(a, typ, _) => write!(f, "{} ({}) = {}", a.ident, a.id, typ),
            Constraint::RetOf(a, fun, _) => write!(f, "{} ({}) = RetOf({} ({}))", a.ident, a.id, fun.ident, fun.id),
        }
    }
}

#[derive(Debug)]
enum TypeOrBinding<'i> {
    Type(Type, Span),
    Binding(Binding<'i>, Span),
    RetOf(Binding<'i>, Span),
}

pub struct ConstraintCreator<'a, 'i> {
    binding_types: &'a BindingTypes<'i>,
    constraints: Vec<Constraint<'i>>,
}

impl<'a, 'i> ConstraintCreator<'a, 'i> {
    pub fn new(binding_types: &'a BindingTypes<'i>) -> ConstraintCreator<'a, 'i> {
        ConstraintCreator {
            binding_types,
            constraints: Vec::new(),
        }
    }
    /// Iterate over the AST, adding function type signatures to the binding types and returning
    /// a set of type inference constraints.
    pub fn get_constraints(mut self, exprs: &Vec<&Expr<'_, 'i>>) -> Vec<Constraint<'i>> {
        for expr in exprs {
            self.get_type(expr);
        }
        self.constraints
    }
    fn get_type(&mut self, expr: &Expr<'_, 'i>) -> TypeOrBinding<'i> {
        match &expr.typ {
            Unit => TypeOrBinding::Type(Type::Unit, expr.span),
            &Variable((binding, _)) => TypeOrBinding::Binding(binding, expr.span),
            Integer(_) => TypeOrBinding::Type(Type::Integer, expr.span),
            Float(_) => TypeOrBinding::Type(Type::Float, expr.span),
            String(_) => TypeOrBinding::Type(Type::String, expr.span),
            Statement(expr) => {
                self.get_type(expr);
                TypeOrBinding::Type(Type::Unit, expr.span)
            },
            &FunctionCall((binding, _), _) => TypeOrBinding::RetOf(binding, expr.span),
            &Assign((binding, _), expr) | &Bind(binding, expr) => {
                let constraint = match self.get_type(expr) {
                    TypeOrBinding::Type(typ, span) => Constraint::Type(binding, typ, span),
                    TypeOrBinding::Binding(var, span) => Constraint::Eq(binding, var, span),
                    TypeOrBinding::RetOf(ret, span) => Constraint::RetOf(binding, ret, span),
                };
                self.constraints.push(constraint);
                TypeOrBinding::Type(Type::Unit, expr.span)
            },
            Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) => {
                let type_a = self.get_type(a);
                let type_b = self.get_type(b);
                match (type_a, type_b) {
                    (TypeOrBinding::Type(typ_a, span_a), TypeOrBinding::Type(typ_b, span_b)) => match (typ_a, typ_b) {
                        (t @ Type::Integer, Type::Integer) | (t @ Type::Float, Type::Float) => {
                            TypeOrBinding::Type(t, span_a)
                        },
                        _ => TypeOrBinding::Type(Type::Any, expr.span),
                    },
                    (TypeOrBinding::Binding(binding, binding_span), TypeOrBinding::Type(typ, typ_span))
                    | (TypeOrBinding::Type(typ, typ_span), TypeOrBinding::Binding(binding, binding_span)) => {
                        self.constraints.push(Constraint::Type(binding, typ.clone(), typ_span));
                        TypeOrBinding::Type(typ, typ_span)
                    },
                    (TypeOrBinding::Binding(binding_a, span_a), TypeOrBinding::Binding(binding_b, span_b)) => {
                        self.constraints.push(Constraint::Eq(binding_a, binding_b, expr.span));
                        TypeOrBinding::Binding(binding_a, span_a)
                    },
                    (TypeOrBinding::RetOf(_, _), TypeOrBinding::Type(typ, _)) => TypeOrBinding::Type(typ, b.span),
                    (TypeOrBinding::Type(typ, _), TypeOrBinding::RetOf(_, _)) => TypeOrBinding::Type(typ, a.span),
                    (TypeOrBinding::RetOf(_, _), TypeOrBinding::Binding(binding, _)) => TypeOrBinding::Binding(binding, b.span),
                    (TypeOrBinding::Binding(binding, _), TypeOrBinding::RetOf(_, _)) => TypeOrBinding::Binding(binding, a.span),
                    (TypeOrBinding::RetOf(fun, _), TypeOrBinding::RetOf(_, _)) => TypeOrBinding::RetOf(fun, a.span),
                }
            },
        }
    }
}
