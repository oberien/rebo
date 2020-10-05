use std::fmt;

use crate::parser::{Expr, ExprType, Binding};
use crate::typeck::BindingTypes;
use crate::diagnostics::Span;
use crate::common::{SpecificType, Type};

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
            Constraint::Eq(a, b, _) => write!(f, "{}({}) = {}({})", a.ident, a.id, b.ident, b.id),
            Constraint::Type(a, typ, _) => write!(f, "{}({}) = {}", a.ident, a.id, typ),
            Constraint::RetOf(a, fun, _) => write!(f, "{}({}) = RetOf({}({}))", a.ident, a.id, fun.ident, fun.id),
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
        use ExprType::*;
        match &expr.typ {
            Unit => TypeOrBinding::Type(Type::Specific(SpecificType::Unit), expr.span),
            &Variable(binding) => TypeOrBinding::Binding(binding, expr.span),
            Integer(_) => TypeOrBinding::Type(Type::Specific(SpecificType::Integer), expr.span),
            Float(_) => TypeOrBinding::Type(Type::Specific(SpecificType::Float), expr.span),
            Bool(_) => TypeOrBinding::Type(Type::Specific(SpecificType::Bool), expr.span),
            String(_) => TypeOrBinding::Type(Type::Specific(SpecificType::String), expr.span),
            Statement(inner) => {
                self.get_type(inner);
                TypeOrBinding::Type(Type::Specific(SpecificType::Unit), expr.span)
            },
            Parenthezised(inner) => self.get_type(inner),
            &FunctionCall((binding, _), _) => TypeOrBinding::RetOf(binding, expr.span),
            &Assign((binding, _), expr) | &Bind(binding, expr) => {
                let constraint = match self.get_type(expr) {
                    TypeOrBinding::Type(typ, _) => Constraint::Type(binding, typ, expr.span),
                    TypeOrBinding::Binding(var, _) => Constraint::Eq(binding, var, expr.span),
                    TypeOrBinding::RetOf(ret, _) => Constraint::RetOf(binding, ret, expr.span),
                };
                self.constraints.push(constraint);
                TypeOrBinding::Type(Type::Specific(SpecificType::Unit), expr.span)
            },
            &Add(a, b) | &Sub(a, b) | &Mul(a, b) | &Div(a, b) => {
                let type_a = self.get_type(a);
                let type_b = self.get_type(b);
                match (type_a, type_b) {
                    (TypeOrBinding::Type(typ_a, _span_a), TypeOrBinding::Type(typ_b, _span_b)) => match (typ_a, typ_b) {
                        (t @ Type::Specific(SpecificType::Integer), Type::Specific(SpecificType::Integer)) | (t @ Type::Specific(SpecificType::Float), Type::Specific(SpecificType::Float)) => {
                            TypeOrBinding::Type(t, expr.span)
                        },
                        _ => TypeOrBinding::Type(Type::Bottom, expr.span),
                    },
                    (TypeOrBinding::Binding(binding, _binding_span), TypeOrBinding::Type(typ, _typ_span))
                    | (TypeOrBinding::Type(typ, _typ_span), TypeOrBinding::Binding(binding, _binding_span)) => {
                        self.constraints.push(Constraint::Type(binding, typ.clone(), expr.span));
                        TypeOrBinding::Type(typ, expr.span)
                    },
                    (TypeOrBinding::Binding(binding_a, _span_a), TypeOrBinding::Binding(binding_b, _span_b)) => {
                        self.constraints.push(Constraint::Eq(binding_a, binding_b, expr.span));
                        TypeOrBinding::Binding(binding_a, expr.span)
                    },
                    (TypeOrBinding::RetOf(_, _), TypeOrBinding::Type(typ, _)) => TypeOrBinding::Type(typ, expr.span),
                    (TypeOrBinding::Type(typ, _), TypeOrBinding::RetOf(_, _)) => TypeOrBinding::Type(typ, expr.span),
                    (TypeOrBinding::RetOf(_, _), TypeOrBinding::Binding(binding, _)) => TypeOrBinding::Binding(binding, expr.span),
                    (TypeOrBinding::Binding(binding, _), TypeOrBinding::RetOf(_, _)) => TypeOrBinding::Binding(binding, expr.span),
                    (TypeOrBinding::RetOf(fun, _), TypeOrBinding::RetOf(_, _)) => TypeOrBinding::RetOf(fun, expr.span),
                }
            },
            &BoolAnd(a, b) | &BoolOr(a, b) => {
                for e in &[a, b] {
                    match self.get_type(e) {
                        TypeOrBinding::Binding(binding, span) => self.constraints.push(Constraint::Type(binding, Type::Specific(SpecificType::Bool), span)),
                        TypeOrBinding::Type(_, _) | TypeOrBinding::RetOf(_, _) => (),
                    }
                }
                TypeOrBinding::Type(Type::Specific(SpecificType::Bool), expr.span)
            }
            &BoolNot(expr) => {
                match self.get_type(expr) {
                    TypeOrBinding::Binding(binding, span) => self.constraints.push(Constraint::Type(binding, Type::Specific(SpecificType::Bool), span)),
                    TypeOrBinding::Type(_, _) | TypeOrBinding::RetOf(_, _) => (),
                }
                TypeOrBinding::Type(Type::Specific(SpecificType::Bool), expr.span)
            }
            Block(exprs) => exprs.last().map(|expr| self.get_type(expr))
                .unwrap_or(TypeOrBinding::Type(Type::Specific(SpecificType::Unit), expr.span)),
        }
    }
}
