use crate::parser::{Expr, ExprType};
use crate::typeck::{Constraint, TypeVar};
use crate::common::{SpecificType, Type, PreTypeInfo};
use itertools::Either;
use diagnostic::Diagnostics;
use crate::error_codes::ErrorCode;

pub struct ConstraintCreator<'a, 'i> {
    diagnostics: &'a Diagnostics,
    pre_info: &'a PreTypeInfo<'a, 'i>,
    constraints: Vec<Constraint>,
    restrictions: Vec<(TypeVar, Vec<SpecificType>)>,
}

impl<'a, 'i> ConstraintCreator<'a, 'i> {
    pub fn new(diagnostics: &'a Diagnostics, pre_info: &'a PreTypeInfo<'a, 'i>) -> ConstraintCreator<'a, 'i> {
        ConstraintCreator {
            diagnostics,
            pre_info,
            constraints: Vec::new(),
            restrictions: Vec::new(),
        }
    }
    /// Iterate over the AST, adding function type signatures to the binding types and returning
    /// a set of type inference constraints.
    pub fn get_constraints(mut self, exprs: &[&Expr<'a, 'i>]) -> (Vec<Constraint>, Vec<(TypeVar, Vec<SpecificType>)>) {
        for expr in exprs {
            let val_type_var = self.get_type(expr);
            self.constraints.push(Constraint::Type(val_type_var, Type::Top));
        }
        (self.constraints, self.restrictions)
    }
    #[must_use]
    fn get_type(&mut self, expr_outer: &Expr<'a, 'i>) -> TypeVar {
        use ExprType::*;
        let type_var = TypeVar::new(expr_outer.span);
        match &expr_outer.typ {
            Unit => {
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            Variable(binding) => return TypeVar::new(binding.span),
            Integer(_) => {
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Integer)));
                self.restrictions.push((type_var, vec![SpecificType::Integer]));
            },
            Float(_) => {
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Float)));
                self.restrictions.push((type_var, vec![SpecificType::Float]));
            },
            Bool(_) => {
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            String(_) => {
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::String)));
                self.restrictions.push((type_var, vec![SpecificType::String]));
            },
            &Bind(binding, expr)
            | &Assign((binding, _), expr) => {
                let left = TypeVar::new(binding.span);
                let right = self.get_type(expr);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            &Add(a, b)
            | &Sub(a, b)
            | &Mul(a, b)
            | &Div(a, b) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Eq(type_var, left));
                self.constraints.push(Constraint::Eq(type_var, right));
                self.restrictions.push((left, vec![SpecificType::Integer, SpecificType::Float]));
                self.restrictions.push((right, vec![SpecificType::Integer, SpecificType::Float]));
                self.restrictions.push((type_var, vec![SpecificType::Integer, SpecificType::Float]));
            },
            &BoolAnd(a, b)
            | &BoolOr(a, b) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Type(left, Type::Specific(SpecificType::Bool)));
                self.constraints.push(Constraint::Type(right, Type::Specific(SpecificType::Bool)));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((left, vec![SpecificType::Bool]));
                self.restrictions.push((right, vec![SpecificType::Bool]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            &BoolNot(expr) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::Type(val_type_var, Type::Specific(SpecificType::Bool)));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((val_type_var, vec![SpecificType::Bool]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            LessThan(a, b)
            | LessEquals(a, b)
            | GreaterEquals(a, b)
            | GreaterThan(a, b) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((left, vec![SpecificType::Unit, SpecificType::Integer, SpecificType::Float, SpecificType::Bool, SpecificType::String]));
                self.restrictions.push((right, vec![SpecificType::Unit, SpecificType::Integer, SpecificType::Float, SpecificType::Bool, SpecificType::String]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            Equals(a, b)
            | NotEquals(a, b) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((left, vec![SpecificType::Unit, SpecificType::Integer, SpecificType::Bool, SpecificType::String]));
                self.restrictions.push((right, vec![SpecificType::Unit, SpecificType::Integer, SpecificType::Bool, SpecificType::String]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            FloatEquals(a, b)
            | FloatNotEquals(a, b) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((left, vec![SpecificType::Float, SpecificType::String]));
                self.restrictions.push((right, vec![SpecificType::Float, SpecificType::String]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            Statement(expr) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::Type(val_type_var, Type::Top));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            Block(exprs) => {
                let mut last = None;
                for expr in exprs {
                    last = Some(self.get_type(expr));
                }
                if let Some(last) = last {
                    self.constraints.push(Constraint::Eq(type_var, last));
                } else {
                    self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                    self.restrictions.push((type_var, vec![SpecificType::Unit]));
                }
            },
            Parenthezised(expr) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::Eq(type_var, val_type_var));
            },
            FunctionCall((binding, span), args) => {
                let passed_arg_type_vars: Vec<_> = args.iter().map(|expr| self.get_type(expr)).collect();
                let fun = match &self.pre_info.bindings.get(binding) {
                    Some(SpecificType::Function(f)) => f,
                    _ => {
                        self.diagnostics.error(ErrorCode::NotAFunction)
                            .with_error_label(*span, "this is not a function")
                            .with_info_label(binding.span, "defined here")
                            .emit();
                        return type_var;
                    },
                };
                let expected_arg_types = if fun.args.last() == Some(&Type::Varargs) {
                    Either::Left(fun.args.iter().chain(std::iter::repeat(&Type::Varargs)))
                } else {
                    Either::Right(fun.args.iter())
                };

                // check number of arguments
                let varargs = fun.args.last() == Some(&Type::Varargs);
                let correct_number_of_args = args.len() == fun.args.len()
                    || varargs && args.len() >= fun.args.len() - 1;
                if !correct_number_of_args {
                    let expected_str = if varargs { "at least " } else { "" };
                    let expected = if varargs { fun.args.len() - 1 } else { fun.args.len() };
                    self.diagnostics.error(ErrorCode::InvalidNumberOfArguments)
                        .with_error_label(*span, format!("found {} arguments", args.len()))
                        .with_info_label(binding.span, format!("expected {}{} arguments", expected_str, expected))
                        .emit();
                }

                for (passed_type_var, expected_type) in passed_arg_type_vars.into_iter().zip(expected_arg_types) {
                    match expected_type {
                        Type::Top => unreachable!("function argument type is Top"),
                        Type::Bottom => unreachable!("function argument type is Bottom"),
                        Type::Varargs => self.constraints.push(Constraint::Type(passed_type_var, Type::Varargs)),
                        Type::Specific(typ) => {
                            self.constraints.push(Constraint::Type(passed_type_var, Type::Specific(typ.clone())));
                            self.restrictions.push((passed_type_var, vec![typ.clone()]));
                        }
                    }
                }
            },
            FunctionDefinition(fn_binding, _args, ret_type, body) => {
                if body.is_empty() && *ret_type != SpecificType::Unit {
                    self.diagnostics.error(ErrorCode::EmptyFunctionBody)
                        .with_error_label(fn_binding.span, format!("this function returns {} but has an empty body", ret_type))
                        .emit();
                }
                let mut last = None;
                for expr in body {
                    last = Some(self.get_type(expr));
                }
                if let Some(last) = last {
                    self.constraints.push(Constraint::Type(last, Type::Specific(ret_type.clone())));
                }
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            }
        }
        type_var
    }
}
