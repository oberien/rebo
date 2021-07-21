use crate::parser::{Expr, Spanned, ExprBind, ExprAssign, ExprPattern, ExprPatternUntyped, ExprPatternTyped, ExprVariable, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolAnd, ExprBoolOr, ExprBoolNot, ExprLessThan, ExprGreaterEquals, ExprLessEquals, ExprGreaterThan, ExprEquals, ExprNotEquals, ExprFuzzyEquals, ExprFuzzyNotEquals, ExprBlock, ExprParenthesized, ExprFunctionCall, ExprFunctionDefinition, BlockBody, ExprStructDefinition, ExprType, ExprStructDefFields, ExprStructInitialization};
use crate::typeck::{Constraint, TypeVar};
use crate::common::{SpecificType, Type, PreInfo};
use itertools::{Either, Itertools};
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::lexer::TokenIdent;

pub struct ConstraintCreator<'a, 'i> {
    diagnostics: &'a Diagnostics,
    pre_info: &'a PreInfo<'a, 'i>,
    constraints: Vec<Constraint>,
    restrictions: Vec<(TypeVar, Vec<SpecificType>)>,
}

impl<'a, 'i> ConstraintCreator<'a, 'i> {
    pub fn new(diagnostics: &'a Diagnostics, pre_info: &'a PreInfo<'a, 'i>) -> ConstraintCreator<'a, 'i> {
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
        use Expr::*;
        let type_var = TypeVar::new(expr_outer.span());
        match expr_outer {
            Unit(..) => {
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            Variable(variable) => return TypeVar::new(variable.span()),
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
            Bind(ExprBind { pattern, expr, .. }) => {
                let left = match pattern {
                    ExprPattern::Untyped(ExprPatternUntyped { binding }) => TypeVar::new(binding.span()),
                    ExprPattern::Typed(ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. }) => {
                        let left = TypeVar::new(binding.span());
                        self.constraints.push(Constraint::Type(left, Type::Specific(SpecificType::from(typ))));
                        self.restrictions.push((type_var, vec![SpecificType::from(typ)]));
                        left
                    }
                };
                let right = self.get_type(expr);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            }
            Assign(ExprAssign { variable: ExprVariable { binding, .. }, expr, .. }) => {
                let left = TypeVar::new(binding.span());
                let right = self.get_type(expr);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            Add(ExprAdd { a, b, .. })
            | Sub(ExprSub { a, b, .. })
            | Mul(ExprMul { a, b, .. })
            | Div(ExprDiv { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Eq(type_var, left));
                self.constraints.push(Constraint::Eq(type_var, right));
                self.restrictions.push((left, vec![SpecificType::Integer, SpecificType::Float]));
                self.restrictions.push((right, vec![SpecificType::Integer, SpecificType::Float]));
                self.restrictions.push((type_var, vec![SpecificType::Integer, SpecificType::Float]));
            },
            BoolAnd(ExprBoolAnd { a, b, .. })
            | BoolOr(ExprBoolOr { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Type(left, Type::Specific(SpecificType::Bool)));
                self.constraints.push(Constraint::Type(right, Type::Specific(SpecificType::Bool)));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((left, vec![SpecificType::Bool]));
                self.restrictions.push((right, vec![SpecificType::Bool]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            BoolNot(ExprBoolNot { expr, .. }) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::Type(val_type_var, Type::Specific(SpecificType::Bool)));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((val_type_var, vec![SpecificType::Bool]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            LessThan(ExprLessThan { a, b, .. })
            | LessEquals(ExprLessEquals { a, b, .. })
            | GreaterEquals(ExprGreaterEquals { a, b, .. })
            | GreaterThan(ExprGreaterThan { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((left, vec![SpecificType::Unit, SpecificType::Integer, SpecificType::Float, SpecificType::Bool, SpecificType::String]));
                self.restrictions.push((right, vec![SpecificType::Unit, SpecificType::Integer, SpecificType::Float, SpecificType::Bool, SpecificType::String]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            Equals(ExprEquals { a, b, .. })
            | NotEquals(ExprNotEquals { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((left, vec![SpecificType::Unit, SpecificType::Integer, SpecificType::Bool, SpecificType::String]));
                self.restrictions.push((right, vec![SpecificType::Unit, SpecificType::Integer, SpecificType::Bool, SpecificType::String]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            FuzzyEquals(ExprFuzzyEquals { a, b, .. })
            | FuzzyNotEquals(ExprFuzzyNotEquals { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::Eq(left, right));
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Bool)));
                self.restrictions.push((left, vec![SpecificType::Float, SpecificType::String]));
                self.restrictions.push((right, vec![SpecificType::Float, SpecificType::String]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            Block(ExprBlock { body: BlockBody { exprs, terminated }, .. }) => {
                let mut last = None;
                for expr in exprs {
                    last = Some(self.get_type(expr));
                }
                match (terminated, last) {
                    (false, Some(last_type_var)) => {
                        self.constraints.push(Constraint::Eq(type_var, last_type_var));
                    },
                    (true, _) | (false, None) => {
                        self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                        self.restrictions.push((type_var, vec![SpecificType::Unit]));
                    }
                }
            },
            Parenthesized(ExprParenthesized { expr, .. }) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::Eq(type_var, val_type_var));
            },
            FunctionCall(ExprFunctionCall { variable, args, open, close }) => {
                let args_span = args.span().unwrap_or_else(|| Span::new(open.span.file, open.span.start, close.span.end));
                let passed_arg_type_vars: Vec<_> = args.iter().map(|expr| self.get_type(expr)).collect();
                let fun = match &self.pre_info.bindings.get(&variable.binding) {
                    Some(SpecificType::Function(f)) => f,
                    _ => {
                        self.diagnostics.error(ErrorCode::NotAFunction)
                            .with_error_label(variable.span(), "this is not a function")
                            .with_info_label(variable.binding.span(), "defined here")
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
                        .with_error_label(args_span, format!("found {} arguments", args.len()))
                        .with_info_label(variable.binding.span(), format!("expected {}{} arguments", expected_str, expected))
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
            FunctionDefinition(ExprFunctionDefinition { binding, ret_type, body: ExprBlock { body: BlockBody { exprs, terminated }, .. }, .. }) => {
                let ret_type = match ret_type {
                    Some((_arrow, typ)) => SpecificType::from(typ),
                    None => SpecificType::Unit,
                };
                if exprs.is_empty() && ret_type != SpecificType::Unit {
                    self.diagnostics.error(ErrorCode::EmptyFunctionBody)
                        .with_error_label(binding.span(), format!("this function returns {} but has an empty body", ret_type))
                        .emit();
                }
                let mut last = None;
                for expr in exprs {
                    last = Some(self.get_type(expr));
                }
                match (terminated, last) {
                    (false, Some(last)) => self.constraints.push(Constraint::Type(last, Type::Specific(ret_type.clone()))),
                    (false, None) | (true, _) => {
                        self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                        self.restrictions.push((type_var, vec![SpecificType::Unit]));
                    }
                }
            }
            StructDefinition(ExprStructDefinition { name, fields, .. }) => {
                self.check_struct_fields(name, fields);
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Unit)));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            StructInitialization(ExprStructInitialization { name, fields, .. }) => {
                self.constraints.push(Constraint::Type(type_var, Type::Specific(SpecificType::Struct(name.ident.to_string()))));
                self.restrictions.push((type_var, vec![SpecificType::Struct(name.ident.to_string())]));
                let typ = match self.pre_info.structs.get(name.ident) {
                    Some((typ, _span)) => typ,
                    None => return type_var,
                };
                for (field, _colon, expr) in fields {
                    let field_typ_var = self.get_type(expr);
                    let expected_typ = typ.fields.iter().filter(|(name, _typ)| name == field.ident)
                        .map(|(_name, typ)| typ)
                        .next();
                    let expected_typ = match expected_typ {
                        Some(typ) => typ,
                        None => continue,
                    };
                    self.constraints.push(Constraint::Type(field_typ_var, Type::Specific(expected_typ.clone())));
                    self.restrictions.push((field_typ_var, vec![expected_typ.clone()]))
                }
            }
        }
        type_var
    }

    fn check_struct_fields(&self, name: &TokenIdent, fields: &ExprStructDefFields) {
        // check existence of field types
        for (field, _colon, typ) in fields {
            match typ {
                ExprType::String(_) => (),
                ExprType::Int(_) => (),
                ExprType::Float(_) => (),
                ExprType::Bool(_) => (),
                ExprType::Unit(_, _) => (),
                ExprType::Struct(s) => if self.pre_info.structs.get(s.ident).is_none() {
                    let mut diag = self.diagnostics.error(ErrorCode::TypeNotFound)
                        .with_error_label(typ.span(), "can't find type");
                    if let Some(similar) = crate::util::similar_name(s.ident, self.pre_info.structs.keys().copied()) {
                        diag = diag.with_info_label(typ.span(), format!("did you mean `{}`", similar));
                    }
                    diag.emit()
                }
            }
            // check for (mutual) recursion
            let specific_typ = SpecificType::from(typ);
            self.check_struct_recursion(name, &specific_typ, vec![field.ident]);
        }
    }
    fn check_struct_recursion(&self, outer_name: &TokenIdent, field_typ: &SpecificType, field_stack: Vec<&str>) {
        let inner_name = match field_typ {
            SpecificType::String => return,
            SpecificType::Integer => return,
            SpecificType::Float => return,
            SpecificType::Bool => return,
            SpecificType::Unit => return,
            SpecificType::Function(..) => return,
            SpecificType::Struct(s) => s,
        };
        if outer_name.ident == inner_name {
            let path = field_stack.iter().join(".");
            self.diagnostics.error(ErrorCode::RecursiveStruct)
                .with_error_label(outer_name.span, format!("this struct is recursive via `{}.{}`", outer_name.ident, path))
                .with_note("recursive structs can never be initialized")
                .emit();
            return
        }
        let typ = match self.pre_info.structs.get(&**inner_name) {
            Some((typ, _span)) => typ,
            None => return,
        };
        for (field, typ) in &typ.fields {
            let mut stack = field_stack.clone();
            stack.push(field);
            self.check_struct_recursion(outer_name, typ, stack);
        }
    }
}
