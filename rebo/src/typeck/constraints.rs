use crate::parser::{Expr, Spanned, ExprBind, ExprAssign, ExprPattern, ExprPatternUntyped, ExprPatternTyped, ExprVariable, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolAnd, ExprBoolOr, ExprBoolNot, ExprLessThan, ExprGreaterEquals, ExprLessEquals, ExprGreaterThan, ExprEquals, ExprNotEquals, ExprBlock, ExprParenthesized, ExprFunctionCall, ExprFunctionDefinition, BlockBody, ExprStructDefinition, ExprType, ExprStructDefFields, ExprStructInitialization, ExprAssignLhs, ExprIfElse, ExprWhile, ExprFormatString, ExprFormatStringPart, ExprLiteral, ExprMatch, ExprMatchPattern, ExprImplBlock};
use crate::typeck::{Constraint, TypeVar, ConstraintTyp, MyConstraint, Constraints};
use crate::common::{SpecificType, Type, PreInfo, Function, Mutability};
use itertools::{Either, Itertools};
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::lexer::TokenIdent;
use std::collections::{VecDeque, BTreeMap};
use petgraph::graphmap::GraphMap;

pub struct ConstraintCreator<'a, 'i> {
    diagnostics: &'a Diagnostics,
    meta_info: &'a PreInfo<'a, 'i>,
    constraints: Vec<Constraint>,
    restrictions: Vec<(TypeVar, Vec<SpecificType>)>,
    my_constraints: Constraints<'a>,
}

impl<'a, 'i> ConstraintCreator<'a, 'i> {
    pub fn new(diagnostics: &'a Diagnostics, meta_info: &'a PreInfo<'a, 'i>) -> ConstraintCreator<'a, 'i> {
        ConstraintCreator {
            diagnostics,
            meta_info,
            constraints: Vec::new(),
            restrictions: Vec::new(),
            my_constraints: Constraints::new(diagnostics),
        }
    }
    /// Iterate over the AST, returning a set of type inference constraints.
    pub fn get_constraints(mut self, exprs: &[&'a Expr<'a, 'i>]) -> (Vec<Constraint>, Vec<(TypeVar, Vec<SpecificType>)>) {
        for expr in exprs {
            let val_type_var = self.get_type(expr);
            self.constraints.push(Constraint::new(expr.span(), ConstraintTyp::Type(val_type_var, Type::Top)));
        }
        (self.constraints, self.restrictions)
    }

    #[must_use]
    fn get_literal_type(&mut self, lit: &ExprLiteral) -> TypeVar {
        let type_var = TypeVar::new(lit.span());
        match lit {
            ExprLiteral::Unit(_) => {
                self.constraints.push(Constraint::new(lit.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.my_constraints.add_inital_type(type_var, li.span(), Type::Specific(SpecificType::Unit)),
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            ExprLiteral::Integer(_) => {
                self.constraints.push(Constraint::new(lit.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Integer))));
                self.my_constraints.add_inital_type(type_var, li.span(), Type::Specific(SpecificType::Integer)),
                self.restrictions.push((type_var, vec![SpecificType::Integer]));
            },
            ExprLiteral::Float(_) => {
                self.constraints.push(Constraint::new(lit.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Float))));
                self.my_constraints.add_inital_type(type_var, li.span(), Type::Specific(SpecificType::Float)),
                self.restrictions.push((type_var, vec![SpecificType::Float]));
            },
            ExprLiteral::Bool(_) => {
                self.constraints.push(Constraint::new(lit.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Bool))));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
                self.my_constraints.add_inital_type(type_var, li.span(), Type::Specific(SpecificType::Bool)),
            },
            ExprLiteral::String(_) => {
                self.constraints.push(Constraint::new(lit.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::String))));
                self.my_constraints.add_inital_type(type_var, li.span(), Type::Specific(SpecificType::String)),
                self.restrictions.push((type_var, vec![SpecificType::String]));
            },
        }
        type_var
    }

    #[must_use]
    fn get_type(&mut self, expr_outer: &'a Expr<'a, 'i>) -> TypeVar {
        use Expr::*;
        let type_var = TypeVar::new(expr_outer.span());
        match expr_outer {
            Literal(lit) => { let _ = self.get_literal_type(lit); },
            Variable(variable) => {
                let binding_type_var = TypeVar::new(variable.binding.ident.span());
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::SubEq(binding_type_var, type_var)));
                self.my_constraints.add_constraint(binding_type_var, type_var, expr_outer.span(), MyConstraint::SubEq)
            },
            FieldAccess(field_access) => {
                let initial_var = TypeVar::new(field_access.variable.binding.ident.span());
                let field_names = field_access.fields.iter().map(|f| f.ident.to_string()).collect();
                self.constraints.push(Constraint::new(field_access.span(), ConstraintTyp::FieldAccess(initial_var, field_names.clone(), type_var)));
                self.my_constraints.add_constraint(initial_var, type_var, field_access.span(), MyConstraint::FieldAccess(field_names));
            }
            FormatString(ExprFormatString { parts, .. }) => {
                for part in parts {
                    match part {
                        ExprFormatStringPart::Str(_) => (),
                        ExprFormatStringPart::Escaped(_) => (),
                        ExprFormatStringPart::FmtArg(expr) => {
                            let _ = self.get_type(expr);
                        },
                    }
                }
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::String))));
                self.my_constraints.add_inital_type(type_var, expr_outer.span(), Type::Specific(SpecificType::String));
                self.restrictions.push((type_var, vec![SpecificType::String]));
            },
            Bind(ExprBind { pattern, expr, .. }) => {
                let binding_type_var = match pattern {
                    ExprPattern::Untyped(ExprPatternUntyped { binding }) => TypeVar::new(binding.ident.span()),
                    ExprPattern::Typed(typed) => {
                        let pattern_span = typed.span();
                        let ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. } = typed;
                        let left = TypeVar::new(binding.ident.span());
                        let mutability = Mutability::from(typed.pattern.binding.mutable.is_some());
                        self.constraints.push(Constraint::new(pattern_span, ConstraintTyp::Type(left, Type::Specific(SpecificType::from(mutability, typ.clone())))));
                        self.my_constraints.add_inital_type(left, pattern_span, Type::Specific(SpecificType::from(mutability, typ)));
                        self.restrictions.push((type_var, vec![SpecificType::from(mutability, typ)]));
                        left
                    }
                };
                let value_type_var = self.get_type(expr);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::SubEq(binding_type_var, value_type_var)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.my_constraints.add_constraint(binding_type_var, value_type_var, expr_outer.span(), MyConstraint::SubEq);
                self.my_constraints.add_initial_type(type_var, expr_outer.span(), Type::Specific(SpecificType::Unit));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            }
            Assign(ExprAssign { lhs, expr, .. }) => {
                let binding_type_var = match lhs {
                    ExprAssignLhs::Variable(ExprVariable { binding, .. }) => TypeVar::new(binding.ident.span()),
                    ExprAssignLhs::FieldAccess(field_access) => {
                        let initial_var = TypeVar::new(field_access.variable.binding.ident.span());
                        let field_type_var = TypeVar::new(field_access.span());
                        let field_names = field_access.fields.iter().map(|f| f.ident.to_string()).collect();
                        self.constraints.push(Constraint::new(field_access.span(), ConstraintTyp::FieldAccess(initial_var, field_names.clone(), field_type_var)));
                        self.my_constraints.add_constraint(initial_var, field_type_var, field_access.span(), MyConstraint::FieldAccess(field_names));
                        field_type_var
                    }
                };
                let value_type_var = self.get_type(expr);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::SubEq(binding_type_var, value_type_var)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.my_constraints.add_constraint(binding_type_var, value_type_var, expr_outer.span(), MyConstraint::SubEq);
                self.my_constraints.add_inital_type(type_var, expr_outer.span(), Type::Specific(SpecificType::Unit));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            Add(ExprAdd { a, b, .. })
            | Sub(ExprSub { a, b, .. })
            | Mul(ExprMul { a, b, .. })
            | Div(ExprDiv { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Similar(type_var, left)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Similar(type_var, right)));
                self.my_constraints.add_constraint(type_var, left, expr_outer.span(), MyConstraint::Eq);
                self.my_constraints.add_constraint(type_var, right, expr_outer.span(), MyConstraint::Eq);
                self.restrictions.push((left, vec![SpecificType::Integer, SpecificType::Float]));
                self.restrictions.push((right, vec![SpecificType::Integer, SpecificType::Float]));
                self.restrictions.push((type_var, vec![SpecificType::Integer, SpecificType::Float]));
            },
            BoolAnd(ExprBoolAnd { a, b, .. })
            | BoolOr(ExprBoolOr { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(left, Type::Specific(SpecificType::Bool))));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(right, Type::Specific(SpecificType::Bool))));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Bool))));
                self.my_constraints.add_inital_type(left, expr_outer.span(), Type::Specific(SpecificType::Bool));
                self.my_constraints.add_inital_type(right, expr_outer.span(), Type::Specific(SpecificType::Bool));
                self.my_constraints.add_inital_type(type_varr, expr_outer.span(), Type::Specific(SpecificType::Bool));
                self.restrictions.push((left, vec![SpecificType::Bool]));
                self.restrictions.push((right, vec![SpecificType::Bool]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            BoolNot(ExprBoolNot { expr, .. }) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(val_type_var, Type::Specific(SpecificType::Bool))));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Bool))));
                self.my_constraints.add_inital_type(val_type_var, expr_outer.span(), Type::Specific(SpecificType::Bool));
                self.my_constraints.add_inital_type(type_var, expr_outer.span(), Type::Specific(SpecificType::Bool));
                self.restrictions.push((val_type_var, vec![SpecificType::Bool]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            LessThan(ExprLessThan { a, b, .. })
            | LessEquals(ExprLessEquals { a, b, .. })
            | GreaterEquals(ExprGreaterEquals { a, b, .. })
            | GreaterThan(ExprGreaterThan { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Similar(left, right)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Bool))));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            Equals(ExprEquals { a, b, .. })
            | NotEquals(ExprNotEquals { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Similar(left, right)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Bool))));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            Block(ExprBlock { body: BlockBody { exprs, terminated }, .. }) => {
                let mut last = None;
                for expr in exprs {
                    last = Some(self.get_type(expr));
                }
                match (terminated, last) {
                    (false, Some(last_type_var)) => {
                        self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::SubEq(type_var, last_type_var)));
                    },
                    (true, _) | (false, None) => {
                        self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                        self.restrictions.push((type_var, vec![SpecificType::Unit]));
                    }
                }
            },
            Parenthesized(ExprParenthesized { expr, .. }) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::SubEq(type_var, val_type_var)));
            },
            IfElse(ifelse) => {
                let ifelse_span = ifelse.span();
                let ExprIfElse { condition, then, else_ifs, els, .. } = ifelse;

                // find out if the `if` has a value
                let then_with_value = !then.body.terminated;
                let else_if_with_value = else_ifs.iter().any(|(_, _, _, block)| !block.body.terminated);
                let else_with_value = els.iter().any(|(_, block)| !block.body.terminated);
                let with_value = then_with_value || else_if_with_value || else_with_value;

                let branches = ::std::iter::once((Some(condition), then))
                    .chain(else_ifs.iter().map(|(_, _, cond, block)| (Some(cond), block)))
                    .chain(els.iter().map(|(_, block)| (None, block)));

                let mut branch_type_vars = VecDeque::new();
                for (cond, block) in branches {
                    // cond
                    if let Some(cond) = cond {
                        let cond_type_var = self.get_type(cond);
                        self.constraints.push(Constraint::new(condition.span(), ConstraintTyp::Type(cond_type_var, Type::Specific(SpecificType::Bool))));
                        self.restrictions.push((cond_type_var, vec![SpecificType::Bool]));
                    }
                    // block
                    let mut last = None;
                    for expr in &block.body.exprs {
                        last = Some((expr.span(), self.get_type(expr)));
                    }
                    branch_type_vars.extend(last);
                }

                if with_value {
                    for (span, var) in branch_type_vars {
                        self.constraints.push(Constraint::new(span, ConstraintTyp::SubEq(type_var, var)));
                    }
                } else {
                    for (span, var) in branch_type_vars.into_iter().chain(Some((expr_outer.span(), type_var))) {
                        self.constraints.push(Constraint::new(span, ConstraintTyp::Type(var, Type::Specific(SpecificType::Unit))));
                        self.restrictions.push((var, vec![SpecificType::Unit]));
                    }
                }
            }
            Match(ExprMatch { expr, arms, .. }) => {
                let expr_type_var = self.get_type(expr);
                for (pattern, _arrow, arm_expr) in arms {
                    let pattern_type_var = match pattern {
                        ExprMatchPattern::Literal(lit) => self.get_literal_type(lit),
                        ExprMatchPattern::Wildcard(wildcard) => {
                            let type_var = TypeVar::new(wildcard.span());
                            self.constraints.push(Constraint::new(wildcard.span(), ConstraintTyp::Type(type_var, Type::Top)));
                            type_var
                        }
                        ExprMatchPattern::Binding(binding) => TypeVar::new(binding.ident.span()),
                    };
                    let arm_expr_type_var = self.get_type(arm_expr);
                    self.constraints.push(Constraint::new(arm_expr.span(), ConstraintTyp::SubEq(type_var, arm_expr_type_var)));
                    self.constraints.push(Constraint::new(pattern.span(), ConstraintTyp::SubEq(pattern_type_var, expr_type_var)));
                }
            }
            While(ExprWhile { condition, block, .. }) => {
                let cond_type_var = self.get_type(condition);
                self.constraints.push(Constraint::new(condition.span(), ConstraintTyp::Type(cond_type_var, Type::Specific(SpecificType::Bool))));
                self.restrictions.push((cond_type_var, vec![SpecificType::Bool]));
                for expr in &block.body.exprs {
                    let _ = self.get_type(expr);
                }
            }
            FunctionCall(ExprFunctionCall { name, args, open, close }) => {
                let args_span = args.span().unwrap_or_else(|| Span::new(open.span.file, open.span.start, close.span.end));
                let passed_arg_type_vars: Vec<_> = args.iter().map(|expr| (expr.span(), self.get_type(expr))).collect();
                let fun = match &self.meta_info.functions.get(name.ident) {
                    Some(Function { typ, imp: _ }) => typ,
                    _ => {
                        let similar = crate::util::similar_name(name.ident, self.meta_info.rebo_functions.keys());
                        let mut diag = self.diagnostics.error(ErrorCode::UnknownFunction)
                            .with_error_label(name.span, "can't find function with this same");
                        if let Some(similar) = similar {
                            diag = diag.with_info_label(name.span, format!("did you mean `{}`", similar));
                        }
                        diag.emit();
                        return type_var;
                    },
                };
                let expected_arg_types = if let Some(typ @ &Type::Varargs(_)) = fun.args.last() {
                    Either::Left(fun.args.iter().chain(std::iter::repeat(typ)))
                } else {
                    Either::Right(fun.args.iter())
                };

                // check number of arguments
                let varargs = matches!(fun.args.last(), Some(&Type::Varargs(_)));
                let correct_number_of_args = args.len() == fun.args.len()
                    || varargs && args.len() >= fun.args.len() - 1;
                if !correct_number_of_args {
                    let expected_str = if varargs { "at least " } else { "" };
                    let expected = if varargs { fun.args.len() - 1 } else { fun.args.len() };
                    self.diagnostics.error(ErrorCode::InvalidNumberOfArguments)
                        .with_error_label(args_span, format!("found {} arguments", args.len()))
                        .with_info_label(name.span, format!("expected {}{} arguments", expected_str, expected))
                        .emit();
                }

                for ((span, passed_type_var), expected_type) in passed_arg_type_vars.into_iter().zip(expected_arg_types) {
                    match expected_type {
                        Type::Top => unreachable!("function argument type is Top"),
                        Type::Bottom => unreachable!("function argument type is Bottom"),
                        t @ Type::Varargs(_) => self.constraints.push(Constraint::new(span, ConstraintTyp::Type(passed_type_var, t.clone()))),
                        Type::Specific(typ) => {
                            self.constraints.push(Constraint::new(span, ConstraintTyp::Type(passed_type_var, Type::Specific(typ.clone()))));
                            self.restrictions.push((passed_type_var, vec![typ.clone()]));
                        }
                    }
                }
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, fun.ret.clone())));
                if let Type::Specific(ret) = &fun.ret {
                    self.restrictions.push((type_var, vec![ret.clone()]));
                }
            },
            FunctionDefinition(function_definition) => {
                self.get_function_definition_type(function_definition);
            }
            StructDefinition(ExprStructDefinition { name, fields, .. }) => {
                self.check_struct_fields(name, fields);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            StructInitialization(ExprStructInitialization { name, fields, .. }) => {
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Struct(Mutability::Mutable, name.ident.to_string())))));
                self.restrictions.push((type_var, vec![SpecificType::Struct(Mutability::Mutable, name.ident.to_string())]));
                let typ = match self.meta_info.structs.get(name.ident) {
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
                    self.constraints.push(Constraint::new(expr.span(), ConstraintTyp::Type(field_typ_var, Type::Specific(expected_typ.clone()))));
                    self.restrictions.push((field_typ_var, vec![expected_typ.clone()]))
                }
            }
            ImplBlock(ExprImplBlock { name, functions, .. }) => {
                if self.meta_info.structs.get(name.ident).is_none() {
                    let similar = crate::util::similar_name(name.ident, self.meta_info.structs.keys());
                    let mut diag = self.diagnostics.error(ErrorCode::UnknownImplBlockTarget)
                        .with_error_label(name.span, "can't find this type");
                    if let Some(similar) = similar {
                        diag = diag.with_info_label(name.span, format!("did you mean `{}`", similar));
                    }
                    diag.emit();
                }
                for function_definition in functions {
                    self.get_function_definition_type(function_definition);
                }
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.restrictions.push((type_var, vec![SpecificType::Unit]))
            }
        }
        type_var
    }

    fn get_function_definition_type(&mut self, function_definition: &'a ExprFunctionDefinition<'a, 'i>) {
        let ExprFunctionDefinition { name, ret_type, body: ExprBlock { body: BlockBody { exprs, terminated }, .. }, args, .. } = function_definition;

        // arg types
        for arg in args {
            let ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. } = arg;
            let arg_type_var = TypeVar::new(binding.ident.span);
            let typ = SpecificType::from(Mutability::from(binding.mutable.is_some()), typ);
            self.constraints.push(Constraint::new(arg.span(), ConstraintTyp::Type(arg_type_var, Type::Specific(typ.clone()))));
            self.restrictions.push((arg_type_var, vec![typ]));
        }

        let type_var = TypeVar::new(function_definition.span());
        let mut last = None;
        for expr in exprs {
            last = Some((expr.span(), self.get_type(expr)));
        }
        match (terminated, last) {
            (false, Some((span, last))) => self.constraints.push(Constraint::new(span, ConstraintTyp::Type(last, Type::Specific(ret_type.clone())))),
            (false, None) | (true, _) => {
                self.constraints.push(Constraint::new(function_definition.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            }
        }
    }
}
