use crate::parser::{Expr, Spanned, ExprBind, ExprAssign, ExprPattern, ExprPatternUntyped, ExprPatternTyped, ExprVariable, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolAnd, ExprBoolOr, ExprBoolNot, ExprLessThan, ExprGreaterEquals, ExprLessEquals, ExprGreaterThan, ExprEquals, ExprNotEquals, ExprBlock, ExprParenthesized, ExprFunctionCall, ExprFunctionDefinition, BlockBody, ExprStructDefinition, ExprType, ExprStructDefFields, ExprStructInitialization, ExprAssignLhs, ExprIfElse, ExprWhile, ExprFormatString, ExprFormatStringPart};
use crate::typeck::{Constraint, TypeVar, ConstraintTyp};
use crate::common::{SpecificType, Type, PreInfo};
use itertools::{Either, Itertools};
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::lexer::TokenIdent;
use std::collections::VecDeque;

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
    /// Iterate over the AST, returning a set of type inference constraints.
    pub fn get_constraints(mut self, exprs: &[&Expr<'a, 'i>]) -> (Vec<Constraint>, Vec<(TypeVar, Vec<SpecificType>)>) {
        for expr in exprs {
            let val_type_var = self.get_type(expr);
            self.constraints.push(Constraint::new(expr.span(), ConstraintTyp::Type(val_type_var, Type::Top)));
        }
        (self.constraints, self.restrictions)
    }
    #[must_use]
    fn get_type(&mut self, expr_outer: &Expr<'a, 'i>) -> TypeVar {
        use Expr::*;
        let type_var = TypeVar::new(expr_outer.span());
        match expr_outer {
            Unit(..) => {
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            Variable(variable) => {
                return TypeVar::new(variable.binding.ident.span())
            },
            FieldAccess(field_access) => {
                let initial_var = TypeVar::new(field_access.variable.binding.ident.span());
                let field_names = field_access.fields.iter().map(|f| f.ident.to_string()).collect();
                self.constraints.push(Constraint::new(field_access.span(), ConstraintTyp::FieldAccess(initial_var, field_names, type_var)));
            }
            Integer(_) => {
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Integer))));
                self.restrictions.push((type_var, vec![SpecificType::Integer]));
            },
            Float(_) => {
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Float))));
                self.restrictions.push((type_var, vec![SpecificType::Float]));
            },
            Bool(_) => {
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Bool))));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            String(_) => {
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::String))));
                self.restrictions.push((type_var, vec![SpecificType::String]));
            },
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
                self.restrictions.push((type_var, vec![SpecificType::String]));
            },
            Bind(ExprBind { pattern, expr, .. }) => {
                let left = match pattern {
                    ExprPattern::Untyped(ExprPatternUntyped { binding }) => TypeVar::new(binding.ident.span()),
                    ExprPattern::Typed(typed) => {
                        let pattern_span = typed.span();
                        let ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. } = typed;
                        let left = TypeVar::new(binding.ident.span());
                        self.constraints.push(Constraint::new(pattern_span, ConstraintTyp::Type(left, Type::Specific(SpecificType::from(typ)))));
                        self.restrictions.push((type_var, vec![SpecificType::from(typ)]));
                        left
                    }
                };
                let right = self.get_type(expr);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Eq(left, right)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            }
            Assign(ExprAssign { lhs, expr, .. }) => {
                let left = match lhs {
                    ExprAssignLhs::Variable(ExprVariable { binding, .. }) => TypeVar::new(binding.ident.span()),
                    ExprAssignLhs::FieldAccess(field_access) => {
                        let initial_var = TypeVar::new(field_access.variable.binding.ident.span());
                        let field_type_var = TypeVar::new(field_access.span());
                        let field_names = field_access.fields.iter().map(|f| f.ident.to_string()).collect();
                        self.constraints.push(Constraint::new(field_access.span(), ConstraintTyp::FieldAccess(initial_var, field_names, field_type_var)));
                        field_type_var
                    }
                };
                let right = self.get_type(expr);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Eq(left, right)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            Add(ExprAdd { a, b, .. })
            | Sub(ExprSub { a, b, .. })
            | Mul(ExprMul { a, b, .. })
            | Div(ExprDiv { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Eq(type_var, left)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Eq(type_var, right)));
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
                self.restrictions.push((left, vec![SpecificType::Bool]));
                self.restrictions.push((right, vec![SpecificType::Bool]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            BoolNot(ExprBoolNot { expr, .. }) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(val_type_var, Type::Specific(SpecificType::Bool))));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Bool))));
                self.restrictions.push((val_type_var, vec![SpecificType::Bool]));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            }
            LessThan(ExprLessThan { a, b, .. })
            | LessEquals(ExprLessEquals { a, b, .. })
            | GreaterEquals(ExprGreaterEquals { a, b, .. })
            | GreaterThan(ExprGreaterThan { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Eq(left, right)));
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Bool))));
                self.restrictions.push((type_var, vec![SpecificType::Bool]));
            },
            Equals(ExprEquals { a, b, .. })
            | NotEquals(ExprNotEquals { a, b, .. }) => {
                let left = self.get_type(a);
                let right = self.get_type(b);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Eq(left, right)));
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
                        self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Eq(type_var, last_type_var)));
                    },
                    (true, _) | (false, None) => {
                        self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                        self.restrictions.push((type_var, vec![SpecificType::Unit]));
                    }
                }
            },
            Parenthesized(ExprParenthesized { expr, .. }) => {
                let val_type_var = self.get_type(expr);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Eq(type_var, val_type_var)));
            },
            IfElse(ifelse) => {
                let ifelse_span = ifelse.span();
                let ExprIfElse { condition, then, else_ifs, els, .. } = ifelse;

                // find out if the `if` has a value
                let then_with_value = !then.body.terminated;
                let else_if_with_value = else_ifs.iter().any(|(_, _, _, block)| !block.body.terminated);
                let else_with_value = els.iter().any(|(_, block)| !block.body.terminated);
                let with_value = then_with_value || else_if_with_value || else_with_value;

                if with_value && els.is_none() {
                    self.diagnostics.error(ErrorCode::MissingElse)
                        .with_error_label(ifelse_span, "missing else-branch in this if")
                        .with_note("if the `if` should return a value, all branches must return a value")
                        .with_note("if the `if` should not return a value, all branches must be terminated with a `;`")
                        .emit();
                }

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

                    // if returns value but block is empty
                    if with_value && block.body.exprs.is_empty() {
                        self.diagnostics.error(ErrorCode::MissingBranchBody)
                            .with_error_label(block.span(), "this branch body is expected to evaluate to a value")
                            .with_info_label(ifelse_span, "all branches in this if must evaluate to a value")
                            .with_note("if not all branches are terminated with a `;` an if-expression evalutes to a value")
                            .emit();
                    // if returns value but block is terminated
                    } else if with_value && block.body.terminated {
                        self.diagnostics.error(ErrorCode::MissingBranchValue)
                            .with_error_label(block.span(), "this block doesn't evaluate to a value")
                            .with_info_label(ifelse_span, "all branches in this if must evaluate to a value")
                            .with_note("if not all branches are terminated with a `;` an if-expression evalutes to a value")
                            .emit();
                    }
                }

                if with_value {
                    for (span, var) in branch_type_vars {
                        self.constraints.push(Constraint::new(span, ConstraintTyp::Eq(type_var, var)));
                    }
                } else {
                    for (span, var) in branch_type_vars.into_iter().chain(Some((expr_outer.span(), type_var))) {
                        self.constraints.push(Constraint::new(span, ConstraintTyp::Type(var, Type::Specific(SpecificType::Unit))));
                        self.restrictions.push((var, vec![SpecificType::Unit]));
                    }
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
            FunctionCall(ExprFunctionCall { variable, args, open, close }) => {
                let args_span = args.span().unwrap_or_else(|| Span::new(open.span.file, open.span.start, close.span.end));
                let passed_arg_type_vars: Vec<_> = args.iter().map(|expr| (expr.span(), self.get_type(expr))).collect();
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

                for ((span, passed_type_var), expected_type) in passed_arg_type_vars.into_iter().zip(expected_arg_types) {
                    match expected_type {
                        Type::Top => unreachable!("function argument type is Top"),
                        Type::Bottom => unreachable!("function argument type is Bottom"),
                        Type::Varargs => self.constraints.push(Constraint::new(span, ConstraintTyp::Type(passed_type_var, Type::Varargs))),
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
                    last = Some((expr.span(), self.get_type(expr)));
                }
                match (terminated, last) {
                    (false, Some((span, last))) => self.constraints.push(Constraint::new(span, ConstraintTyp::Type(last, Type::Specific(ret_type.clone())))),
                    (false, None) | (true, _) => {
                        self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                        self.restrictions.push((type_var, vec![SpecificType::Unit]));
                    }
                }
            }
            StructDefinition(ExprStructDefinition { name, fields, .. }) => {
                self.check_struct_fields(name, fields);
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Unit))));
                self.restrictions.push((type_var, vec![SpecificType::Unit]));
            },
            StructInitialization(ExprStructInitialization { name, fields, .. }) => {
                self.constraints.push(Constraint::new(expr_outer.span(), ConstraintTyp::Type(type_var, Type::Specific(SpecificType::Struct(name.ident.to_string())))));
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
                    self.constraints.push(Constraint::new(expr.span(), ConstraintTyp::Type(field_typ_var, Type::Specific(expected_typ.clone()))));
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
