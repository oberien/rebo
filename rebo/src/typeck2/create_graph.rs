use crate::typeck2::graph::Graph;
use crate::parser::{Expr, Spanned, ExprFormatString, ExprFormatStringPart, ExprBind, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprAssign, ExprAssignLhs, ExprVariable, ExprFieldAccess, ExprBoolNot, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolAnd, ExprBoolOr, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprBlock, BlockBody, ExprParenthesized, ExprMatch, ExprMatchPattern, ExprWhile, ExprFunctionCall, ExprFunctionDefinition, ExprStructInitialization, ExprImplBlock};
use crate::typeck2::TypeVar;
use crate::common::{SpecificType, MetaInfo, Type};
use itertools::Either;

pub fn create_graph(meta_info: &MetaInfo, exprs: &[&Expr<'_, '_>]) -> Graph {
    let mut graph = Graph::new();
    for expr in exprs {
        visit_expr(&mut graph, meta_info, expr);
    }
    graph
}

fn visit_expr(graph: &mut Graph, meta_info: &MetaInfo, expr: &Expr<'_, '_>) -> TypeVar {
    let type_var = TypeVar::new(expr.span());
    graph.add_type_var(type_var);
    match expr {
        Expr::Literal(lit) => graph.set_exact_type(type_var, SpecificType::from(lit)),
        Expr::FormatString(ExprFormatString { parts, .. }) => {
            graph.set_exact_type(type_var, SpecificType::String);
            for part in parts {
                match part {
                    ExprFormatStringPart::Str(_)
                    | ExprFormatStringPart::Escaped(_) => (),
                    ExprFormatStringPart::FmtArg(expr) => { visit_expr(graph, meta_info, expr); }
                }
            }
        }
        Expr::Bind(ExprBind { pattern, expr, .. }) => {
            graph.set_exact_type(type_var, SpecificType::Unit);
            let var_type_var = match pattern {
                ExprPattern::Typed(ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. }) => {
                    let var_type_var = TypeVar::new(binding.ident.span);
                    graph.add_type_var(var_type_var);
                    graph.set_exact_type(var_type_var, SpecificType::from(typ));
                    var_type_var
                }
                ExprPattern::Untyped(ExprPatternUntyped { binding }) => {
                    let var_type_var = TypeVar::new(binding.ident.span);
                    graph.add_type_var(var_type_var);
                    var_type_var
                }
            };
            let expr_type_var = visit_expr(graph, meta_info, expr);
            graph.add_eq_constraint(var_type_var, expr_type_var);
        }
        Expr::Assign(ExprAssign { lhs, expr, .. }) => {
            graph.set_exact_type(type_var, SpecificType::Unit);
            let expr_type_var = visit_expr(graph, meta_info, expr);
            match lhs {
                ExprAssignLhs::Variable(ExprVariable { binding, .. }) => {
                    let var_type_var = TypeVar::new(binding.ident.span);
                    graph.add_type_var(var_type_var);
                    graph.add_eq_constraint(var_type_var, expr_type_var);
                }
                ExprAssignLhs::FieldAccess(ExprFieldAccess { variable: ExprVariable{ binding, .. }, fields, .. }) => {
                    let var_type_var = TypeVar::new(binding.ident.span);
                    let field_type_var = TypeVar::new(lhs.span());
                    graph.add_type_var(var_type_var);
                    graph.add_type_var(field_type_var);
                    let fields = fields.iter().map(|ident| ident.ident.to_string()).collect();
                    graph.add_field_access(var_type_var, field_type_var, fields);
                    graph.add_eq_constraint(field_type_var, expr_type_var);
                }
            }
        }
        Expr::BoolNot(ExprBoolNot { expr, .. }) => {
            let expr_type_var = visit_expr(graph, meta_info, expr);
            graph.set_exact_type(type_var, SpecificType::Bool);
            graph.add_eq_constraint(type_var, expr_type_var);
        }
        Expr::Add(ExprAdd { a, b, .. })
        | Expr::Sub(ExprSub { a, b, .. })
        | Expr::Mul(ExprMul { a, b, .. })
        | Expr::Div(ExprDiv { a, b, .. }) => {
            let a_type_var = visit_expr(graph, meta_info, a);
            let b_type_var = visit_expr(graph, meta_info, b);
            graph.add_reduce_constraint(type_var, a_type_var, vec![SpecificType::Integer, SpecificType::Float]);
            graph.add_reduce_constraint(type_var, b_type_var, vec![SpecificType::Integer, SpecificType::Float]);
            graph.add_reduce_constraint(type_var, type_var, vec![SpecificType::Integer, SpecificType::Float]);
            graph.add_eq_constraint(a_type_var, type_var);
            graph.add_eq_constraint(b_type_var, type_var);
        }
        Expr::BoolAnd(ExprBoolAnd { a, b, .. })
        | Expr::BoolOr(ExprBoolOr { a, b, .. }) => {
            let a_type_var = visit_expr(graph, meta_info, a);
            let b_type_var = visit_expr(graph, meta_info, b);
            graph.set_exact_type(type_var, SpecificType::Bool);
            graph.add_eq_constraint(type_var, a_type_var);
            graph.add_eq_constraint(type_var, b_type_var);
        }
        Expr::LessThan(ExprLessThan { a, b, .. })
        | Expr::LessEquals(ExprLessEquals { a, b, .. })
        | Expr::Equals(ExprEquals { a, b, .. })
        | Expr::NotEquals(ExprNotEquals { a, b, .. })
        | Expr::GreaterEquals(ExprGreaterEquals { a, b, .. })
        | Expr::GreaterThan(ExprGreaterThan { a, b, .. }) => {
            let a_type_var = visit_expr(graph, meta_info, a);
            let b_type_var = visit_expr(graph, meta_info, b);
            graph.set_exact_type(type_var, SpecificType::Bool);
            graph.add_eq_constraint(a_type_var, b_type_var);
        }
        Expr::Block(block) => {
            assert_eq!(visit_block(graph, meta_info, block), type_var);
        }
        Expr::Variable(ExprVariable { binding, .. }) => {
            let var_type_var = TypeVar::new(binding.ident.span);
            graph.add_type_var(var_type_var);
            graph.add_unidirectional_eq_constraint(var_type_var, type_var);
            return var_type_var;
        }
        Expr::FieldAccess(ExprFieldAccess { variable, fields, .. }) => {
            let var_type_var = TypeVar::new(variable.span());
            let binding_type_var = TypeVar::new(variable.binding.ident.span);
            graph.add_type_var(var_type_var);
            graph.add_type_var(binding_type_var);
            graph.add_unidirectional_eq_constraint(binding_type_var, var_type_var);
            let fields = fields.iter().map(|ident| ident.ident.to_string()).collect();
            graph.add_field_access(binding_type_var, type_var, fields);
        }
        Expr::Parenthesized(ExprParenthesized { expr, .. }) => {
            let expr_type_var = visit_expr(graph, meta_info, expr);
            graph.add_eq_constraint(expr_type_var, type_var);
        }
        Expr::IfElse(ifelse) => {
            let mut block_type_vars = Vec::new();
            for (condition, body) in ifelse.iter_branches() {
                if let Some(condition) = condition {
                    let cond_type_var = visit_expr(graph, meta_info, condition);
                    graph.add_reduce_constraint(type_var, cond_type_var, vec![SpecificType::Bool]);
                }
                block_type_vars.push(visit_block(graph, meta_info, body));
            }
            for var in block_type_vars {
                graph.add_eq_constraint(type_var, var);
            }
        }
        Expr::Match(ExprMatch { expr, arms, .. }) => {
            let expr_type_var = visit_expr(graph, meta_info, expr);
            for (pat, _arrow, arm_expr) in arms {
                match pat {
                    ExprMatchPattern::Literal(lit) => {
                        let pat_type_var = TypeVar::new(pat.span());
                        graph.add_type_var(pat_type_var);
                        graph.set_exact_type(pat_type_var, SpecificType::from(lit));
                        graph.add_eq_constraint(expr_type_var, pat_type_var);
                    }
                    ExprMatchPattern::Binding(binding) => {
                        let binding_type_var = TypeVar::new(binding.ident.span);
                        graph.add_type_var(binding_type_var);
                        graph.add_eq_constraint(binding_type_var, expr_type_var);
                    }
                    ExprMatchPattern::Wildcard(_) => (),
                }
                let arm_expr_type_var = visit_expr(graph, meta_info, arm_expr);
                graph.add_eq_constraint(type_var, arm_expr_type_var);
            }
        }
        Expr::While(ExprWhile { condition, block, .. }) => {
            let cond_type_var = visit_expr(graph, meta_info, condition);
            graph.add_reduce_constraint(type_var, cond_type_var, vec![SpecificType::Bool]);
            let block_type_var = visit_block(graph, meta_info, block);
            graph.add_eq_constraint(type_var, block_type_var);
        }
        Expr::FunctionCall(ExprFunctionCall { name, args, .. }) => {
            let fun = match meta_info.functions.get(name.ident) {
                Some(fun) => fun,
                None => return type_var,
            };
            let passed_args: Vec<_> = args.iter().map(|expr| visit_expr(graph, meta_info, expr)).collect();
            let expected_args = if let Some(Type::Varargs) = fun.typ.args.last() {
                Either::Left(fun.typ.args.iter().chain(std::iter::repeat(&Type::Varargs)))
            } else {
                Either::Right(fun.typ.args.iter())
            };
            for (passed_type_var, expected) in passed_args.into_iter().zip(expected_args) {
                match expected {
                    Type::Top => unreachable!("function argument type is Top"),
                    Type::Bottom => unreachable!("function argument type is Bottom"),
                    Type::Varargs => (),
                    Type::Specific(specific) => {
                        graph.add_reduce_constraint(type_var, passed_type_var, vec![specific.clone()]);
                    }
                }
            }
            match &fun.typ.ret {
                Type::Varargs => unreachable!("function return type is Varargs"),
                Type::Top | Type::Bottom => (),
                Type::Specific(specific) => graph.set_exact_type(type_var, specific.clone()),
            }
        }
        Expr::FunctionDefinition(function) => {
            visit_function(graph, meta_info, function);
        }
        Expr::StructDefinition(_) => graph.set_exact_type(type_var, SpecificType::Unit),
        Expr::StructInitialization(ExprStructInitialization { name, fields, .. }) => {
            graph.set_exact_type(type_var, SpecificType::Struct(name.ident.to_string()));
            let (struct_typ, struct_def_span) = match meta_info.structs.get(name.ident) {
                Some((typ, span)) => (typ, span),
                None => return type_var,
            };
            let struct_def_type_var = TypeVar::new(*struct_def_span);
            graph.add_type_var(struct_def_type_var);
            graph.set_exact_type(struct_def_type_var, SpecificType::Unit);

            for (name, _colon, expr) in fields {
                let expected_type = struct_typ.fields.iter()
                    .filter(|(field_name, _typ)| field_name == name.ident)
                    .map(|(_name, typ)| typ)
                    .next();
                let expected_type = match expected_type {
                    Some(typ) => typ,
                    None => continue,
                };
                let expr_type_var = visit_expr(graph, meta_info, expr);
                graph.add_reduce_constraint(struct_def_type_var, expr_type_var, vec![expected_type.clone()]);
            }
        }
        Expr::ImplBlock(ExprImplBlock { functions, .. }) => {
            for function in functions {
                visit_function(graph, meta_info, function);
            }
            graph.set_exact_type(type_var, SpecificType::Unit);
        }
    }
    type_var
}

fn visit_block(graph: &mut Graph, meta_info: &MetaInfo, block: &ExprBlock) -> TypeVar {
    let type_var = TypeVar::new(block.span());
    graph.add_type_var(type_var);
    let ExprBlock { body: BlockBody { exprs, terminated_with_semicolon }, .. } = block;

    let mut last = None;
    for expr in exprs {
        last = Some(visit_expr(graph, meta_info, expr));
    }
    match (terminated_with_semicolon, last) {
        (true, _) | (false, None) => graph.set_exact_type(type_var, SpecificType::Unit),
        (false, Some(var)) => graph.add_eq_constraint(type_var, var),
    }
    type_var
}

fn visit_function(graph: &mut Graph, meta_info: &MetaInfo, function: &ExprFunctionDefinition) -> TypeVar {
    let type_var = TypeVar::new(function.span());
    graph.add_type_var(type_var);
    let ExprFunctionDefinition { args, ret_type, body, .. } = function;

    for ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. } in args {
        let arg_type_var = TypeVar::new(binding.ident.span);
        graph.add_type_var(arg_type_var);
        graph.add_reduce_constraint(type_var, arg_type_var, vec![SpecificType::from(typ)]);
    }

    let body_type_var = visit_block(graph, meta_info, body);
    if let Some((_arrow, ret_type)) = ret_type {
        graph.add_reduce_constraint(type_var, body_type_var, vec![SpecificType::from(ret_type)]);
    }
    graph.set_exact_type(type_var, SpecificType::Unit);
    type_var
}
