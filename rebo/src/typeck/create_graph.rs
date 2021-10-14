use crate::typeck::graph::Graph;
use crate::parser::{Expr, Spanned, ExprFormatString, ExprFormatStringPart, ExprBind, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprAssign, ExprAssignLhs, ExprVariable, ExprFieldAccess, ExprBoolNot, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolAnd, ExprBoolOr, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprBlock, BlockBody, ExprParenthesized, ExprMatch, ExprMatchPattern, ExprWhile, ExprFunctionCall, ExprFunctionDefinition, ExprStructInitialization, ExprImplBlock, ExprMethodCall, ExprType};
use crate::typeck::TypeVar;
use crate::common::{MetaInfo, UserType, Function};
use itertools::Either;
use crate::typeck::types::{StructType, EnumType, EnumTypeVariant, SpecificType, FunctionType, Type};
use diagnostic::{Diagnostics, Span};
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;
use crate::error_codes::ErrorCode;

struct FunctionGenerics {
    stack: Rc<RefCell<Vec<Span>>>,
}
impl FunctionGenerics {
    fn new() -> Self {
        FunctionGenerics {
            stack: Rc::new(RefCell::new(Vec::new())),
        }
    }
    fn push_stack(&self) -> FunctionGenericsStackGuard {
        let prev_stack = std::mem::take(&mut *self.stack.borrow_mut());
        FunctionGenericsStackGuard {
            stack: Rc::clone(&self.stack),
            prev_stack: Some(prev_stack),
        }
    }
    fn insert(&self, generic: Span) {
        assert!(!self.contains(generic));
        self.stack.borrow_mut().push(generic);
    }
    fn contains(&self, span: Span) -> bool {
        self.stack.borrow().iter().find(|&&s| s == span).is_some()
    }
}

struct FunctionGenericsStackGuard {
    stack: Rc<RefCell<Vec<Span>>>,
    prev_stack: Option<Vec<Span>>,
}
impl Drop for FunctionGenericsStackGuard {
    fn drop(&mut self) {
        *self.stack.borrow_mut() = self.prev_stack.take().unwrap();
    }
}

fn convert_expr_type(typ: &ExprType, diagnostics: &Diagnostics, meta_info: &MetaInfo, function_generics: &FunctionGenerics) -> Type {
    match typ {
        ExprType::String(_) => Type::Specific(SpecificType::String),
        ExprType::Int(_) => Type::Specific(SpecificType::Integer),
        ExprType::Float(_) => Type::Specific(SpecificType::Float),
        ExprType::Bool(_) => Type::Specific(SpecificType::Bool),
        ExprType::Unit(_, _) => Type::Specific(SpecificType::Unit),
        ExprType::UserType(ut, _generics) => {
            match meta_info.user_types.get(ut.ident) {
                Some(UserType::Struct(s)) => Type::Specific(SpecificType::Struct(s.name.ident.to_string())),
                Some(UserType::Enum(e)) => Type::Specific(SpecificType::Enum(e.name.ident.to_string())),
                None => {
                    let similar = crate::util::similar_name(ut.ident, meta_info.user_types.keys());
                    let mut diag = diagnostics.error(ErrorCode::UnknownType)
                        .with_error_label(typ.span(), "can't find type with this name");
                    if let Some(similar) = similar {
                        diag = diag.with_info_label(typ.span(), format!("did you mean `{}`", similar));
                    }
                    diag.emit();
                    // hack to make the type resolve regularly even though we don't have any information
                    Type::Top
                }
            }
        },
        ExprType::Generic(g) => {
            if function_generics.contains(g.def_ident.span) {
                Type::Specific(SpecificType::UnUnifyableGeneric(g.def_ident.span))
            } else {
                Type::Specific(SpecificType::UnifyableGeneric(g.def_ident.span))
            }
        },
    }
}

pub fn create_graph<'i>(diagnostics: &'i Diagnostics, meta_info: &mut MetaInfo, exprs: &[&Expr<'_, '_>]) -> Graph<'i> {
    let mut graph = Graph::new(diagnostics);
    let function_generics = FunctionGenerics::new();

    // resolve global types
    for user_type in meta_info.user_types.values() {
        match user_type {
            UserType::Struct(struct_def) => {
                meta_info.struct_types.insert(struct_def.name.ident, StructType {
                    name: struct_def.name.ident.to_string(),
                    fields: struct_def.fields.iter()
                        .map(|(name, _, typ)| (name.ident.to_string(), convert_expr_type(typ, diagnostics, meta_info, &function_generics)))
                        .collect(),
                });
            }
            UserType::Enum(enum_def) => {
                meta_info.enum_types.insert(enum_def.name.ident, EnumType {
                    name: enum_def.name.ident.to_string(),
                    variants: enum_def.variants.iter()
                        .map(|variant| {
                            let name = variant.name.ident.to_string();
                            let variant = match &variant.fields {
                                Some((_open, fields, _close)) => EnumTypeVariant::TupleVariant(
                                    fields.iter()
                                        .map(|typ| convert_expr_type(typ, diagnostics, meta_info, &function_generics))
                                        .collect()
                                ),
                                None => EnumTypeVariant::CLike,
                            };
                            (name, variant)
                        }).collect(),
                });
            }
        }
    }
    for (name, fun) in &meta_info.functions {
        match fun {
            Function::Rebo(..) => {
                let fun = &meta_info.rebo_functions[name];
                let typ = FunctionType {
                    args: fun.args.iter().map(|pattern| {
                        convert_expr_type(&pattern.typ, diagnostics, meta_info, &function_generics)
                    }).collect(),
                    ret: fun.ret_type.as_ref()
                        .map(|(_, typ)| convert_expr_type(typ, diagnostics, meta_info, &function_generics))
                        .unwrap_or(Type::Specific(SpecificType::Unit)),
                };
                meta_info.function_types.insert(name.clone(), typ);
            }
            Function::EnumInitializer(enum_name, variant_name) => {
                let enum_def = match &meta_info.user_types[enum_name.as_str()] {
                    UserType::Enum(enum_def) => enum_def,
                    _ => unreachable!(),
                };
                let variant = enum_def.variants.iter()
                    .find(|variant| variant.name.ident == variant_name)
                    .unwrap();
                let typ = FunctionType {
                    args: Cow::Owned(variant.fields.as_ref().unwrap().1.iter()
                        .map(|typ| convert_expr_type(typ, diagnostics, meta_info, &function_generics))
                        .collect()),
                    ret: Type::Specific(SpecificType::Enum(enum_name.clone())),
                };
                meta_info.function_types.insert(name.clone(), typ);
            }
            Function::Rust(_) => (),
        }
    }

    // resolve local types
    for expr in exprs {
        visit_expr(&mut graph, diagnostics, meta_info, &function_generics, expr);
    }
    graph
}

fn visit_expr(graph: &mut Graph, diagnostics: &Diagnostics, meta_info: &MetaInfo, function_generics: &FunctionGenerics, expr: &Expr<'_, '_>) -> TypeVar {
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
                    ExprFormatStringPart::FmtArg(expr) => { visit_expr(graph, diagnostics, meta_info, function_generics, expr); }
                }
            }
        }
        Expr::Bind(ExprBind { pattern, expr, .. }) => {
            graph.set_exact_type(type_var, SpecificType::Unit);
            let var_type_var = match pattern {
                ExprPattern::Typed(ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. }) => {
                    let var_type_var = TypeVar::new(binding.ident.span);
                    graph.add_type_var(var_type_var);
                    if let Type::Specific(specific) = convert_expr_type(typ, diagnostics, meta_info, function_generics) {
                        graph.set_exact_type(var_type_var, specific);
                    }
                    var_type_var
                }
                ExprPattern::Untyped(ExprPatternUntyped { binding }) => {
                    let var_type_var = TypeVar::new(binding.ident.span);
                    graph.add_type_var(var_type_var);
                    var_type_var
                }
            };
            let expr_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, expr);
            graph.add_eq_constraint(var_type_var, expr_type_var);
        }
        Expr::Assign(ExprAssign { lhs, expr, .. }) => {
            graph.set_exact_type(type_var, SpecificType::Unit);
            let expr_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, expr);
            match lhs {
                ExprAssignLhs::Variable(ExprVariable { binding, .. }) => {
                    let var_type_var = TypeVar::new(binding.ident.span);
                    graph.add_type_var(var_type_var);
                    graph.add_eq_constraint(var_type_var, expr_type_var);
                }
                ExprAssignLhs::FieldAccess(ExprFieldAccess { variable: ExprVariable { binding, .. }, fields, .. }) => {
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
            let expr_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, expr);
            graph.set_exact_type(type_var, SpecificType::Bool);
            graph.add_eq_constraint(type_var, expr_type_var);
        }
        Expr::Add(ExprAdd { a, b, .. })
        | Expr::Sub(ExprSub { a, b, .. })
        | Expr::Mul(ExprMul { a, b, .. })
        | Expr::Div(ExprDiv { a, b, .. }) => {
            let a_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, a);
            let b_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, b);
            graph.add_reduce_constraint(type_var, a_type_var, vec![SpecificType::Integer, SpecificType::Float]);
            graph.add_reduce_constraint(type_var, b_type_var, vec![SpecificType::Integer, SpecificType::Float]);
            graph.add_reduce_constraint(type_var, type_var, vec![SpecificType::Integer, SpecificType::Float]);
            graph.add_eq_constraint(a_type_var, type_var);
            graph.add_eq_constraint(b_type_var, type_var);
        }
        Expr::BoolAnd(ExprBoolAnd { a, b, .. })
        | Expr::BoolOr(ExprBoolOr { a, b, .. }) => {
            let a_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, a);
            let b_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, b);
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
            let a_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, a);
            let b_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, b);
            graph.set_exact_type(type_var, SpecificType::Bool);
            graph.add_eq_constraint(a_type_var, b_type_var);
        }
        Expr::Block(block) => {
            assert_eq!(visit_block(graph, diagnostics, meta_info, function_generics, block), type_var);
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
        Expr::MethodCall(ExprMethodCall { variable, fields, fn_call, .. }) => {
            // add variable type-vars
            let var_type_var = TypeVar::new(variable.span());
            let binding_type_var = TypeVar::new(variable.binding.ident.span);
            graph.add_type_var(var_type_var);
            graph.add_type_var(binding_type_var);
            graph.add_unidirectional_eq_constraint(binding_type_var, var_type_var);

            // add field access type-vars
            let field_access_type_var = if fields.is_empty() {
                binding_type_var
            } else {
                let field_access_type_var = TypeVar::new(Span::new(variable.span().file, variable.span().start, fields.last().unwrap().0.span().end));
                let fields = fields.iter().map(|(ident, _dot)| ident.ident.to_string()).collect();
                graph.add_field_access(binding_type_var, field_access_type_var, fields);
                field_access_type_var
            };

            graph.add_method_call_ret(field_access_type_var, type_var, fn_call.name.ident.to_string());
            for (i, arg_expr) in fn_call.args.iter().enumerate() {
                let arg_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, arg_expr);
                // arg 0 is `self`
                let arg_index = i + 1;
                graph.add_method_call_arg(field_access_type_var, arg_type_var, fn_call.name.ident.to_string(), arg_index);
            }
        }
        Expr::Parenthesized(ExprParenthesized { expr, .. }) => {
            let expr_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, expr);
            graph.add_eq_constraint(expr_type_var, type_var);
        }
        Expr::IfElse(ifelse) => {
            let mut block_type_vars = Vec::new();
            for (condition, body) in ifelse.iter_branches() {
                if let Some(condition) = condition {
                    let cond_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, condition);
                    graph.add_reduce_constraint(type_var, cond_type_var, vec![SpecificType::Bool]);
                }
                block_type_vars.push(visit_block(graph, diagnostics, meta_info, function_generics, body));
            }
            for var in block_type_vars {
                graph.add_eq_constraint(type_var, var);
            }
        }
        Expr::Match(ExprMatch { expr, arms, .. }) => {
            let expr_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, expr);
            for (pat, _arrow, arm_expr) in arms {
                match pat {
                    ExprMatchPattern::Literal(lit) => {
                        let pat_type_var = TypeVar::new(pat.span());
                        graph.add_type_var(pat_type_var);
                        graph.set_exact_type(pat_type_var, SpecificType::from(lit));
                        graph.add_eq_constraint(expr_type_var, pat_type_var);
                    }
                    ExprMatchPattern::Variant(variant) => {
                        let pat_type_var = TypeVar::new(pat.span());
                        graph.add_type_var(pat_type_var);
                        graph.set_exact_type(pat_type_var, SpecificType::Enum(variant.enum_name.ident.to_string()));
                        graph.add_eq_constraint(expr_type_var, pat_type_var);

                        let repeat_top = std::iter::repeat(&Type::Top);
                        let variant_type = meta_info.enum_types.get(variant.enum_name.ident)
                            .iter()
                            .flat_map(|enum_def| &enum_def.variants)
                            .find(|(name, _variant)| name == variant.variant_name.ident)
                            .map(|(_name, variant)| variant);

                        let expected_field_types = match variant_type {
                            Some(EnumTypeVariant::TupleVariant(fields)) => {
                                Either::Left(fields.iter().chain(repeat_top))
                            }
                            Some(EnumTypeVariant::CLike) | None => Either::Right(repeat_top),
                        };

                        let iter = variant.fields.iter()
                            .flat_map(|(_open, fields, _close)| fields)
                            .zip(expected_field_types);
                        for (field_binding, expected_type) in iter {
                            let field_binding_type_var = TypeVar::new(field_binding.ident.span);
                            // always add all field type-vars even for unknown enums / variants
                            graph.add_type_var(field_binding_type_var);
                            if let Type::Specific(specific) = expected_type {
                                graph.add_reduce_constraint(pat_type_var, field_binding_type_var, vec![specific.clone()])
                            }
                        }
                    }
                    ExprMatchPattern::Binding(binding) => {
                        let binding_type_var = TypeVar::new(binding.ident.span);
                        graph.add_type_var(binding_type_var);
                        graph.add_eq_constraint(binding_type_var, expr_type_var);
                    }
                    ExprMatchPattern::Wildcard(_) => (),
                }
                let arm_expr_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, arm_expr);
                graph.add_eq_constraint(type_var, arm_expr_type_var);
            }
        }
        Expr::While(ExprWhile { condition, block, .. }) => {
            let cond_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, condition);
            graph.add_reduce_constraint(type_var, cond_type_var, vec![SpecificType::Bool]);
            let block_type_var = visit_block(graph, diagnostics, meta_info, function_generics, block);
            graph.add_eq_constraint(type_var, block_type_var);
        }
        Expr::FunctionCall(ExprFunctionCall { name, args, .. }) => {
            let fun = match meta_info.function_types.get(name.ident) {
                Some(fun) => fun,
                None => return type_var,
            };
            let passed_args: Vec<_> = args.iter().map(|expr| visit_expr(graph, diagnostics, meta_info, function_generics, expr)).collect();
            let expected_args = if let Some(Type::Varargs) = fun.args.last() {
                Either::Left(fun.args.iter().chain(std::iter::repeat(&Type::Varargs)))
            } else {
                Either::Right(fun.args.iter())
            };
            for (passed_type_var, expected) in passed_args.into_iter().zip(expected_args) {
                match expected {
                    Type::Top => (),
                    Type::Bottom => unreachable!("function argument type is Bottom"),
                    Type::Varargs => (),
                    Type::Specific(specific) => {
                        graph.add_reduce_constraint(type_var, passed_type_var, vec![specific.clone()]);
                    }
                }
            }
            match &fun.ret {
                Type::Varargs => unreachable!("function return type is Varargs"),
                Type::Top | Type::Bottom => (),
                Type::Specific(specific) => graph.set_exact_type(type_var, specific.clone()),
            }
        }
        Expr::FunctionDefinition(function) => {
            visit_function(graph, diagnostics, meta_info, function_generics, function);
        }
        Expr::StructDefinition(_) => graph.set_exact_type(type_var, SpecificType::Unit),
        Expr::StructInitialization(ExprStructInitialization { name, fields, .. }) => {
            graph.set_exact_type(type_var, SpecificType::Struct(name.ident.to_string()));
            let struct_def_span = match meta_info.user_types.get(name.ident) {
                Some(user_type) => user_type.span(),
                None => return type_var,
            };
            let struct_typ = meta_info.struct_types.get(name.ident).unwrap();
            let struct_def_type_var = TypeVar::new(struct_def_span);
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
                let expr_type_var = visit_expr(graph, diagnostics, meta_info, function_generics, expr);
                if let Type::Specific(specific) = expected_type {
                    graph.add_reduce_constraint(struct_def_type_var, expr_type_var, vec![specific.clone()]);
                }
            }
        }
        Expr::EnumDefinition(_) => graph.set_exact_type(type_var, SpecificType::Unit),
        Expr::EnumInitialization(enum_init) => graph.set_exact_type(type_var, SpecificType::Enum(enum_init.enum_name.ident.to_string())),
        Expr::ImplBlock(ExprImplBlock { functions, .. }) => {
            for function in functions {
                visit_function(graph, diagnostics, meta_info, function_generics, function);
            }
            graph.set_exact_type(type_var, SpecificType::Unit);
        }
    }
    type_var
}

fn visit_block(graph: &mut Graph, diagnostics: &Diagnostics, meta_info: &MetaInfo, function_generics: &FunctionGenerics, block: &ExprBlock) -> TypeVar {
    let type_var = TypeVar::new(block.span());
    graph.add_type_var(type_var);
    let ExprBlock { body: BlockBody { exprs, terminated_with_semicolon }, .. } = block;

    let mut last = None;
    for expr in exprs {
        last = Some(visit_expr(graph, diagnostics, meta_info, function_generics, expr));
    }
    match (terminated_with_semicolon, last) {
        (true, _) | (false, None) => graph.set_exact_type(type_var, SpecificType::Unit),
        (false, Some(var)) => graph.add_eq_constraint(type_var, var),
    }
    type_var
}

fn visit_function(graph: &mut Graph, diagnostics: &Diagnostics, meta_info: &MetaInfo, function_generics: &FunctionGenerics, function: &ExprFunctionDefinition) -> TypeVar {
    let _scope_guard = function_generics.push_stack();
    for generic in function.generics.iter().flat_map(|g| &g.generics).flatten() {
        function_generics.insert(generic.def_ident.span);
    }

    let type_var = TypeVar::new(function.span());
    graph.add_type_var(type_var);
    let ExprFunctionDefinition { args, ret_type, body, .. } = function;

    for ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. } in args {
        let arg_type_var = TypeVar::new(binding.ident.span);
        graph.add_type_var(arg_type_var);
        if let Type::Specific(specific) = convert_expr_type(typ, diagnostics, meta_info, function_generics) {
            graph.add_reduce_constraint(type_var, arg_type_var, vec![specific]);
        }
    }

    let body_type_var = visit_block(graph, diagnostics, meta_info, function_generics, body);
    if let Some((_arrow, ret_type)) = ret_type {
        if let Type::Specific(specific) = convert_expr_type(ret_type, diagnostics, meta_info, function_generics) {
            graph.add_reduce_constraint(type_var, body_type_var, vec![specific]);
        }
    }
    graph.set_exact_type(type_var, SpecificType::Unit);
    type_var
}
