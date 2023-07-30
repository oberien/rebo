use crate::typeck::graph::{Graph, Node, PossibleTypes, Constraint};
use crate::parser::{Expr, Spanned, ExprFormatString, ExprFormatStringPart, ExprBind, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprAssign, ExprAssignLhs, ExprVariable, ExprFieldAccess, ExprBoolNot, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprMod, ExprXor, ExprBoolAnd, ExprBoolOr, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprBlock, BlockBody, ExprParenthesized, ExprMatch, ExprMatchPattern, ExprWhile, ExprFunctionCall, ExprFunctionDefinition, ExprStructInitialization, ExprImplBlock, ExprType, ExprGenerics, ExprAccess, FieldOrMethod, ExprFor, ExprStatic, ExprFunctionType, ExprFunctionSignature, ExprNeg, ExprStaticSignature, ExprAddAssign, ExprSubAssign, ExprMulAssign, ExprDivAssign, ExprModAssign, ExprXorAssign, ExprBoolAndAssign, ExprBoolOrAssign, ExprLoop, ExprBreak, ExprContinue, ExprReturn};
use crate::common::{MetaInfo, UserType, Function, RequiredReboFunctionStruct, BlockStack, BlockType};
use itertools::Either;
use crate::typeck::types::{StructType, EnumType, EnumTypeVariant, SpecificType, FunctionType, Type, ResolvableSpecificType};
use diagnostic::{Diagnostics, Span};
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;
use crate::error_codes::ErrorCode;
use std::sync::atomic::{Ordering, AtomicU64};
use rt_format::Format;
use rebo::{Value, FunctionValue};
use crate::{CowVec, EXTERNAL_SPAN};

static CALL_INDEX: AtomicU64 = AtomicU64::new(0);

#[derive(Clone)]
pub(super) struct FunctionGenerics {
    /// ununifyable generics within function definitions
    ///
    /// When visting a function's body, the generics of that function must not
    /// unify with any other type. For example `fn foo<T>(t: T) -> int { t }`
    /// must produce an error because `T` isn't guaranteed to be `int`.
    /// Thus, within a function body, all generics of that function must
    /// be represented by an ununifyable generic type.
    ununifyable: Rc<RefCell<Vec<Span>>>,
    /// generics of a called function
    generic_nodes: Rc<RefCell<Vec<Vec<Node>>>>,
}
impl FunctionGenerics {
    pub(super) fn new() -> Self {
        FunctionGenerics {
            ununifyable: Rc::new(RefCell::new(Vec::new())),
            generic_nodes: Rc::new(RefCell::new(vec![Vec::new()])),
        }
    }
    fn push_ununifyable(&self) -> UnunifyableGuard {
        let prev_stack = std::mem::take(&mut *self.ununifyable.borrow_mut());
        UnunifyableGuard {
            stack: Rc::clone(&self.ununifyable),
            prev_stack: Some(prev_stack),
        }
    }
    fn push_generic_nodes(&self) -> GenericGuard {
        self.generic_nodes.borrow_mut().push(Vec::new());
        GenericGuard {
            stack: Rc::clone(&self.generic_nodes),
        }
    }
    fn insert_ununifyable(&self, generic: Span) {
        assert!(!self.contains_ununifyable(generic));
        self.ununifyable.borrow_mut().push(generic);
    }
    fn contains_ununifyable(&self, span: Span) -> bool {
        self.ununifyable.borrow().iter().any(|&s| s == span)
    }
    pub(super) fn insert_generic(&self, node: Node) {
        assert!(matches!(node, Node::Synthetic(..)));
        if self.contains_generic(node) {
            return;
        }
        self.generic_nodes.borrow_mut().last_mut().unwrap().push(node);
    }
    pub(super) fn contains_generic(&self, node: Node) -> bool {
        self.generic_nodes.borrow().iter().flatten()
            .any(|&n| n.span() == node.span())
    }
    pub(super) fn contains_generic_span(&self, span: Span) -> bool {
        self.generic_nodes.borrow().iter().flatten()
            .any(|&n| n.span() == span)
    }
    pub(super) fn generics(&self) -> impl Iterator<Item = Node> {
        self.generic_nodes.borrow().iter().rev().flatten().copied().collect::<Vec<_>>().into_iter()
    }
    pub(super) fn get_generic(&self, span: Span) -> Option<Node> {
        self.generics().find(|n| n.span() == span)
    }
    pub(super) fn apply_type_reduce(&self, source: Node, from: Node, to: Node, typ: &Type, graph: &mut Graph) {
        match typ {
            Type::Top | Type::Bottom | Type::UntypedVarargs => (),
            Type::TypedVarargs(specific)
            | Type::Specific(specific) => self.apply_specific_type_reduce(source, from, to, specific, graph),
        }
    }
    pub(super) fn apply_specific_type_reduce(&self, source: Node, from: Node, to: Node, typ: &SpecificType, graph: &mut Graph) {
        self.apply_specific_type_reduce_internal(source, from, to, typ, graph);
        // graph.dot();
    }
    fn apply_specific_type_reduce_internal(&self, source: Node, from: Node, to: Node, typ: &SpecificType, graph: &mut Graph) {
        let (type_constructor, generics, name): (fn(_, _) -> _, _, _) =  match typ {
            SpecificType::Unit => { graph.add_reduce_constraint(from, to, vec![ResolvableSpecificType::Unit]); return },
            SpecificType::Bool => { graph.add_reduce_constraint(from, to, vec![ResolvableSpecificType::Bool]); return },
            SpecificType::Integer => { graph.add_reduce_constraint(from, to, vec![ResolvableSpecificType::Integer]); return },
            SpecificType::Float => { graph.add_reduce_constraint(from, to, vec![ResolvableSpecificType::Float]); return },
            SpecificType::String => { graph.add_reduce_constraint(from, to, vec![ResolvableSpecificType::String]); return },
            SpecificType::Function(fn_type) => { graph.add_reduce_constraint(from, to, vec![ResolvableSpecificType::Function(Some((**fn_type).clone()))]); return },
            SpecificType::Struct(name, generics) => (
                |generics, name| ResolvableSpecificType::Struct(name, generics),
                generics,
                name.clone().into_owned(),
            ),
            SpecificType::Enum(name, generics) => (
                |generics, name| ResolvableSpecificType::Enum(name, generics),
                generics,
                name.clone().into_owned()
            ),
            &SpecificType::Generic(span) => if self.contains_ununifyable(span) {
                graph.add_reduce_constraint(from, to, vec![ResolvableSpecificType::UnUnifyableGeneric(span)]);
                return;
            } else if let Some(node) = self.get_generic(span) {
                graph.add_generic_constraint(from, node);
                graph.add_generic_eq_source_constraint(source, to);
                graph.add_generic_eq_source_constraint(source, node);
                graph.add_eq_constraint(to, node);
                return;
            } else {
                eprintln!("tried to convert unknown generic {}", typ);
                graph.xdot();
                unreachable!("tried to convert unknown generic {}", typ);
            }
        };
        let mut resolvable_generics = Vec::new();
        for (span, generic) in generics {
            let synthetic = Node::synthetic(*span);
            graph.add_node(synthetic);
            self.apply_type_reduce(source, synthetic, synthetic, generic, graph);
            graph.add_generic_constraint(from, synthetic);
            resolvable_generics.push(synthetic);
        }
        let resolvable_typ = type_constructor(resolvable_generics, name);
        graph.add_reduce_constraint(from, to, vec![resolvable_typ]);
    }
}

#[must_use]
struct UnunifyableGuard {
    stack: Rc<RefCell<Vec<Span>>>,
    prev_stack: Option<Vec<Span>>,
}
impl Drop for UnunifyableGuard {
    fn drop(&mut self) {
        *self.stack.borrow_mut() = self.prev_stack.take().unwrap();
    }
}
#[must_use]
struct GenericGuard {
    stack: Rc<RefCell<Vec<Vec<Node>>>>,
}
impl Drop for GenericGuard {
    fn drop(&mut self) {
        self.stack.borrow_mut().pop().unwrap();
    }
}

struct Context<'ctx, 'a, 'i> {
    diagnostics: &'ctx Diagnostics<ErrorCode>,
    meta_info: &'ctx MetaInfo<'a, 'i>,
    function_generics: &'ctx FunctionGenerics,
    block_stack: &'ctx BlockStack<'a, 'i, Node>,
}

fn convert_expr_type(typ: &ExprType, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo) -> Type {
    match typ {
        ExprType::String(_) => Type::Specific(SpecificType::String),
        ExprType::Int(_) => Type::Specific(SpecificType::Integer),
        ExprType::Float(_) => Type::Specific(SpecificType::Float),
        ExprType::Bool(_) => Type::Specific(SpecificType::Bool),
        ExprType::Unit(_, _) => Type::Specific(SpecificType::Unit),
        ExprType::UserType(ut, generics) => {
            let (type_constructor, typ_span, expected_generics, name): (fn(_, _) -> _, _, _, _) = match meta_info.user_types.get(ut.ident) {
                Some(UserType::Struct(s)) => (
                    |generics, name| Type::Specific(SpecificType::Struct(Cow::Owned(name), CowVec::Owned(generics))),
                    s.name.span,
                    s.generics.as_ref(),
                    s.name.ident.to_string(),
                ),
                Some(UserType::Enum(e)) => (
                    |generics, name| Type::Specific(SpecificType::Enum(Cow::Owned(name), CowVec::Owned(generics))),
                    e.name.span,
                    e.generics.as_ref(),
                    e.name.ident.to_string(),
                ),
                None => {
                    let similar = crate::util::similar_name(ut.ident, meta_info.user_types.keys());
                    let mut diag = diagnostics.error(ErrorCode::UnknownType)
                        .with_error_label(typ.span(), "can't find type with this name");
                    if let Some(similar) = similar {
                        diag = diag.with_info_label(typ.span(), format!("did you mean `{}`", similar));
                    }
                    diag.emit();
                    // hack to make the type resolve regularly even though we don't have any information
                    return Type::Top
                }
            };
            if generics.is_none() {
                return type_constructor(Vec::new(), name);
            }
            let (open, generics, close) = generics.as_ref().unwrap();
            let generic_span = Span::new(open.span.file, open.span.start, close.span.end);

            let expected_generics = expected_generics.and_then(|g| g.generics.as_ref());
            let expected_generic_span = expected_generics.and_then(|g| g.span()).unwrap_or(typ_span);
            let expected_generic_iter = match expected_generics {
                Some(generics) => Either::Left(generics.iter().map(Some).chain(std::iter::repeat(None))),
                None => Either::Right(std::iter::repeat(None)),
            };
            let generics_iter = generics.iter().map(Some).chain(std::iter::repeat_with(|| None));
            let mut iter = expected_generic_iter.zip(generics_iter);
            let mut generics = Vec::new();
            loop {
                match iter.next() {
                    Some((Some(expected), Some(generic))) => {
                        let typ = convert_expr_type(generic, diagnostics, meta_info);
                        generics.push((expected.span(), typ));
                    },
                    Some((Some(expected), None)) => {
                        diagnostics.error(ErrorCode::MissingGeneric)
                            .with_error_label(generic_span, format!("missing generic `{}`", expected.def_ident.ident))
                            .with_info_label(expected.def_ident.span, "defined here")
                            .emit();
                        generics.push((expected.span(), Type::Top));
                    },
                    Some((None, Some(generic))) => diagnostics.error(ErrorCode::TooManyGenerics)
                        .with_error_label(generic.span(), format!("too many generics, unknown generic `{}`", generic))
                        .with_info_label(expected_generic_span, "expected generics defined here")
                        .emit(),
                    Some((None, None)) => break,
                    None => unreachable!(),
                }
            }
            type_constructor(generics, name)
        },
        ExprType::Function(f) => {
            let ExprFunctionType { generics, args, ret_type, .. } = &**f;
            let generics = generics.iter().flat_map(|g| &g.generics).flatten()
                .map(|g| g.def_ident.span)
                .collect();
            let args = args.iter().map(|arg| convert_expr_type(arg, diagnostics, meta_info)).collect();
            let ret = ret_type.as_ref().map(|(_, ret)| convert_expr_type(ret, diagnostics, meta_info))
                .unwrap_or(Type::Specific(SpecificType::Unit));
            Type::Specific(SpecificType::Function(Box::new(FunctionType {
                is_method: false,
                generics,
                args,
                ret,
            })))
        }
        ExprType::Generic(g) => {
            Type::Specific(SpecificType::Generic(g.def_ident.span))
        },
        ExprType::Never(_) => Type::Bottom,
    }
}

impl<'i> Graph<'i> {
    /// Add types defined in the rebo code as resolved struct and enum types
    fn add_user_types(diagnostics: &'i Diagnostics<ErrorCode>, meta_info: &mut MetaInfo) {
        for user_type in meta_info.user_types.values() {
            match user_type {
                UserType::Struct(struct_def) => {
                    meta_info.struct_types.insert(struct_def.name.ident, StructType {
                        generics: struct_def.generics.iter().flat_map(|g| &g.generics).flatten()
                            .map(|g| g.def_ident.span)
                            .collect(),
                        name: struct_def.name.ident.to_string(),
                        fields: struct_def.fields.iter()
                            .map(|(name, _, typ)| (name.ident.to_string(), convert_expr_type(typ, diagnostics, meta_info)))
                            .collect(),
                    });
                }
                UserType::Enum(enum_def) => {
                    meta_info.enum_types.insert(enum_def.name.ident, EnumType {
                        generics: enum_def.generics.iter().flat_map(|g| &g.generics).flatten()
                            .map(|g| g.def_ident.span)
                            .collect(),
                        name: enum_def.name.ident.to_string(),
                        variants: enum_def.variants.iter()
                            .map(|variant| {
                                let name = variant.name.ident.to_string();
                                let variant = match &variant.fields {
                                    Some((_open, fields, _close)) => EnumTypeVariant::TupleVariant(
                                        fields.iter()
                                            .map(|typ| convert_expr_type(typ, diagnostics, meta_info))
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
    }
    /// Verify that all external types were defined correctly and match our internal types
    fn verify_external_types(diagnostics: &'i Diagnostics<ErrorCode>, meta_info: &mut MetaInfo) {
        for (name, typ) in &meta_info.external_types {
            let (internal_generics, external_generics) = match typ {
                SpecificType::Struct(_name, generics) => (
                    &meta_info.struct_types[name.as_str()].generics,
                    generics,
                ),
                SpecificType::Enum(_name, generics) => (
                    &meta_info.enum_types[name.as_str()].generics,
                    generics,
                ),
                _ => panic!("external type `{}` is not a Struct or Enum but a `{:?}`", name, typ),
            };
            let matches = external_generics.iter().map(|(span, _)| span).zip(internal_generics.as_ref())
                .find(|(ext, int)| ext != int);
            assert!(matches.is_none(), "Generic `{}::{}` of ExternalType-definition (`{:?}`) doesn't equal parsed generic (`{:?}`)",
                    name,
                    diagnostics.resolve_span(*matches.unwrap().1),
                    matches.unwrap().0,
                    matches.unwrap().1,
            );
        }
    }
    fn get_function_type(meta_info: &MetaInfo, diagnostics: &Diagnostics<ErrorCode>, sig: &ExprFunctionSignature, external: bool) -> FunctionType {
        FunctionType {
            is_method: if external {
                sig.args.iter().next().map(|arg| arg.pattern.binding.ident.ident == "this").unwrap_or(false)
            } else {
                sig.self_arg.is_some()
            },
            generics: sig.generics.iter().flat_map(|g| &g.generics).flatten()
                .map(|g| g.def_ident.span)
                .collect(),
            args: sig.args.iter().map(|pattern| {
                convert_expr_type(&pattern.typ, diagnostics, meta_info)
            }).chain(sig.varargs.iter().map(|(typ, _)| {
                if let Some(vararg_typ) = typ {
                    let typ = convert_expr_type(vararg_typ, diagnostics, meta_info);
                    match typ {
                        Type::Specific(specific) => Type::TypedVarargs(specific),
                        _ => {
                            diagnostics.error(ErrorCode::InvalidVarargs)
                                .with_error_label(vararg_typ.span(), "can't convert this type")
                                .emit();
                            Type::UntypedVarargs
                        }
                    }
                } else {
                    Type::UntypedVarargs
                }
            })).collect(),
            ret: sig.ret_type.as_ref()
                .map(|(_, typ)| convert_expr_type(typ, diagnostics, meta_info))
                .unwrap_or(Type::Specific(SpecificType::Unit)),
        }
    }
    fn add_function_types(diagnostics: &'i Diagnostics<ErrorCode>, meta_info: &mut MetaInfo) {
        for (name, fun) in &meta_info.functions {
            match fun {
                Function::Rebo(..) => {
                    let fun = &meta_info.rebo_functions[name];
                    let typ = Self::get_function_type(meta_info, diagnostics, &fun.sig, false);
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
                        is_method: false,
                        generics: get_user_type_generics(meta_info, enum_name).into_iter()
                            .map(|(span, _typ)| span)
                            .collect(),
                        args: Cow::Owned(variant.fields.as_ref().unwrap().1.iter()
                            .map(|typ| convert_expr_type(typ, diagnostics, meta_info))
                            .collect()),
                        ret: Type::Specific(SpecificType::Enum(Cow::Owned(enum_name.clone()), CowVec::Owned(get_user_type_generics(meta_info, enum_name)))),
                    };
                    meta_info.function_types.insert(name.clone(), typ);
                }
                Function::Rust(_) => {
                    let sig = &meta_info.external_function_signatures[name.as_ref()];
                    let typ = Self::get_function_type(meta_info, diagnostics, sig, true);
                    meta_info.function_types.insert(name.clone(), typ);
                }
            }
        }
    }
    /// Check that required rebo functions are there and have the correct types
    fn check_required_rebo_functions(diagnostics: &'i Diagnostics<ErrorCode>, meta_info: &mut MetaInfo) {
        for rrf in &meta_info.required_rebo_functions {
            let RequiredReboFunctionStruct { name, is_method, generics, args, ret } = rrf;
            let metfun = if *is_method { "method" } else { "function" };
            let typ = match meta_info.function_types.get(*name) {
                Some(typ) => typ,
                None => {
                    diagnostics.error(ErrorCode::RequiredReboFunctionUnavailable)
                        .with_error_label(EXTERNAL_SPAN, format!("couldn't find {} {}", metfun, name))
                        .emit();
                    continue;
                }
            };
            let fun = &meta_info.rebo_functions[*name];
            if typ.is_method != *is_method {
                diagnostics.error(ErrorCode::RequiredReboFunctionDiffers)
                    .with_error_label(fun.sig.span(), format!("expected this to be a {}", metfun))
                    .emit();
            }
            if typ.generics.len() != generics.len() {
                diagnostics.error(ErrorCode::RequiredReboFunctionDiffers)
                    .with_error_label(fun.sig.span(), format!("expected {} generics, got {}", generics.len(), typ.generics.len()))
                    .emit();
            }
            for ((span, generic), expected) in typ.generics.iter().map(|&span| (span, diagnostics.resolve_span(span))).zip(*generics) {
                if generic != *expected {
                    diagnostics.error(ErrorCode::RequiredReboFunctionDiffers)
                        .with_error_label(span, format!("expected generic named `{}`", expected))
                        .emit();
                }
            }
            if typ.args.len() != args.len() {
                diagnostics.error(ErrorCode::RequiredReboFunctionDiffers)
                    .with_error_label(Span::new(fun.sig.open.span.file, fun.sig.open.span.start, fun.sig.close.span.end), format!("expected {} args, found {}", args.len(), typ.args.len()))
                    .emit();
            }
            let arg_spans: Vec<_> = fun.sig.self_arg.iter().map(|a| a.span()).chain(fun.sig.args.iter().map(|a| a.span())).collect();
            for (i, (arg, expected)) in typ.args.iter().zip(*args).enumerate() {
                if arg != expected {
                    diagnostics.error(ErrorCode::RequiredReboFunctionDiffers)
                        .with_error_label(arg_spans[i], format!("expected type `{}`, found type `{}`", expected, arg))
                        .emit();
                }
            }
            if *ret != typ.ret {
                diagnostics.error(ErrorCode::RequiredReboFunctionDiffers)
                    .with_error_label(Span::new(fun.sig.close.span.file, fun.sig.close.span.end, fun.body.span().start), format!("expected return type `{}`, found `{}`", ret, typ.ret))
                    .emit();
            }
        }
    }
    /// Resolve a value to its resolved type after all struct and enum types are available
    fn resolve_value_type(&mut self, meta_info: &MetaInfo, val: &Value) -> ResolvableSpecificType {
        match val {
            Value::Unit => ResolvableSpecificType::Unit,
            Value::Integer(_) => ResolvableSpecificType::Integer,
            Value::Float(_) => ResolvableSpecificType::Float,
            Value::Bool(_) => ResolvableSpecificType::Bool,
            Value::String(_) => ResolvableSpecificType::String,
            Value::Struct(s) => {
                let name = s.s.lock().borrow().name.clone();
                if !meta_info.struct_types[&*name].generics.is_empty() {
                    unimplemented!("passing generic structs as external values is unsupported");
                }
                ResolvableSpecificType::Struct(name, vec![])
            },
            Value::Enum(e) => {
                let name = e.e.lock().borrow().name.clone();
                if !meta_info.enum_types[&*name].generics.is_empty() {
                    unimplemented!("passing generic enums as external values is unsupported");
                }
                ResolvableSpecificType::Enum(name, vec![])
            }
            Value::Function(FunctionValue::Named(name)) => ResolvableSpecificType::Function(Some(meta_info.function_types[name.as_str()].to_owned())),
            Value::Function(FunctionValue::Anonymous(..)) => unimplemented!("passing an anonymous function as external value is unsupported"),
            Value::List(l) => {
                let name = "List".to_string();
                let t_span = meta_info.struct_types["List"].generics.first().unwrap();
                let generics = match l.list.lock().borrow().first() {
                    Some(inner) => {
                        let typ = self.resolve_value_type(meta_info, inner);
                        let node = Node::synthetic(*t_span);
                        self.add_node(node);
                        self.add_reduce_constraint(node, node, vec![typ]);
                        vec![node]
                    },
                    None => unimplemented!("passing an empty list as external value is unsupported"),
                };
                ResolvableSpecificType::Struct(name, generics)
            },
            Value::Map(m) => {
                let name = "Map".to_string();
                let k_span = meta_info.struct_types["Map"].generics.get(0).unwrap();
                let v_span = meta_info.struct_types["Map"].generics.get(1).unwrap();
                let generics = match m.map.lock().borrow().first_key_value() {
                    Some((k, v)) => {
                        let k_typ = self.resolve_value_type(meta_info, k);
                        let v_typ = self.resolve_value_type(meta_info, v);
                        let k_node = Node::synthetic(*k_span);
                        let v_node = Node::synthetic(*v_span);
                        self.add_node(k_node);
                        self.add_node(v_node);
                        self.add_reduce_constraint(k_node, k_node, vec![k_typ]);
                        self.add_reduce_constraint(v_node, v_node, vec![v_typ]);
                        vec![k_node, v_node]
                    },
                    None => unimplemented!("passing an empty map as external value is unsupported"),
                };
                ResolvableSpecificType::Struct(name, generics)
            },
            Value::Set(s) => {
                let name = "Set".to_string();
                let t_span = meta_info.struct_types["Set"].generics.first().unwrap();
                let generics = match s.set.lock().borrow().first() {
                    Some(inner) => {
                        let typ = self.resolve_value_type(meta_info, inner);
                        let node = Node::synthetic(*t_span);
                        self.add_node(node);
                        self.add_reduce_constraint(node, node, vec![typ]);
                        vec![node]
                    },
                    None => unimplemented!("passing an empty set as external value is unsupported"),
                };
                ResolvableSpecificType::Struct(name, generics)
            },
        }
    }
    pub fn create(diagnostics: &'i Diagnostics<ErrorCode>, meta_info: &mut MetaInfo<'_, 'i>, exprs: &[&Expr<'_, 'i>]) -> Graph<'i> {
        Self::add_user_types(diagnostics, meta_info);
        Self::verify_external_types(diagnostics, meta_info);
        Self::add_function_types(diagnostics, meta_info);
        Self::check_required_rebo_functions(diagnostics, meta_info);

        let mut graph = Graph::new(diagnostics);

        // add global function bindings
        for (binding, name) in &meta_info.function_bindings {
            let node = Node::type_var(binding.ident.span);
            graph.add_node(node);
            let typ = match meta_info.function_types.get(name.as_str()) {
                Some(typ) => typ,
                None => continue,
            };
            graph.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Function(Some(typ.clone()))]);
        }
        // add user-provided value types
        for (binding, val) in &meta_info.external_values {
            let typ = graph.resolve_value_type(meta_info, val);
            let node = Node::type_var(binding.ident.span());
            graph.add_node(node);
            graph.add_reduce_constraint(node, node, vec![typ]);
        }

        // resolve local types
        let ctx = Context {
            diagnostics,
            meta_info,
            function_generics: &FunctionGenerics::new(),
            block_stack: &BlockStack::new(),
        };
        for expr in exprs {
            graph.visit_expr(&ctx, expr);
        }
        graph
    }

    fn visit_expr<'a>(&mut self, ctx: &Context<'_, 'a, 'i>, expr: &'a Expr<'a, 'i>) -> Node {
        let node = Node::type_var(expr.span());
        self.add_node(node);
        match expr {
            Expr::Literal(lit) => self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::from(lit)]),
            Expr::FormatString(ExprFormatString { parts, .. }) => {
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::String]);
                for part in parts {
                    match part {
                        ExprFormatStringPart::Str(_)
                        | ExprFormatStringPart::Escaped(_) => (),
                        ExprFormatStringPart::FmtArg(expr, spec) => {
                            let node = self.visit_expr(ctx, expr);
                            if let Some((_colon, spec, spec_span)) = spec {
                                let fmt_node = Node::type_var(*spec_span);
                                self.add_node(fmt_node);
                                match spec.format {
                                    Format::Display => (),
                                    Format::Debug => (),
                                    Format::Octal => {
                                        self.add_reduce_constraint(fmt_node, node, vec![
                                            ResolvableSpecificType::Unit,
                                            ResolvableSpecificType::Integer,
                                            ResolvableSpecificType::Struct("struct".to_string(), vec![]),
                                            ResolvableSpecificType::Enum("enum".to_string(), vec![]),
                                            ResolvableSpecificType::Function(None),
                                        ]);
                                    }
                                    Format::LowerHex => {
                                        self.add_reduce_constraint(fmt_node, node, vec![
                                            ResolvableSpecificType::Unit,
                                            ResolvableSpecificType::Integer,
                                            ResolvableSpecificType::Struct("struct".to_string(), vec![]),
                                            ResolvableSpecificType::Enum("enum".to_string(), vec![]),
                                            ResolvableSpecificType::Function(None),
                                        ]);
                                    }
                                    Format::UpperHex => {
                                        self.add_reduce_constraint(fmt_node, node, vec![
                                            ResolvableSpecificType::Unit,
                                            ResolvableSpecificType::Integer,
                                            ResolvableSpecificType::Struct("struct".to_string(), vec![]),
                                            ResolvableSpecificType::Enum("enum".to_string(), vec![]),
                                            ResolvableSpecificType::Function(None),
                                        ]);
                                    }
                                    Format::Binary => {
                                        self.add_reduce_constraint(fmt_node, node, vec![
                                            ResolvableSpecificType::Unit,
                                            ResolvableSpecificType::Integer,
                                            ResolvableSpecificType::Struct("struct".to_string(), vec![]),
                                            ResolvableSpecificType::Enum("enum".to_string(), vec![]),
                                            ResolvableSpecificType::Function(None),
                                        ]);
                                    }
                                    Format::LowerExp => {
                                        self.add_reduce_constraint(fmt_node, node, vec![
                                            ResolvableSpecificType::Unit,
                                            ResolvableSpecificType::Integer,
                                            ResolvableSpecificType::Float,
                                            ResolvableSpecificType::Struct("struct".to_string(), vec![]),
                                            ResolvableSpecificType::Enum("enum".to_string(), vec![]),
                                            ResolvableSpecificType::Function(None),
                                        ]);
                                    }
                                    Format::UpperExp => {
                                        self.add_reduce_constraint(fmt_node, node, vec![
                                            ResolvableSpecificType::Unit,
                                            ResolvableSpecificType::Integer,
                                            ResolvableSpecificType::Float,
                                            ResolvableSpecificType::Struct("struct".to_string(), vec![]),
                                            ResolvableSpecificType::Enum("enum".to_string(), vec![]),
                                            ResolvableSpecificType::Function(None),
                                        ]);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Expr::Bind(ExprBind { pattern, expr, .. })
            | Expr::Static(ExprStatic { sig: ExprStaticSignature { pattern, .. }, expr, .. }) => {
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
                let var_node = match pattern {
                    ExprPattern::Typed(ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. }) => {
                        let var_node = Node::type_var(binding.ident.span);
                        self.add_node(var_node);
                        let typ = convert_expr_type(typ, ctx.diagnostics, ctx.meta_info);
                        ctx.function_generics.apply_type_reduce(var_node, var_node, var_node, &typ, self);
                        var_node
                    }
                    ExprPattern::Untyped(ExprPatternUntyped { binding }) => {
                        let var_node = Node::type_var(binding.ident.span);
                        self.add_node(var_node);
                        var_node
                    }
                };
                let expr_node = self.visit_expr(ctx, expr);
                self.add_eq_constraint(var_node, expr_node);
            }
            Expr::Assign(ExprAssign { lhs, expr, .. }) => {
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
                let expr_node = self.visit_expr(ctx, expr);
                let lhs_node = self.visit_expr_assign_lhs(lhs);
                self.add_eq_constraint(lhs_node, expr_node);
            }
            Expr::BoolNot(ExprBoolNot { expr, .. }) => {
                let expr_node = self.visit_expr(ctx, expr);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Bool]);
                self.add_eq_constraint(node, expr_node);
            }
            Expr::Neg(ExprNeg { expr, .. }) => {
                let expr_node = self.visit_expr(ctx, expr);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_eq_constraint(node, expr_node);
            }
            Expr::Add(ExprAdd { a, b, .. })
            | Expr::Sub(ExprSub { a, b, .. })
            | Expr::Mul(ExprMul { a, b, .. })
            | Expr::Div(ExprDiv { a, b, .. }) => {
                let a_node = self.visit_expr(ctx, a);
                let b_node = self.visit_expr(ctx, b);
                self.add_reduce_constraint(node, a_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_reduce_constraint(node, b_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_eq_constraint(a_node, node);
                self.add_eq_constraint(b_node, node);
            }
            Expr::Mod(ExprMod { a, b, .. }) => {
                let a_node = self.visit_expr(ctx, a);
                let b_node = self.visit_expr(ctx, b);
                self.add_reduce_constraint(node, a_node, vec![ResolvableSpecificType::Integer]);
                self.add_reduce_constraint(node, b_node, vec![ResolvableSpecificType::Integer]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Integer]);
                self.add_eq_constraint(a_node, node);
                self.add_eq_constraint(b_node, node);
            }
            Expr::Xor(ExprXor { a, b, .. }) => {
                let a_node = self.visit_expr(ctx, a);
                let b_node = self.visit_expr(ctx, b);
                self.add_reduce_constraint(node, a_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Bool]);
                self.add_reduce_constraint(node, b_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Bool]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Bool]);
                self.add_eq_constraint(a_node, node);
                self.add_eq_constraint(b_node, node);
            }
            Expr::AddAssign(ExprAddAssign { lhs, expr, .. })
            | Expr::SubAssign(ExprSubAssign { lhs, expr, .. })
            | Expr::MulAssign(ExprMulAssign { lhs, expr, .. })
            | Expr::DivAssign(ExprDivAssign { lhs, expr, .. }) => {
                let lhs_node = self.visit_expr_assign_lhs(lhs);
                let expr_node = self.visit_expr(ctx, expr);
                self.add_reduce_constraint(node, lhs_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_reduce_constraint(node, expr_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
                self.add_eq_constraint(lhs_node, expr_node);
            }
            Expr::ModAssign(ExprModAssign { lhs, expr, .. }) => {
                let lhs_node = self.visit_expr_assign_lhs(lhs);
                let expr_node = self.visit_expr(ctx, expr);
                self.add_reduce_constraint(node, lhs_node, vec![ResolvableSpecificType::Integer]);
                self.add_reduce_constraint(node, expr_node, vec![ResolvableSpecificType::Integer]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
                self.add_eq_constraint(lhs_node, expr_node);
            }
            Expr::XorAssign(ExprXorAssign { lhs, expr, .. }) => {
                let lhs_node = self.visit_expr_assign_lhs(lhs);
                let expr_node = self.visit_expr(ctx, expr);
                self.add_reduce_constraint(node, lhs_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Bool]);
                self.add_reduce_constraint(node, expr_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Bool]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
                self.add_eq_constraint(lhs_node, expr_node);
            }
            Expr::BoolAnd(ExprBoolAnd { a, b, .. })
            | Expr::BoolOr(ExprBoolOr { a, b, .. }) => {
                let a_node = self.visit_expr(ctx, a);
                let b_node = self.visit_expr(ctx, b);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Bool]);
                self.add_eq_constraint(node, a_node);
                self.add_eq_constraint(node, b_node);
            }
            Expr::BoolAndAssign(ExprBoolAndAssign { lhs, expr, .. })
            | Expr::BoolOrAssign(ExprBoolOrAssign { lhs, expr, .. }) => {
                let lhs_node = self.visit_expr_assign_lhs(lhs);
                let expr_node = self.visit_expr(ctx, expr);
                self.add_reduce_constraint(node, lhs_node, vec![ResolvableSpecificType::Bool]);
                self.add_reduce_constraint(node, expr_node, vec![ResolvableSpecificType::Bool]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
                self.add_eq_constraint(lhs_node, expr_node);
            }
            Expr::LessThan(ExprLessThan { a, b, .. })
            | Expr::LessEquals(ExprLessEquals { a, b, .. })
            | Expr::Equals(ExprEquals { a, b, .. })
            | Expr::NotEquals(ExprNotEquals { a, b, .. })
            | Expr::GreaterEquals(ExprGreaterEquals { a, b, .. })
            | Expr::GreaterThan(ExprGreaterThan { a, b, .. }) => {
                let a_node = self.visit_expr(ctx, a);
                let b_node = self.visit_expr(ctx, b);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Bool]);
                self.add_eq_constraint(a_node, b_node);
            }
            Expr::Block(block) => {
                assert_eq!(self.visit_block(ctx, block), node);
            }
            Expr::Variable(var @ ExprVariable { binding, .. }) => {
                let binding_node = Node::type_var(binding.ident.span);
                assert_eq!(node, Node::type_var(var.span()));
                let var_node = node;
                self.add_node(binding_node);
                self.add_node(var_node);
                self.add_eq_constraint(var_node, binding_node);
                return var_node;
            }
            Expr::Access(ExprAccess { variable, accesses, .. }) => {
                let binding_node = Node::type_var(variable.binding.ident.span);
                let var_node = Node::type_var(variable.span());
                self.add_node(binding_node);
                self.add_node(var_node);
                self.add_eq_constraint(binding_node, var_node);
                let mut access_node = var_node;
                for access in accesses {
                    access_node = match access {
                        FieldOrMethod::Field(field) => {
                            let field_node = Node::type_var(field.span);
                            self.add_node(field_node);
                            self.add_field_access(access_node, field_node, field.ident.to_string());
                            field_node
                        }
                        FieldOrMethod::Method(fn_call) => {
                            let method_name_node = Node::type_var(fn_call.name.span);
                            self.add_node(method_name_node);
                            let method_call_node = Node::type_var(fn_call.span());
                            self.add_node(method_call_node);

                            let idx = CALL_INDEX.fetch_add(1, Ordering::SeqCst);
                            self.add_method(access_node, method_name_node);
                            self.add_method_call_ret(method_name_node, method_call_node, idx);
                            // self-argument
                            self.add_method_call_arg(method_name_node, access_node, idx, 0);
                            for (i, arg_expr) in fn_call.args.iter().enumerate() {
                                let arg_node = self.visit_expr(ctx, arg_expr);
                                // arg 0 is `self`
                                let arg_index = i + 1;
                                self.add_method_call_arg(method_name_node, arg_node, idx, arg_index);
                            }
                            method_call_node
                        }
                    };
                }
                self.add_eq_constraint(node, access_node);
            }
            Expr::Parenthesized(ExprParenthesized { expr, .. }) => {
                let expr_node = self.visit_expr(ctx, expr);
                self.add_eq_constraint(expr_node, node);
            }
            Expr::IfElse(ifelse) => {
                let mut block_nodes = Vec::new();
                for (condition, body) in ifelse.iter_branches() {
                    if let Some(condition) = condition {
                        let cond_node = self.visit_expr(ctx, condition);
                        self.add_reduce_constraint(node, cond_node, vec![ResolvableSpecificType::Bool]);
                    }
                    block_nodes.push(self.visit_block(ctx, body));
                }
                for block_node in block_nodes {
                    self.add_eq_constraint(node, block_node);
                }
            }
            Expr::Match(ExprMatch { expr, arms, .. }) => {
                let expr_node = self.visit_expr(ctx, expr);
                for (pat, _arrow, arm_expr) in arms {
                    match pat {
                        ExprMatchPattern::Literal(lit) => {
                            let pat_node = Node::type_var(pat.span());
                            self.add_node(pat_node);
                            self.add_reduce_constraint(pat_node, pat_node, vec![ResolvableSpecificType::from(lit)]);
                            self.add_eq_constraint(expr_node, pat_node);
                        }
                        ExprMatchPattern::Variant(variant) => {
                            let pat_node = Node::type_var(pat.span());
                            self.add_node(pat_node);
                            let generics = get_user_type_generics(ctx.meta_info, variant.enum_name.ident);
                            let _scope = ctx.function_generics.push_generic_nodes();
                            for (gen, _) in &generics {
                                let synthetic = Node::synthetic(*gen);
                                self.add_node(synthetic);
                                self.add_generic_constraint(node, synthetic);
                                ctx.function_generics.insert_generic(synthetic);
                            }
                            let typ = SpecificType::Enum(Cow::Owned(variant.enum_name.ident.to_string()), CowVec::Owned(generics));
                            ctx.function_generics.apply_specific_type_reduce(pat_node, pat_node, pat_node, &typ, self);
                            self.add_eq_constraint(expr_node, pat_node);

                            let repeat_top = std::iter::repeat(&Type::Top);
                            let variant_type = ctx.meta_info.enum_types.get(variant.enum_name.ident)
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
                                let field_binding_node = Node::type_var(field_binding.ident.span);
                                // always add all field type-vars even for unknown enums / variants
                                self.add_node(field_binding_node);
                                ctx.function_generics.apply_type_reduce(pat_node, pat_node, field_binding_node, expected_type, self);
                            }
                        }
                        ExprMatchPattern::Binding(binding) => {
                            let binding_node = Node::type_var(binding.ident.span);
                            self.add_node(binding_node);
                            self.add_eq_constraint(binding_node, expr_node);
                        }
                        ExprMatchPattern::Wildcard(_) => (),
                    }
                    let arm_expr_node = self.visit_expr(ctx, arm_expr);
                    self.add_eq_constraint(node, arm_expr_node);
                }
            }
            Expr::Loop(ExprLoop { label, block, .. }) => {
                let block_node = Node::type_var(block.span());
                let block_guard = ctx.block_stack.push_block(BlockType::Loop(label.as_ref().map(|l| &l.label)), node);
                let block_node2 = self.visit_block(ctx, block);
                assert_eq!(block_node, block_node2);
                drop(block_guard);
                self.add_reduce_constraint(node, block_node, vec![ResolvableSpecificType::Unit]);
            }
            Expr::While(ExprWhile { label, condition, block, .. }) => {
                let cond_node = self.visit_expr(ctx, condition);
                self.add_reduce_constraint(node, cond_node, vec![ResolvableSpecificType::Bool]);
                let block_node = Node::type_var(block.span());
                let block_guard = ctx.block_stack.push_block(BlockType::While(label.as_ref().map(|l| &l.label)), node);
                let block_node2 = self.visit_block(ctx, block);
                assert_eq!(block_node, block_node2);
                drop(block_guard);
                self.add_reduce_constraint(node, block_node, vec![ResolvableSpecificType::Unit]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
            }
            Expr::For(ExprFor { label, binding, expr, block, .. }) => {
                let binding_node = Node::type_var(binding.ident.span);
                self.add_node(binding_node);
                let expr_node = self.visit_expr(ctx, expr);
                let block_node = Node::type_var(block.span());
                let block_guard = ctx.block_stack.push_block(BlockType::For(label.as_ref().map(|l| &l.label)), node);
                let block_node2 = self.visit_block(ctx, block);
                assert_eq!(block_node, block_node2);
                drop(block_guard);
                let list_t = ctx.meta_info.user_types["List"].generics().unwrap().generics.as_ref().unwrap().iter().next().unwrap().def_ident.span;

                let synthetic = Node::synthetic(list_t);
                self.add_node(synthetic);
                self.add_generic_constraint(node, synthetic);
                self.add_reduce_constraint(node, expr_node, vec![ResolvableSpecificType::Struct("List".to_string(), vec![synthetic])]);
                self.add_eq_constraint(binding_node, synthetic);
                self.add_reduce_constraint(node, block_node, vec![ResolvableSpecificType::Unit]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
            }
            Expr::Break(ExprBreak { label, expr, .. }) => {
                let loop_like = ctx.block_stack.get_loop_like(label.as_ref());
                let expr_node = expr.map(|expr| self.visit_expr(ctx, expr));
                match (loop_like, expr_node) {
                    (Some((_, loop_node)), Some(expr_node)) => self.add_eq_constraint(expr_node, loop_node),
                    (Some((_, loop_node)), None) => self.add_reduce_constraint(node, loop_node, vec![ResolvableSpecificType::Unit]),
                    (_, _) => (),
                }
                // break returns bottom, which is top during type resolution, which is the default
            }
            Expr::Continue(ExprContinue { .. }) => {
                // continue returns bottom, which is top during type resolution, which is the default
            }
            Expr::Return(ExprReturn { expr, .. }) => {
                let function = ctx.block_stack.get_function();
                let expr_node = expr.map(|expr| self.visit_expr(ctx, expr));
                match (function, expr_node) {
                    (Some((_, fn_node)), Some(expr_node)) => self.add_eq_constraint(expr_node, fn_node),
                    (Some((_, fn_node)), None) => self.add_reduce_constraint(node, fn_node, vec![ResolvableSpecificType::Unit]),
                    (_, _) => (),
                }
                // return returns bottom, which is top during type resolution, which is the default
            }
            Expr::FunctionCall(ExprFunctionCall { name, args, .. }) => {
                let binding_node = Node::type_var(name.binding.ident.span);
                let var_node = Node::type_var(name.span());
                self.add_node(binding_node);
                self.add_node(var_node);
                self.add_eq_constraint(binding_node, var_node);
                if let Some(name) = ctx.meta_info.function_bindings.get(&name.binding) {
                    if let Some(fun) = ctx.meta_info.function_types.get(name.as_str()) {
                        self.add_reduce_constraint(binding_node, binding_node, vec![ResolvableSpecificType::Function(Some(fun.clone()))]);
                    }
                }

                let idx = CALL_INDEX.fetch_add(1, Ordering::SeqCst);
                self.add_function_call_ret(var_node, node, idx);
                for (i, arg_expr) in args.iter().enumerate() {
                    let arg_node = self.visit_expr(ctx, arg_expr);
                    self.add_function_call_arg(var_node, arg_node, idx, i);
                }
            }
            Expr::FunctionDefinition(function) => {
                self.visit_function(ctx, function);
            }
            Expr::StructDefinition(_) => self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]),
            Expr::StructInitialization(ExprStructInitialization { name, fields, .. }) => {
                let generics = get_user_type_generics(ctx.meta_info, name.ident);
                let typ = SpecificType::Struct(Cow::Owned(name.ident.to_string()), CowVec::Owned(generics.clone()));
                let struct_def_span = match ctx.meta_info.user_types.get(name.ident) {
                    Some(user_type) => user_type.span(),
                    None => return node,
                };
                let struct_typ = ctx.meta_info.struct_types.get(name.ident).unwrap();
                let struct_def_node = Node::type_var(struct_def_span);
                self.add_node(struct_def_node);
                self.add_reduce_constraint(struct_def_node, struct_def_node, vec![ResolvableSpecificType::Unit]);

                let mut field_nodes = Vec::new();
                for (name, _colon, expr) in fields {
                    let expected_type = struct_typ.fields.iter()
                        .filter(|(field_name, _typ)| field_name == name.ident)
                        .map(|(_name, typ)| typ)
                        .next();
                    let expected_type = match expected_type {
                        Some(typ) => typ,
                        None => &Type::Top,
                    };
                    let expr_node = self.visit_expr(ctx, expr);
                    field_nodes.push((expr_node, expected_type));
                }

                // apply types
                let _guard = ctx.function_generics.push_generic_nodes();
                for (span, _) in &generics {
                    let synthetic = Node::synthetic(*span);
                    self.add_node(synthetic);
                    self.add_generic_constraint(node, synthetic);
                    ctx.function_generics.insert_generic(synthetic);
                }
                ctx.function_generics.apply_specific_type_reduce(node, node, node, &typ, self);
                for (expr_node, expected_type) in field_nodes {
                    ctx.function_generics.apply_type_reduce(struct_def_node, struct_def_node, expr_node, expected_type, self);
                }
            }
            Expr::EnumDefinition(_) => self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]),
            Expr::EnumInitialization(enum_init) => {
                let _guard = ctx.function_generics.push_generic_nodes();
                let generics = get_user_type_generics(ctx.meta_info, enum_init.enum_name.ident);
                for (span, _) in &generics {
                    let synthetic = Node::synthetic(*span);
                    self.add_node(synthetic);
                    self.add_generic_constraint(node, synthetic);
                    ctx.function_generics.insert_generic(synthetic);
                }
                let typ = SpecificType::Enum(Cow::Owned(enum_init.enum_name.ident.to_string()), CowVec::Owned(generics));
                ctx.function_generics.apply_specific_type_reduce(node, node, node, &typ, self);
            },
            Expr::ImplBlock(ExprImplBlock { functions, .. }) => {
                for function in functions {
                    self.visit_function(ctx, function);
                }
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
            }
        }
        node
    }

    fn visit_block<'a>(&mut self, ctx: &Context<'_, 'a, 'i>, block: &'a ExprBlock<'a, 'i>) -> Node {
        let node = Node::type_var(block.span());
        self.add_node(node);
        let ExprBlock { body: BlockBody { exprs, terminated_with_semicolon }, .. } = block;

        let mut last = None;
        for expr in exprs {
            last = Some(self.visit_expr(ctx, expr));
        }
        match (terminated_with_semicolon, last) {
            (true, _) | (false, None) => self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]),
            (false, Some(last)) => self.add_eq_constraint(node, last),
        }
        node
    }

    fn visit_function<'a>(&mut self, ctx: &Context<'_, 'a, 'i>, function: &'a ExprFunctionDefinition<'a, 'i>) -> Node {
        let _scope_guard = ctx.function_generics.push_ununifyable();
        for generic in function.sig.generics.iter().flat_map(|g| &g.generics).flatten() {
            ctx.function_generics.insert_ununifyable(generic.def_ident.span);
        }

        let node = Node::type_var(function.span());
        self.add_node(node);
        let ExprFunctionDefinition { sig, captures: _, body } = function;

        for ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. } in &sig.args {
            let arg_node = Node::type_var(binding.ident.span);
            self.add_node(arg_node);
            let arg_type = convert_expr_type(typ, ctx.diagnostics, ctx.meta_info);
            ctx.function_generics.apply_type_reduce(arg_node, arg_node, arg_node, &arg_type, self);
        }

        let body_node = Node::type_var(body.span());
        let block_guard = ctx.block_stack.push_block(BlockType::Function, body_node);
        let body_node2 = self.visit_block(ctx, body);
        drop(block_guard);
        assert_eq!(body_node, body_node2);
        if let Some((_arrow, ret_type)) = &sig.ret_type {
            let ret_type = convert_expr_type(ret_type, ctx.diagnostics, ctx.meta_info);
            ctx.function_generics.apply_type_reduce(node, node, body_node, &ret_type, self);
        }
        if sig.name.is_some() {
            self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
        } else {
            self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Function(Some(Self::get_function_type(ctx.meta_info, ctx.diagnostics, sig, false)))])
        }
        node
    }

    fn visit_expr_assign_lhs(&mut self, lhs: &ExprAssignLhs) -> Node {
        match lhs {
            ExprAssignLhs::Variable(var @ ExprVariable { binding, .. }) => {
                let binding_node = Node::type_var(binding.ident.span);
                let var_node = Node::type_var(var.span());
                self.add_node(binding_node);
                self.add_node(var_node);
                self.add_eq_constraint(binding_node, var_node);
                var_node
            }
            ExprAssignLhs::FieldAccess(ExprFieldAccess { variable: var @ ExprVariable { binding, .. }, fields, .. }) => {
                let binding_node = Node::type_var(binding.ident.span);
                let var_node = Node::type_var(var.span());
                let field_node = Node::type_var(lhs.span());
                self.add_node(binding_node);
                self.add_node(var_node);
                self.add_node(field_node);
                self.add_eq_constraint(binding_node, var_node);
                let fields = fields.iter().map(|ident| ident.ident.to_string()).collect();
                self.add_field_access(var_node, field_node, fields);
                field_node
            }
        }
    }

    pub(super) fn add_node(&mut self, node: Node) {
        if self.graph_indices.contains_key(&node) {
            return;
        }
        let ix = self.graph.add_node(node);
        self.graph_indices.insert(node, ix);
        self.possible_types.insert(node, PossibleTypes::any());
    }

    pub(super) fn add_eq_constraint(&mut self, from: Node, to: Node) {
        self.add_nonduplicate_edge(from, to, Constraint::Eq);
        self.add_nonduplicate_edge(to, from, Constraint::Eq);
    }

    pub(super) fn add_reduce_constraint(&mut self, from: Node, to: Node, reduce: Vec<ResolvableSpecificType>) {
        self.add_nonduplicate_edge(from, to, Constraint::Reduce(reduce));
    }

    fn add_field_access(&mut self, struct_node: Node, field_node: Node, field: String) {
        self.add_nonduplicate_edge(struct_node, field_node, Constraint::FieldAccess(field));
        self.add_nonduplicate_edge(field_node, struct_node, Constraint::Reduce(vec![ResolvableSpecificType::Struct("struct".to_string(), Vec::new())]));
    }

    fn add_function_call_arg(&mut self, source: Node, arg: Node, function_call_index: u64, arg_index: usize) {
        self.add_nonduplicate_edge(source, arg, Constraint::FunctionCallArg(function_call_index, arg_index));
    }
    fn add_function_call_ret(&mut self, source: Node, function_call_node: Node, function_call_index: u64) {
        self.add_nonduplicate_edge(source, function_call_node, Constraint::FunctionCallReturnType(function_call_index));
    }
    fn add_method(&mut self, source: Node, method_name: Node) {
        self.add_nonduplicate_edge(source, method_name, Constraint::Method);
    }
    fn add_method_call_arg(&mut self, source: Node, arg: Node, method_call_index: u64, arg_index: usize) {
        self.add_nonduplicate_edge(source, arg, Constraint::MethodCallArg(method_call_index, arg_index));
    }
    fn add_method_call_ret(&mut self, source: Node, method_call_node: Node, method_call_index: u64) {
        self.add_nonduplicate_edge(source, method_call_node, Constraint::MethodCallReturnType(method_call_index));
    }

    pub(super) fn add_generic_constraint(&mut self, from: Node, to: Node) {
        self.add_nonduplicate_edge(from, to, Constraint::Generic);
    }
    pub(super) fn add_generic_eq_source_constraint(&mut self, from: Node, to: Node) {
        self.add_nonduplicate_edge(from, to, Constraint::GenericEqSource);
    }

    fn add_nonduplicate_edge(&mut self, from: Node, to: Node, constraint: Constraint) {
        let from = self.graph_indices[&from];
        let to = self.graph_indices[&to];
        let contained = self.graph.edges_connecting(from, to)
            .any(|er| *er.weight() == constraint);
        if !contained {
            self.graph.add_edge(from, to, constraint);
        }
    }
}

fn get_user_type_generics(meta_info: &MetaInfo, name: &str) -> Vec<(Span, Type)> {
    match meta_info.user_types.get(name).and_then(|user_type| user_type.generics()) {
        Some(ExprGenerics { generics: Some(generics), .. }) => generics.iter()
            .map(|g| (g.def_ident.span, Type::Specific(SpecificType::Generic(g.def_ident.span))))
            .collect(),
        _ => Vec::new(),
    }
}
