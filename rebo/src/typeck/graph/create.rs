use crate::typeck::graph::{Graph, Node, PossibleTypes, Constraint};
use crate::parser::{Expr, Spanned, ExprFormatString, ExprFormatStringPart, ExprBind, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprAssign, ExprAssignLhs, ExprVariable, ExprFieldAccess, ExprBoolNot, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprBoolAnd, ExprBoolOr, ExprLessThan, ExprLessEquals, ExprEquals, ExprNotEquals, ExprGreaterEquals, ExprGreaterThan, ExprBlock, BlockBody, ExprParenthesized, ExprMatch, ExprMatchPattern, ExprWhile, ExprFunctionCall, ExprFunctionDefinition, ExprStructInitialization, ExprImplBlock, ExprType, ExprGenerics, ExprAccess, FieldOrMethod, ExprFor, ExprStatic, ExprFunctionType, ExprFunctionSignature, ExprNeg, ExprStaticSignature};
use crate::common::{MetaInfo, UserType, Function, RequiredReboFunctionStruct};
use itertools::Either;
use crate::typeck::types::{StructType, EnumType, EnumTypeVariant, SpecificType, FunctionType, Type, ResolvableSpecificType};
use diagnostic::{Diagnostics, Span};
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;
use crate::error_codes::ErrorCode;
use std::sync::atomic::{Ordering, AtomicU64};
use rt_format::Format;
use crate::{CowVec, EXTERNAL_SPAN};

static CALL_INDEX: AtomicU64 = AtomicU64::new(0);

#[derive(Clone)]
pub(super) struct FunctionGenerics {
    /// ununifyable generics within function definitions
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
    pub(super) fn apply_type_reduce(&self, from: Node, to: Node, typ: &Type, graph: &mut Graph) {
        match typ {
            Type::Top | Type::Bottom | Type::UntypedVarargs => (),
            Type::TypedVarargs(specific)
            | Type::Specific(specific) => self.apply_specific_type_reduce(from, to, specific, graph),
        }
    }
    pub(super) fn apply_specific_type_reduce(&self, from: Node, to: Node, typ: &SpecificType, graph: &mut Graph) {
        self.apply_specific_type_reduce_internal(from, to, typ, graph);
        // graph.dot();
    }
    fn apply_specific_type_reduce_internal(&self, from: Node, to: Node, typ: &SpecificType, graph: &mut Graph) {
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
                graph.add_eq_constraint(to, node);
                return;
            } else {
                eprintln!("tried to convert unknown generic {}", typ);
                graph.dot();
                unreachable!("tried to convert unknown generic {}", typ);
            }
        };
        let mut resolvable_generics = Vec::new();
        for (span, generic) in generics {
            let synthetic = Node::synthetic(*span);
            graph.add_node(synthetic);
            self.apply_type_reduce(synthetic, synthetic, generic, graph);
            graph.add_generic_constraint(from, synthetic);
            resolvable_generics.push(synthetic);
        }
        let resolvable_typ = type_constructor(resolvable_generics, name);
        graph.add_reduce_constraint(from, to, vec![resolvable_typ]);
    }
}

struct UnunifyableGuard {
    stack: Rc<RefCell<Vec<Span>>>,
    prev_stack: Option<Vec<Span>>,
}
impl Drop for UnunifyableGuard {
    fn drop(&mut self) {
        *self.stack.borrow_mut() = self.prev_stack.take().unwrap();
    }
}
struct GenericGuard {
    stack: Rc<RefCell<Vec<Vec<Node>>>>,
}
impl Drop for GenericGuard {
    fn drop(&mut self) {
        self.stack.borrow_mut().pop().unwrap();
    }
}

fn convert_expr_type(typ: &ExprType, diagnostics: &Diagnostics, meta_info: &MetaInfo) -> Type {
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
    fn add_user_types(diagnostics: &'i Diagnostics, meta_info: &mut MetaInfo) {
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
    fn verify_external_types(diagnostics: &'i Diagnostics, meta_info: &mut MetaInfo) {
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
    fn get_function_type(meta_info: &MetaInfo, diagnostics: &Diagnostics, sig: &ExprFunctionSignature, external: bool) -> FunctionType {
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
    fn add_function_types(diagnostics: &'i Diagnostics, meta_info: &mut MetaInfo) {
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
                        generics: get_user_type_generics(meta_info, &enum_name).into_iter()
                            .map(|(span, _typ)| span)
                            .collect(),
                        args: Cow::Owned(variant.fields.as_ref().unwrap().1.iter()
                            .map(|typ| convert_expr_type(typ, diagnostics, meta_info))
                            .collect()),
                        ret: Type::Specific(SpecificType::Enum(Cow::Owned(enum_name.clone()), CowVec::Owned(get_user_type_generics(meta_info, &enum_name)))),
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
    fn check_required_rebo_functions(diagnostics: &'i Diagnostics, meta_info: &mut MetaInfo) {
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
    pub fn create(diagnostics: &'i Diagnostics, meta_info: &mut MetaInfo, exprs: &[&Expr<'_, '_>]) -> Graph<'i> {
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

        // resolve local types
        let function_generics = FunctionGenerics::new();
        for expr in exprs {
            graph.visit_expr(diagnostics, meta_info, &function_generics, expr);
        }
        graph
    }

    fn visit_expr(&mut self, diagnostics: &Diagnostics, meta_info: &MetaInfo, function_generics: &FunctionGenerics, expr: &Expr<'_, '_>) -> Node {
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
                            let node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
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
                        let typ = convert_expr_type(typ, diagnostics, meta_info);
                        function_generics.apply_type_reduce(var_node, var_node, &typ, self);
                        var_node
                    }
                    ExprPattern::Untyped(ExprPatternUntyped { binding }) => {
                        let var_node = Node::type_var(binding.ident.span);
                        self.add_node(var_node);
                        var_node
                    }
                };
                let expr_node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
                self.add_eq_constraint(var_node, expr_node);
            }
            Expr::Assign(ExprAssign { lhs, expr, .. }) => {
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
                let expr_node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
                match lhs {
                    ExprAssignLhs::Variable(ExprVariable { binding, .. }) => {
                        let var_node = Node::type_var(binding.ident.span);
                        self.add_node(var_node);
                        self.add_eq_constraint(var_node, expr_node);
                    }
                    ExprAssignLhs::FieldAccess(ExprFieldAccess { variable: ExprVariable { binding, .. }, fields, .. }) => {
                        let var_node = Node::type_var(binding.ident.span);
                        let field_node = Node::type_var(lhs.span());
                        self.add_node(var_node);
                        self.add_node(field_node);
                        let fields = fields.iter().map(|ident| ident.ident.to_string()).collect();
                        self.add_field_access(var_node, field_node, fields);
                        self.add_eq_constraint(field_node, expr_node);
                    }
                }
            }
            Expr::BoolNot(ExprBoolNot { expr, .. }) => {
                let expr_node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Bool]);
                self.add_eq_constraint(node, expr_node);
            }
            Expr::Neg(ExprNeg { expr, .. }) => {
                let expr_node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_eq_constraint(node, expr_node);
            }
            Expr::Add(ExprAdd { a, b, .. })
            | Expr::Sub(ExprSub { a, b, .. })
            | Expr::Mul(ExprMul { a, b, .. })
            | Expr::Div(ExprDiv { a, b, .. }) => {
                let a_node = self.visit_expr(diagnostics, meta_info, function_generics, a);
                let b_node = self.visit_expr(diagnostics, meta_info, function_generics, b);
                self.add_reduce_constraint(node, a_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_reduce_constraint(node, b_node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Integer, ResolvableSpecificType::Float]);
                self.add_eq_constraint(a_node, node);
                self.add_eq_constraint(b_node, node);
            }
            Expr::BoolAnd(ExprBoolAnd { a, b, .. })
            | Expr::BoolOr(ExprBoolOr { a, b, .. }) => {
                let a_node = self.visit_expr(diagnostics, meta_info, function_generics, a);
                let b_node = self.visit_expr(diagnostics, meta_info, function_generics, b);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Bool]);
                self.add_eq_constraint(node, a_node);
                self.add_eq_constraint(node, b_node);
            }
            Expr::LessThan(ExprLessThan { a, b, .. })
            | Expr::LessEquals(ExprLessEquals { a, b, .. })
            | Expr::Equals(ExprEquals { a, b, .. })
            | Expr::NotEquals(ExprNotEquals { a, b, .. })
            | Expr::GreaterEquals(ExprGreaterEquals { a, b, .. })
            | Expr::GreaterThan(ExprGreaterThan { a, b, .. }) => {
                let a_node = self.visit_expr(diagnostics, meta_info, function_generics, a);
                let b_node = self.visit_expr(diagnostics, meta_info, function_generics, b);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Bool]);
                self.add_eq_constraint(a_node, b_node);
            }
            Expr::Block(block) => {
                assert_eq!(self.visit_block(diagnostics, meta_info, function_generics, block), node);
            }
            Expr::Variable(ExprVariable { binding, .. }) => {
                let var_node = Node::type_var(binding.ident.span);
                self.add_node(var_node);
                self.add_unidirectional_eq_constraint(var_node, node);
                return var_node;
            }
            Expr::Access(ExprAccess { variable, accesses, .. }) => {
                let var_node = Node::type_var(variable.span());
                let binding_node = Node::type_var(variable.binding.ident.span);
                self.add_node(var_node);
                self.add_node(binding_node);
                self.add_unidirectional_eq_constraint(binding_node, var_node);
                let mut access_node = binding_node;
                for access in accesses {
                    access_node = match access {
                        FieldOrMethod::Field(field) => {
                            let field_node = Node::type_var(field.span);
                            self.add_node(field_node);
                            self.add_field_access(access_node, field_node, field.ident.to_string());
                            field_node
                        }
                        FieldOrMethod::Method(fn_call) => {

                            let method_call_node = Node::type_var(fn_call.span());
                            self.add_node(method_call_node);

                            let idx = CALL_INDEX.fetch_add(1, Ordering::SeqCst);
                            self.add_method_call_ret(access_node, method_call_node, fn_call.name.ident.to_string(), idx);
                            // self-argument
                            self.add_method_call_arg(access_node, access_node, fn_call.name.ident.to_string(), idx, 0);
                            for (i, arg_expr) in fn_call.args.iter().enumerate() {
                                let arg_node = self.visit_expr(diagnostics, meta_info, function_generics, arg_expr);
                                // arg 0 is `self`
                                let arg_index = i + 1;
                                self.add_method_call_arg(access_node, arg_node, fn_call.name.ident.to_string(), idx, arg_index);
                            }
                            method_call_node
                        }
                    };
                }
                self.add_eq_constraint(node, access_node);
            }
            Expr::Parenthesized(ExprParenthesized { expr, .. }) => {
                let expr_node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
                self.add_eq_constraint(expr_node, node);
            }
            Expr::IfElse(ifelse) => {
                let mut block_nodes = Vec::new();
                for (condition, body) in ifelse.iter_branches() {
                    if let Some(condition) = condition {
                        let cond_node = self.visit_expr(diagnostics, meta_info, function_generics, condition);
                        self.add_reduce_constraint(node, cond_node, vec![ResolvableSpecificType::Bool]);
                    }
                    block_nodes.push(self.visit_block(diagnostics, meta_info, function_generics, body));
                }
                for block_node in block_nodes {
                    self.add_eq_constraint(node, block_node);
                }
            }
            Expr::Match(ExprMatch { expr, arms, .. }) => {
                let expr_node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
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
                            let generics = get_user_type_generics(meta_info, variant.enum_name.ident);
                            let _scope = function_generics.push_generic_nodes();
                            for (gen, _) in &generics {
                                let synthetic = Node::synthetic(*gen);
                                self.add_node(synthetic);
                                function_generics.insert_generic(synthetic);
                            }
                            let typ = SpecificType::Enum(Cow::Owned(variant.enum_name.ident.to_string()), CowVec::Owned(generics));
                            function_generics.apply_specific_type_reduce(pat_node, pat_node, &typ, self);
                            self.add_eq_constraint(expr_node, pat_node);

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
                                let field_binding_node = Node::type_var(field_binding.ident.span);
                                // always add all field type-vars even for unknown enums / variants
                                self.add_node(field_binding_node);
                                function_generics.apply_type_reduce(pat_node, field_binding_node, expected_type, self);
                            }
                        }
                        ExprMatchPattern::Binding(binding) => {
                            let binding_node = Node::type_var(binding.ident.span);
                            self.add_node(binding_node);
                            self.add_eq_constraint(binding_node, expr_node);
                        }
                        ExprMatchPattern::Wildcard(_) => (),
                    }
                    let arm_expr_node = self.visit_expr(diagnostics, meta_info, function_generics, arm_expr);
                    self.add_eq_constraint(node, arm_expr_node);
                }
            }
            Expr::While(ExprWhile { condition, block, .. }) => {
                let cond_node = self.visit_expr(diagnostics, meta_info, function_generics, condition);
                self.add_reduce_constraint(node, cond_node, vec![ResolvableSpecificType::Bool]);
                let block_node = self.visit_block(diagnostics, meta_info, function_generics, block);
                self.add_eq_constraint(node, block_node);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
            }
            Expr::For(ExprFor { binding, expr, block, .. }) => {
                let binding_node = Node::type_var(binding.ident.span);
                self.add_node(binding_node);
                let expr_node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
                let block_node = self.visit_block(diagnostics, meta_info, function_generics, block);
                let list_t = meta_info.user_types["List"].generics().unwrap().generics.as_ref().unwrap().iter().next().unwrap().def_ident.span;

                let synthetic = Node::synthetic(list_t);
                self.add_node(synthetic);
                self.add_reduce_constraint(node, expr_node, vec![ResolvableSpecificType::Struct("List".to_string(), vec![synthetic])]);
                self.add_eq_constraint(binding_node, synthetic);
                self.add_eq_constraint(node, block_node);
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
            }
            Expr::FunctionCall(ExprFunctionCall { name, args, .. }) => {
                let var_node = Node::type_var(name.binding.ident.span);
                self.add_node(var_node);
                if let Some(name) = meta_info.function_bindings.get(&name.binding) {
                    if let Some(fun) = meta_info.function_types.get(name.as_str()) {
                        self.add_reduce_constraint(var_node, var_node, vec![ResolvableSpecificType::Function(Some(fun.clone()))]);
                    }
                }

                let idx = CALL_INDEX.fetch_add(1, Ordering::SeqCst);
                self.add_function_call_ret(var_node, node, idx);
                for (i, arg_expr) in args.iter().enumerate() {
                    let arg_node = self.visit_expr(diagnostics, meta_info, function_generics, arg_expr);
                    self.add_function_call_arg(var_node, arg_node, idx, i);
                }
            }
            Expr::FunctionDefinition(function) => {
                self.visit_function(diagnostics, meta_info, function_generics, function);
            }
            Expr::StructDefinition(_) => self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]),
            Expr::StructInitialization(ExprStructInitialization { name, fields, .. }) => {
                let generics = get_user_type_generics(meta_info, name.ident);
                let typ = SpecificType::Struct(Cow::Owned(name.ident.to_string()), CowVec::Owned(generics.clone()));
                let struct_def_span = match meta_info.user_types.get(name.ident) {
                    Some(user_type) => user_type.span(),
                    None => return node,
                };
                let struct_typ = meta_info.struct_types.get(name.ident).unwrap();
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
                        None => continue,
                    };
                    let expr_node = self.visit_expr(diagnostics, meta_info, function_generics, expr);
                    field_nodes.push((expr_node, expected_type));
                }

                // apply types
                let _guard = function_generics.push_generic_nodes();
                for (span, _) in &generics {
                    let synthetic = Node::synthetic(*span);
                    self.add_node(synthetic);
                    function_generics.insert_generic(synthetic);
                }
                function_generics.apply_specific_type_reduce(node, node, &typ, self);
                for (expr_node, expected_type) in field_nodes {
                    function_generics.apply_type_reduce(struct_def_node, expr_node, expected_type, self);
                }
            }
            Expr::EnumDefinition(_) => self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]),
            Expr::EnumInitialization(enum_init) => {
                let _guard = function_generics.push_generic_nodes();
                let generics = get_user_type_generics(meta_info, enum_init.enum_name.ident);
                for (span, _) in &generics {
                    let synthetic = Node::synthetic(*span);
                    self.add_node(synthetic);
                    function_generics.insert_generic(synthetic);
                }
                let typ = SpecificType::Enum(Cow::Owned(enum_init.enum_name.ident.to_string()), CowVec::Owned(generics));
                function_generics.apply_specific_type_reduce(node, node, &typ, self);
            },
            Expr::ImplBlock(ExprImplBlock { functions, .. }) => {
                for function in functions {
                    self.visit_function(diagnostics, meta_info, function_generics, function);
                }
                self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
            }
        }
        node
    }

    fn visit_block(&mut self, diagnostics: &Diagnostics, meta_info: &MetaInfo, function_generics: &FunctionGenerics, block: &ExprBlock) -> Node {
        let node = Node::type_var(block.span());
        self.add_node(node);
        let ExprBlock { body: BlockBody { exprs, terminated_with_semicolon }, .. } = block;

        let mut last = None;
        for expr in exprs {
            last = Some(self.visit_expr(diagnostics, meta_info, function_generics, expr));
        }
        match (terminated_with_semicolon, last) {
            (true, _) | (false, None) => self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]),
            (false, Some(var)) => self.add_eq_constraint(node, var),
        }
        node
    }

    fn visit_function(&mut self, diagnostics: &Diagnostics, meta_info: &MetaInfo, function_generics: &FunctionGenerics, function: &ExprFunctionDefinition) -> Node {
        let _scope_guard = function_generics.push_ununifyable();
        for generic in function.sig.generics.iter().flat_map(|g| &g.generics).flatten() {
            function_generics.insert_ununifyable(generic.def_ident.span);
        }

        let node = Node::type_var(function.span());
        self.add_node(node);
        let ExprFunctionDefinition { sig, body } = function;

        for ExprPatternTyped { pattern: ExprPatternUntyped { binding }, typ, .. } in &sig.args {
            let arg_node = Node::type_var(binding.ident.span);
            self.add_node(arg_node);
            let arg_type = convert_expr_type(typ, diagnostics, meta_info);
            function_generics.apply_type_reduce(arg_node, arg_node, &arg_type, self);
        }

        let body_node = self.visit_block(diagnostics, meta_info, function_generics, body);
        if let Some((_arrow, ret_type)) = &sig.ret_type {
            let ret_type = convert_expr_type(ret_type, diagnostics, meta_info);
            function_generics.apply_type_reduce(node, body_node, &ret_type, self);
        }
        if sig.name.is_some() {
            self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Unit]);
        } else {
            self.add_reduce_constraint(node, node, vec![ResolvableSpecificType::Function(Some(Self::get_function_type(meta_info, diagnostics, sig, false)))])
        }
        node
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
    fn add_unidirectional_eq_constraint(&mut self, from: Node, to: Node) {
        self.add_nonduplicate_edge(from, to, Constraint::Eq);
    }

    fn add_reduce_constraint(&mut self, from: Node, to: Node, reduce: Vec<ResolvableSpecificType>) {
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
    fn add_method_call_arg(&mut self, source: Node, arg: Node, method_name: String, method_call_index: u64, arg_index: usize) {
        self.add_nonduplicate_edge(source, arg, Constraint::MethodCallArg(method_name, method_call_index, arg_index));
    }
    fn add_method_call_ret(&mut self, source: Node, method_call_node: Node, method_name: String, method_call_index: u64) {
        self.add_nonduplicate_edge(source, method_call_node, Constraint::MethodCallReturnType(method_name, method_call_index));
    }

    fn add_generic_constraint(&mut self, from: Node, to: Node) {
        self.add_nonduplicate_edge(from, to, Constraint::Generic);
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
