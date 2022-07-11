use std::borrow::Cow;
use diagnostic::Diagnostics;
use crate::typeck::graph::{Graph, Constraint, Node};
use crate::common::MetaInfo;
use crate::error_codes::ErrorCode;
use itertools::Itertools;
use crate::typeck::types::{Type, SpecificType, ResolvableSpecificType};
use std::collections::{HashSet, VecDeque};
use crate::util::CowVec;

impl<'i> Graph<'i> {
    pub fn check(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &mut MetaInfo) {
        let mut already_errored = HashSet::new();
        for node in self.nodes() {
            let typ = match self.try_convert_possible_types(&mut already_errored, diagnostics, meta_info, node) {
                Some(typ) => typ,
                None => Type::Top,
            };
            if let Node::TypeVar(type_var) = node {
                meta_info.types.insert(type_var, typ);
            }
        }
    }
    fn try_convert_possible_types(&self, already_errored: &mut HashSet<Node>, diagnostics: &Diagnostics<ErrorCode>, meta_info: &mut MetaInfo, node: Node) -> Option<Type> {
        let types = self.possible_types(node);
        if types.len() == 1 {
            return Some(Type::Specific(match &types[0] {
                ResolvableSpecificType::Unit => SpecificType::Unit,
                ResolvableSpecificType::Bool => SpecificType::Bool,
                ResolvableSpecificType::Integer => SpecificType::Integer,
                ResolvableSpecificType::Float => SpecificType::Float,
                ResolvableSpecificType::String => SpecificType::String,
                ResolvableSpecificType::Function(Some(typ)) => SpecificType::Function(Box::new(typ.clone())),
                ResolvableSpecificType::Function(None) => return None,
                ResolvableSpecificType::Struct(name, generics) => SpecificType::Struct(
                    Cow::Owned(name.clone()),
                    CowVec::Owned(generics.iter().copied()
                        .map(|node| (node.span(), self.try_convert_possible_types(already_errored, diagnostics, meta_info, node).unwrap_or(Type::Top)))
                        .collect()),
                ),
                ResolvableSpecificType::Enum(name, generics) => SpecificType::Enum(
                    Cow::Owned(name.clone()),
                    CowVec::Owned(generics.iter().copied()
                        .map(|node| (node.span(), self.try_convert_possible_types(already_errored, diagnostics, meta_info, node).unwrap_or(Type::Top)))
                        .collect()),
                ),
                &ResolvableSpecificType::UnUnifyableGeneric(span) => SpecificType::Generic(span),
            }));
        }
        if types.len() > 1 {
            return None;
        }
        if already_errored.contains(&node) {
            return None;
        }
        already_errored.insert(node);
        let mut diag = if types.is_empty() {
            diagnostics.error(ErrorCode::UnableToInferType)
                .with_error_label(node.span(), "can't infer this type")
        } else {
            // TODO: unreachable branch?
            diagnostics.error(ErrorCode::TypeConflict)
                .with_error_label(node.span(), "can't infer this type")
                .with_info_label(node.span(), "must be a single type")
                .with_info_label(node.span(), format!("inferred `{}`", types.iter().join(", ")))
        };
        for (_, constraint, incoming) in self.incoming(node) {
            let msg = match constraint {
                Constraint::Eq => format!("must have same type as this (`{}`)", self.possible_types(incoming).iter().join(", ")),
                Constraint::Reduce(reduce) => {
                    let prefix = if reduce.len() == 1 { "" } else { "one of " };
                    format!("this means it can only be {}`{}`", prefix, reduce.iter().join(", "))
                },
                Constraint::FieldAccess(field) => {
                    let struct_typ = self.possible_types(incoming);
                    if struct_typ.len() != 1 {
                        "can't infer this struct type".to_string()
                    } else {
                        match &struct_typ[0] {
                            ResolvableSpecificType::Struct(name, _) => {
                                match meta_info.struct_types[name.as_str()].get_field(&field) {
                                    Some(Type::Specific(typ)) => format!("this says the field has type `{}`", typ),
                                    _ => format!("can't find field `{}` in `{}`", field, struct_typ[0]),
                                }
                            },
                            _ => "can't infer this struct type".to_string(),
                        }
                    }
                }
                Constraint::FunctionCallArg(_, arg_index) => {
                    let function_typ = self.possible_types(incoming);
                    if function_typ.len() != 1 {
                        "can't infer type of the function".to_string()
                    } else {
                        match &function_typ[0] {
                            ResolvableSpecificType::Function(Some(fn_typ)) => match fn_typ.args.iter().chain(std::iter::repeat(&Type::UntypedVarargs)).nth(arg_index) {
                                Some(Type::Top | Type::UntypedVarargs) => "this says the argument can be of any type".to_string(),
                                Some(Type::Bottom) => unreachable!("fn arg type is bottom"),
                                Some(Type::TypedVarargs(specific))
                                | Some(Type::Specific(specific)) => format!("this says the argument must have type {}", specific),
                                None => unreachable!("iter::repeat ended"),
                            }
                            ResolvableSpecificType::Function(None) => "can't infer type of the function".to_string(),
                            _ => "tried to call a non-function".to_string(),
                        }
                    }
                }
                Constraint::FunctionCallReturnType(_) => {
                    let function_typ = self.possible_types(incoming);
                    if function_typ.len() != 1 {
                        "can't infer type of the function".to_string()
                    } else {
                        match &function_typ[0] {
                            ResolvableSpecificType::Function(Some(fn_typ)) => match &fn_typ.ret {
                                Type::Top | Type::UntypedVarargs | Type::TypedVarargs(_) => unreachable!("fn ret type is Top or Varargs"),
                                Type::Bottom => "this says the return type can be any type".to_string(),
                                Type::Specific(specific) => format!("this says the return type must be {}", specific),
                            }
                            ResolvableSpecificType::Function(None) => "can't infer type of the function".to_string(),
                            _ => "tried to call a non-function".to_string(),
                        }
                    }
                }
                Constraint::MethodCallArg(method_name, _, arg_index) => {
                    let field_access_typ = self.possible_types(incoming);
                    if field_access_typ.len() != 1 {
                        "can't infer type of this method-call target".to_string()
                    } else {
                        let type_name = field_access_typ[0].type_name();
                        let fn_name = format!("{}::{}", type_name, method_name);
                        match meta_info.function_types.get(fn_name.as_str()) {
                            Some(fn_typ) => match fn_typ.args.iter().chain(std::iter::repeat(&Type::UntypedVarargs)).nth(arg_index) {
                                Some(Type::Top | Type::UntypedVarargs) => "this says the argument can be of any type".to_string(),
                                Some(Type::Bottom) => unreachable!("fn arg type is bottom"),
                                Some(Type::TypedVarargs(specific))
                                | Some(Type::Specific(specific)) => format!("this says the argument must have type {}", specific),
                                None => unreachable!("iter::repeat ended"),
                            }
                            None => format!("can't find function {}", fn_name),
                        }
                    }
                }
                Constraint::MethodCallReturnType(method_name, _) => {
                    let field_access_typ = self.possible_types(incoming);
                    if field_access_typ.len() != 1 {
                        "can't infer type of this method-call target".to_string()
                    } else {
                        let type_name = field_access_typ[0].type_name();
                        let fn_name = format!("{}::{}", type_name, method_name);
                        match meta_info.function_types.get(fn_name.as_str()) {
                            Some(fn_typ) => match &fn_typ.ret {
                                Type::Top | Type::UntypedVarargs | Type::TypedVarargs(_) => unreachable!("fn ret type is Top or Varargs"),
                                Type::Bottom => "this says the return type can be any type".to_string(),
                                Type::Specific(specific) => format!("this says the return type must be {}", specific),
                            }
                            None => format!("can't find function {}", fn_name),
                        }
                    }
                }
                Constraint::Generic | Constraint::GenericEqSource => continue,
            };
            let normalized = self.normalize(incoming);
            diag = diag.with_info_label(normalized.span(), msg);
        }

        diag.emit();
        None
    }

    /// Normalize nodes before being used in diagnostics.
    ///
    /// When generic nodes are used in diagnostics, they shouldn't be shown when displaying
    /// sources of the errors.
    /// Type conflicts in generics always happen because two nodes representing the same generic
    /// have an equal constraint but different types.
    /// Displaying one of them as the error and the other as the source of the error results
    /// in both messages just being highlighted on the generic in the original type instead
    /// of where the type is actually used.
    /// Take the following code:
    /// ```rust,ignore
    /// fn foo(l: List<int>) {}
    /// fn bar(l: List<float>) {}
    /// let mut list = List::new();
    /// foo(list);
    /// bar(list);
    /// ```
    /// The error diagnostic would be
    /// ```rust,ignore
    /// error[0012]: unable to infer type
    ///   ┌─ external-List.re:1:13
    ///   │
    /// 1 │ struct List<T> {
    ///   │             ^
    ///   │             │
    ///   │             can't infer this type
    ///   │             this means it can only be `int`
    ///   │             must have same type as this (`float`)
    /// ```
    /// This diagnostic doesn't show where the actual error occurred in the code in
    /// possibly thousands of lines of code.
    ///
    /// To resolve this problem, when displaying the sources of why there was a type error,
    /// we normalize generic nodes to instead show the usage which resulted in the type error
    /// of the generic in the diagnostic.
    fn normalize(&self, node: Node) -> Node {
        match node {
            // early return on non-generics
            Node::TypeVar(_) => return node,
            Node::Synthetic(..) => (),
        }

        // start from all nodes with a `GenericEqSource` as those are usages of the generic
        // which produced the equal constraint which resulted in the type error.
        let incoming_generic_eq_source = self.incoming(node).into_iter().filter_map(|(_ix, constraint, node)| match constraint {
            Constraint::GenericEqSource => Some(node),
            _ => None,
        });
        let mut visited = HashSet::new();
        let mut todo: VecDeque<_> = incoming_generic_eq_source.collect();

        // breadth-first-search over `Eq` edges to find an incoming `Reduce` containing the generic_id
        while let Some(current) = todo.pop_front() {
            for (_edge_index, constraint, incoming) in self.incoming(current) {
                match constraint {
                    Constraint::Eq => {
                        if !visited.contains(&incoming) {
                            visited.insert(incoming);
                            todo.push_back(incoming);
                        }
                    },
                    Constraint::Reduce(reduce) => {
                        if reduce.iter().any(|typ| typ.generics().contains(&node)) {
                            return current;
                        }
                    }
                    _ => (),
                }
            }
        }
        node
    }

    fn possible_types(&self, node: Node) -> &[ResolvableSpecificType] {
        &self.possible_types[&node].0
    }
}
