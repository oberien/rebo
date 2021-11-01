use diagnostic::Diagnostics;
use crate::typeck::graph::{Graph, Constraint, Node};
use crate::common::MetaInfo;
use crate::error_codes::ErrorCode;
use itertools::Itertools;
use crate::typeck::types::{Type, SpecificType, ResolvableSpecificType};

impl<'i> Graph<'i> {
    pub fn check(&self, diagnostics: &Diagnostics, meta_info: &mut MetaInfo) {
        for type_var in self.type_vars() {
            let node = Node::TypeVar(type_var);
            match self.try_convert_possible_types(diagnostics, meta_info, node) {
                Some(typ) => meta_info.types.insert(type_var, typ),
                None => meta_info.types.insert(type_var, Type::Top),
            };
        }
    }
    fn try_convert_possible_types(&self, diagnostics: &Diagnostics, meta_info: &mut MetaInfo, node: Node) -> Option<Type> {
        let types = self.possible_types(node);
        if types.len() == 1 {
            return Some(Type::Specific(match &types[0] {
                ResolvableSpecificType::Unit => SpecificType::Unit,
                ResolvableSpecificType::Bool => SpecificType::Bool,
                ResolvableSpecificType::Integer => SpecificType::Integer,
                ResolvableSpecificType::Float => SpecificType::Float,
                ResolvableSpecificType::String => SpecificType::String,
                ResolvableSpecificType::Struct(name, generics) => SpecificType::Struct(
                    name.clone(),
                    generics.iter().copied()
                        .map(|node| (node.span(), self.try_convert_possible_types(diagnostics, meta_info, node).unwrap_or(Type::Top)))
                        .collect(),
                ),
                ResolvableSpecificType::Enum(name, generics) => SpecificType::Enum(
                    name.clone(),
                    generics.iter().copied()
                        .map(|node| (node.span(), self.try_convert_possible_types(diagnostics, meta_info, node).unwrap_or(Type::Top)))
                        .collect(),
                ),
                &ResolvableSpecificType::UnUnifyableGeneric(span) => SpecificType::Generic(span),
            }));
        }
        if types.len() > 1 {
            return None;
        }
        let mut diag = if types.is_empty() {
            diagnostics.error(ErrorCode::UnableToInferType)
                .with_error_label(node.span(), "can't infer this type")
        } else {
            diagnostics.error(ErrorCode::TypeConflict)
                .with_error_label(node.span(), "can't infer this type")
                .with_info_label(node.span(), "must be a single type")
                .with_info_label(node.span(), format!("inferred `{}`", types.iter().join(", ")))
        };
        for (constraint, incoming) in self.incoming(node) {
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
                Constraint::MethodCallArg(method_name, _, arg_index) => {
                    let field_access_typ = self.possible_types(incoming);
                    if field_access_typ.len() != 1 {
                        "can't infer type of this method-call target".to_string()
                    } else {
                        let type_name = field_access_typ[0].type_name();
                        let fn_name = format!("{}::{}", type_name, method_name);
                        match meta_info.function_types.get(fn_name.as_str()) {
                            Some(fn_typ) => match fn_typ.args.iter().chain(std::iter::repeat(&Type::Varargs)).nth(arg_index) {
                                Some(Type::Top | Type::Varargs) => "this says the argument can be of any type".to_string(),
                                Some(Type::Bottom) => unreachable!("fn arg type is bottom"),
                                Some(Type::Specific(specific)) => format!("this says the argument must have type {}", specific),
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
                                Type::Top | Type::Varargs => unreachable!("fn arg type is bottom"),
                                Type::Bottom => "this says the return type can be any type".to_string(),
                                Type::Specific(specific) => format!("this says the return type must be {}", specific),
                            }
                            None => format!("can't find function {}", fn_name),
                        }
                    }
                }
                Constraint::Generic => continue,
            };
            diag = diag.with_info_label(incoming.span(), msg);
        }
        diag.emit();
        None
    }

    fn possible_types(&self, node: Node) -> &[ResolvableSpecificType] {
        &self.possible_types[&node].0
    }
}
