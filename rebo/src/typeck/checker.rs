use diagnostic::Diagnostics;
use crate::typeck::graph::{Graph, Constraint};
use crate::common::MetaInfo;
use crate::error_codes::ErrorCode;
use itertools::Itertools;
use crate::typeck::types::{Type, SpecificType, ResolvableSpecificType};

pub fn check(diagnostics: &Diagnostics, graph: &Graph, meta_info: &mut MetaInfo) {
    for node in graph.type_vars() {
        let types = graph.possible_types(node);
        if types.len() == 1 {
            meta_info.types.insert(node, Type::Specific(SpecificType::from(&types[0])));
            continue;
        }
        if types.len() > 1 {
            meta_info.types.insert(node, Type::Top);
            continue;
        }
        let mut diag = if types.is_empty() {
            diagnostics.error(ErrorCode::UnableToInferType)
                .with_error_label(node.span, "can't infer this type")
        } else {
            diagnostics.error(ErrorCode::TypeConflict)
                .with_error_label(node.span, "can't infer this type")
                .with_info_label(node.span, "must be a single type")
                .with_info_label(node.span, format!("inferred `{}`", types.iter().join(", ")))
        };
        for (constraint, incoming) in graph.incoming(node) {
            let msg = match constraint {
                Constraint::Eq => format!("must have same type as this (`{}`)", graph.possible_types(incoming).iter().join(", ")),
                Constraint::Struct => "this field access means that it must be a struct".to_string(),
                Constraint::Reduce(reduce) => {
                    let prefix = if reduce.len() == 1 { "" } else { "one of " };
                    format!("this means it can only be {}`{}`", prefix, reduce.iter().join(", "))
                },
                Constraint::FieldAccess(fields) => {
                    let struct_typ = graph.possible_types(incoming);
                    if struct_typ.len() != 1 {
                        "can't infer this struct type".to_string()
                    } else {
                        match &struct_typ[0] {
                            ResolvableSpecificType::Struct(name) => {
                                match meta_info.struct_types[name.as_str()].get_field_path(meta_info, &fields) {
                                    Ok(Type::Specific(typ)) => format!("this says the field has type `{}`", typ),
                                    _ => format!("can't find fields starting from `{}`", struct_typ[0]),
                                }
                            },
                            _ => "can't infer this struct type".to_string(),
                        }
                    }
                }
                Constraint::MethodCallArg(method_name, arg_index) => {
                    let field_access_typ = graph.possible_types(incoming);
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
                Constraint::MethodCallReturnType(method_name) => {
                    let field_access_typ = graph.possible_types(incoming);
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
            };
            diag = diag.with_info_label(incoming.span, msg);
        }
        diag.emit();
        meta_info.types.insert(node, Type::Top);
    }
}