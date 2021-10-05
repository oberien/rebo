use diagnostic::Diagnostics;
use crate::typeck::graph::{Graph, Constraint, PossibleTypes};
use crate::common::MetaInfo;
use crate::error_codes::ErrorCode;
use itertools::Itertools;
use crate::typeck::types::{Type, SpecificType};

pub fn check(diagnostics: &Diagnostics, graph: &Graph, meta_info: &mut MetaInfo) {
    for node in graph.type_vars() {
        let types = graph.possible_types(node);
        if types.len() == 1 {
            meta_info.types.insert(node, Type::Specific(types[0].clone()));
            continue;
        }
        if types == PossibleTypes::any() {
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
                            SpecificType::Struct(name) => {
                                match meta_info.struct_types[name.as_str()].get_field_path(meta_info, &fields) {
                                    Ok(Type::Specific(typ)) => format!("this says the field has type `{}`", typ),
                                    _ => format!("can't find fields starting from `{}`", struct_typ[0]),
                                }
                            },
                            _ => "can't infer this struct type".to_string(),
                        }
                    }
                }
                Constraint::MethodCallArg(method_name, arg_index) => todo!(), // TODO
                Constraint::MethodCallReturnType(method_name) => todo!(), // TODO
            };
            diag = diag.with_info_label(incoming.span, msg);
        }
        diag.emit();
        meta_info.types.insert(node, Type::Top);
    }
}