use diagnostic::Diagnostics;
use crate::typeck2::graph::{Graph, Constraint, PossibleTypes};
use crate::common::{MetaInfo, Type};
use crate::error_codes::ErrorCode;
use itertools::Itertools;

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
                Constraint::FieldAccess(_) => "".to_string(), // TODO
            };
            diag = diag.with_info_label(incoming.span, msg);
        }
        diag.emit();
        meta_info.types.insert(node, Type::Top);
    }
}