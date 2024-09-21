use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{BlockStack, MetaInfo};
use crate::common::Spanned;
use crate::parser::{BlockBody, ExprBlock, ExprFunctionDefinition};
use crate::error_codes::ErrorCode;
use crate::typeck::types::{SpecificType, Type};

pub struct FunctionLints;

impl Visitor for FunctionLints {
    fn visit_function_definition(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, def: &ExprFunctionDefinition) {
        let ExprFunctionDefinition { sig, captures, body: ExprBlock { body: BlockBody { exprs, .. }, .. }, .. } = def;
        // TODO: can this be better?
        let rebo_function = meta_info.rebo_functions.iter().find(|(_name, fun)| fun.span_with_id() == def.span_with_id());
        let full_name = match rebo_function {
            Some((name, _def)) => name,
            // external function
            None => return,
        };
        let ret_type = &meta_info.function_types[full_name].ret;

        if exprs.is_empty() && *ret_type != Type::Specific(SpecificType::Unit) {
            diagnostics.error(ErrorCode::EmptyFunctionBody)
                .with_error_label(sig.diagnostics_span(), format!("this function returns {} but has an empty body", ret_type))
                .emit();
        }

        if sig.name.is_some() && !captures.is_empty() {
            for capture in captures {
                diagnostics.error(ErrorCode::NamedFunctionCapture)
                    .with_error_label(sig.diagnostics_span(), format!("this function captures binding `{}` but isn't allowed to", capture.ident.ident))
                    .with_note("only closures / anonymous functions can capture bindings from outer scopes")
                    .emit();
            }
        }
    }
}
