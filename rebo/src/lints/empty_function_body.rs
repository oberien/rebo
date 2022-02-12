use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, BlockStack};
use crate::parser::{ExprFunctionDefinition, ExprBlock, BlockBody, Spanned};
use crate::error_codes::ErrorCode;
use crate::typeck::types::{SpecificType, Type};

pub struct EmptyFunctionBody;

impl Visitor for EmptyFunctionBody {
    fn visit_function_definition(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, def: &ExprFunctionDefinition) {
        let ExprFunctionDefinition { sig, body: ExprBlock { body: BlockBody { exprs, .. }, .. }, .. } = def;
        // TODO: can this be better?
        let rebo_function = meta_info.rebo_functions.iter().find(|(_name, fun)| fun.span() == def.span());
        let full_name = match rebo_function {
            Some((name, _def)) => name,
            // function was already defined and not added
            None => return,
        };
        let ret_type = &meta_info.function_types[full_name].ret;

        if exprs.is_empty() && *ret_type != Type::Specific(SpecificType::Unit) {
            diagnostics.error(ErrorCode::EmptyFunctionBody)
                .with_error_label(sig.span(), format!("this function returns {} but has an empty body", ret_type))
                .emit();
        }
    }
}
