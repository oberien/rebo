use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, SpecificType};
use crate::parser::{ExprFunctionDefinition, ExprBlock, BlockBody};
use crate::error_codes::ErrorCode;

pub struct EmptyFunctionBody;

impl Visitor for EmptyFunctionBody {
    fn visit_function_definition(&self, diagnostics: &Diagnostics, _: &MetaInfo, def: &ExprFunctionDefinition) {
        let ExprFunctionDefinition { name, ret_type, body: ExprBlock { body: BlockBody { exprs, .. }, .. }, .. } = def;
        let ret_type = match ret_type {
            Some((_arrow, typ)) => SpecificType::from(typ),
            None => SpecificType::Unit,
        };
        if exprs.is_empty() && ret_type != SpecificType::Unit {
            diagnostics.error(ErrorCode::EmptyFunctionBody)
                .with_error_label(name.span, format!("this function returns {} but has an empty body", ret_type))
                .emit();
        }
    }
}
