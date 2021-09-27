use crate::lints::visitor::Visitor;
use crate::parser::ExprFunctionCall;
use crate::common::MetaInfo;
use diagnostic::Diagnostics;
use crate::error_codes::ErrorCode;

pub struct UnknownFunction;

impl Visitor for UnknownFunction {
    fn visit_function_call(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, call: &ExprFunctionCall) {
        let ExprFunctionCall { name, .. } = call;

        if meta_info.functions.get(name.ident).is_none() {
            let similar = crate::util::similar_name(name.ident, meta_info.rebo_functions.keys());
            let mut diag = diagnostics.error(ErrorCode::UnknownFunction)
                .with_error_label(name.span, "can't find function with this name");
            if let Some(similar) = similar {
                diag = diag.with_info_label(name.span, format!("did you mean `{}`", similar));
            }
            diag.emit();
        }
    }
}