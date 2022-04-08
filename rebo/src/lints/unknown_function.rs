use crate::lints::visitor::Visitor;
use crate::parser::{ExprFunctionCall, ExprVariable, Spanned};
use crate::common::{MetaInfo, BlockStack};
use diagnostic::Diagnostics;
use crate::error_codes::ErrorCode;
use crate::{SpecificType, Type};
use crate::typeck::TypeVar;

pub struct UnknownFunction;

impl Visitor for UnknownFunction {
    fn visit_variable(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, var: &ExprVariable) {
        let ExprVariable { binding, .. } = var;
        if binding.ident.ident.contains("::") {
            match meta_info.functions.get(binding.ident.ident) {
                Some(_) => (),
                None => {
                    let similar = crate::util::similar_name(binding.ident.ident, meta_info.functions.keys());
                    let mut diag = diagnostics.error(ErrorCode::UnknownFunction)
                        .with_error_label(var.span(), "can't find associated function or method with this name");
                    if let Some(similar) = similar {
                        diag = diag.with_info_label(var.span(), format!("did you mean `{}`", similar));
                    }
                    diag.emit();
                }
            }
        }
    }
    fn visit_function_call(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, call: &ExprFunctionCall) {
        let ExprFunctionCall { name, .. } = call;

        match &meta_info.types[&TypeVar::new(name.binding.ident.span)] {
            Type::Specific(SpecificType::Function(_)) => (),
            _ => {
                let similar = crate::util::similar_name(name.binding.ident.ident, meta_info.functions.keys());
                let mut diag = diagnostics.error(ErrorCode::UnknownFunction)
                    .with_error_label(name.span(), "can't find function with this name");
                if let Some(similar) = similar {
                    diag = diag.with_info_label(name.span(), format!("did you mean `{}`", similar));
                }
                diag.emit();
            }
        }
    }
}