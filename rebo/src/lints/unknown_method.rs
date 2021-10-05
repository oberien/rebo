use crate::lints::visitor::Visitor;
use crate::parser::{ExprMethodCall, Spanned};
use crate::common::MetaInfo;
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::typeck::types::{Type, SpecificType};
use crate::typeck::TypeVar;

pub struct UnknownMethod;

impl Visitor for UnknownMethod {
    fn visit_method_call(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, call: &ExprMethodCall) {
        let ExprMethodCall { variable, fields, fn_call, .. } = call;

        let field_access_type_var = if fields.is_empty() {
            TypeVar::new(variable.binding.ident.span)
        } else {
            TypeVar::new(Span::new(variable.span().file, variable.span().start, fields.last().unwrap().0.span().end))
        };
        let target_typ = &meta_info.types[&field_access_type_var];

        let target_typ = match target_typ {
            Type::Specific(specific) => specific,
            _ => return,
        };

        let fn_name = format!("{}::{}", target_typ.type_name(), fn_call.name.ident);

        if meta_info.functions.get(fn_name.as_str()).is_none() {
            let similar = crate::util::similar_name(&fn_name, meta_info.rebo_functions.keys());
            let mut diag = diagnostics.error(ErrorCode::UnknownMethod)
                .with_error_label(fn_call.name.span, format!("can't find method `{}`", fn_name));
            if let Some(similar) = similar {
                diag = diag.with_info_label(fn_call.name.span, format!("did you mean `{}`", similar));
            }
            diag.emit();
        }
    }
}