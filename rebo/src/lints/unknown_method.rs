use crate::lints::visitor::Visitor;
use crate::parser::{Spanned, ExprAccess};
use crate::common::MetaInfo;
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::typeck::types::Type;
use crate::typeck::TypeVar;

pub struct UnknownMethod;

impl Visitor for UnknownMethod {
    fn visit_access(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, call: &ExprAccess) {
        todo!()
        // let ExprMethodCall { variable, fields, fn_call, .. } = call;
        //
        // let field_access_type_var = if fields.is_empty() {
        //     TypeVar::new(variable.binding.ident.span)
        // } else {
        //     TypeVar::new(Span::new(variable.span().file, variable.span().start, fields.last().unwrap().0.span().end))
        // };
        // let target_typ = &meta_info.types[&field_access_type_var];
        //
        // let target_typ = match target_typ {
        //     Type::Specific(specific) => specific,
        //     _ => return,
        // };
        //
        // let fn_name = format!("{}::{}", target_typ.type_name(), fn_call.name.ident);
        //
        // if meta_info.functions.get(fn_name.as_str()).is_none() {
        //     let similar = crate::util::similar_name(&fn_name, meta_info.rebo_functions.keys());
        //     let mut diag = diagnostics.error(ErrorCode::UnknownMethod)
        //         .with_error_label(fn_call.name.span, format!("can't find method `{}`", fn_name));
        //     if let Some(similar) = similar {
        //         diag = diag.with_info_label(fn_call.name.span, format!("did you mean `{}`", similar));
        //     }
        //     diag.emit();
        //     return;
        // }
        //
        // let fun = &meta_info.rebo_functions[fn_name.as_str()];
        // if fun.self_arg.is_none() {
        //     diagnostics.error(ErrorCode::NotAMethod)
        //         .with_error_label(fn_call.name.span, format!("`{}` is a function and not a method", fn_name))
        //         .with_info_label(fn_call.name.span, "methods must have `self` as first argument")
        //         .with_info_label(fun.arg_span(), "this function doesn't have `self` as first argument")
        //         .emit();
        // }
    }
}