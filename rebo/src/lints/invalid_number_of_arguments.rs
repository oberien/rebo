use crate::lints::visitor::Visitor;
use crate::parser::ExprFunctionCall;
use crate::common::{MetaInfo, Function, Type};
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;

pub struct InvalidNumberOfArguments;

impl Visitor for InvalidNumberOfArguments {
    fn visit_function_call(&self, diagnostics: &Diagnostics, meta_info: &MetaInfo, call: &ExprFunctionCall) {
        let ExprFunctionCall { name, open, args, close } = call;

        let args_span = args.span().unwrap_or_else(|| Span::new(open.span.file, open.span.start, close.span.end));
        let fun = match &meta_info.functions.get(name.ident) {
            Some(Function { typ, imp: _ }) => typ,
            _ => return,
        };

        let varargs = matches!(fun.args.last(), Some(&Type::Varargs(_)));
        let correct_number_of_args = args.len() == fun.args.len()
            || varargs && args.len() >= fun.args.len() - 1;
        if !correct_number_of_args {
            let expected_str = if varargs { "at least " } else { "" };
            let expected = if varargs { fun.args.len() - 1 } else { fun.args.len() };
            diagnostics.error(ErrorCode::InvalidNumberOfArguments)
                .with_error_label(args_span, format!("found {} arguments", args.len()))
                .with_info_label(name.span, format!("expected {}{} arguments", expected_str, expected))
                .emit();
        }
    }
}
