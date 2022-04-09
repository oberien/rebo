use crate::lints::visitor::Visitor;
use crate::parser::{ExprFunctionCall, Separated, Spanned};
use crate::common::{MetaInfo, BlockStack};
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::{Expr, FunctionType, SpecificType};
use crate::lexer::{TokenCloseParen, TokenComma, TokenOpenParen};
use crate::typeck::types::Type;
use crate::typeck::TypeVar;

pub struct InvalidNumberOfArguments;

impl Visitor for InvalidNumberOfArguments {
    fn visit_function_call(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, call: &ExprFunctionCall) {
        let ExprFunctionCall { name, open, args, close } = call;

        match &meta_info.types[&TypeVar::new(name.binding.ident.span)] {
            Type::Specific(SpecificType::Function(fun)) => check_function_call_arg_num(diagnostics, fun, CallType::FunctionCall, name.span(), open, args, close),
            _ => (),
        }
    }
}

pub enum CallType {
    FunctionCall,
    MethodCall,
}

pub fn check_function_call_arg_num(diagnostics: &Diagnostics<ErrorCode>, fun: &FunctionType, call_type: CallType, name_span: Span, open: &TokenOpenParen, args: &Separated<&Expr, TokenComma>, close: &TokenCloseParen) {
    let args_span = args.span().unwrap_or_else(|| Span::new(open.span.file, open.span.start, close.span.end));

    let actual = match call_type {
        CallType::FunctionCall => args.len(),
        CallType::MethodCall => args.len() + 1,
    };

    let varargs = matches!(fun.args.last(), Some(&Type::UntypedVarargs | &Type::TypedVarargs(_)));
    let correct_number_of_args = actual == fun.args.len()
        || varargs && actual >= fun.args.len() - 1;

    if !correct_number_of_args {
        let expected_str = if varargs { "at least " } else { "" };
        let expected = if varargs { fun.args.len() - 1 } else { fun.args.len() };
        let expected = match call_type {
            CallType::FunctionCall => expected,
            CallType::MethodCall => expected.saturating_sub(1),
        };
        diagnostics.error(ErrorCode::InvalidNumberOfArguments)
            .with_error_label(args_span, format!("found {} arguments", actual))
            .with_info_label(name_span, format!("expected {}{} arguments", expected_str, expected))
            .emit();
    }
}
