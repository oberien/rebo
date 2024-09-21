use crate::lints::visitor::Visitor;
use crate::parser::{ExprFunctionCall, Separated};
use crate::common::{BlockStack, MetaInfo};
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::{Expr, FunctionType, SpecificType};
use crate::common::Spanned;
use crate::lexer::{TokenCloseParen, TokenComma, TokenOpenParen};
use crate::typeck::types::Type;
use crate::typeck::TypeVar;

pub struct InvalidNumberOfArguments;

impl Visitor for InvalidNumberOfArguments {
    fn visit_function_call(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, '_, ()>, call: &ExprFunctionCall) {
        let ExprFunctionCall { name, open, args, close, .. } = call;

        if let Type::Specific(SpecificType::Function(fun)) = &meta_info.types[&TypeVar::from_spanned(name)] {
            check_function_call_arg_num(diagnostics, fun, CallType::FunctionCall, name.span_(), open, args, close)
        }
    }
}

pub enum CallType {
    FunctionCall,
    MethodCall,
}

pub fn check_function_call_arg_num(diagnostics: &Diagnostics<ErrorCode>, fun: &FunctionType, call_type: CallType, name_span: Span, open: &TokenOpenParen, args: &Separated<&Expr, TokenComma>, close: &TokenCloseParen) {
    let args_span = args.span().unwrap_or_else(|| Span::new(open.span_().file, open.span_().start, close.span_().end));

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
