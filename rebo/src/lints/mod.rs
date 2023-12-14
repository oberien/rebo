mod visitor;
mod unknown_function;
mod invalid_number_of_arguments;
mod immutable_assign;
mod function_lints;
mod unknown_struct_field_type;
mod unknown_struct;
mod struct_initialization_fields;
mod struct_def;
mod struct_field_assign;
mod unnecessary_parens;
mod ifelse_return_value;
mod match_lints;
mod impl_blocks;
mod enum_def_init_lints;
mod unknown_access;
mod break_continue_return;
mod variable_without_function;
mod closure_capture_primitives;
mod no_yield;

use crate::parser::Expr;
use crate::common::MetaInfo;
use diagnostic::Diagnostics;
use crate::lints::visitor::{Visitor, VisitorDriver};
use crate::lints::unknown_function::UnknownFunction;
use crate::lints::invalid_number_of_arguments::InvalidNumberOfArguments;
use crate::lints::immutable_assign::ImmutableAssign;
use crate::lints::function_lints::FunctionLints;
use crate::lints::unknown_struct_field_type::UnknownStructFieldType;
use crate::lints::unknown_struct::UnknownStruct;
use crate::lints::struct_def::StructDefLints;
use crate::lints::struct_field_assign::StructFieldAssign;
use crate::lints::struct_initialization_fields::StructInitializationFields;
use crate::lints::unnecessary_parens::UnnecessaryParens;
use crate::lints::ifelse_return_value::IfElseReturnValue;
use crate::lints::match_lints::MatchLints;
use crate::lints::impl_blocks::ImplBlockLints;
use crate::lints::enum_def_init_lints::EnumDefInitLints;
use crate::lints::unknown_access::UnknownAccess;
use crate::lints::break_continue_return::BreakContinueReturn;
use crate::lints::variable_without_function::VariableWithoutFunction;
use crate::lints::closure_capture_primitives::ClosureCapturePrimitives;
use crate::ErrorCode;

pub fn lint<'a, 'b, 'i>(diagnostics: &'b Diagnostics<ErrorCode>, meta_info: &'b MetaInfo<'a, 'i>, exprs: &[&'a Expr<'a, 'i>]) {
    let mut visitor_driver = VisitorDriver::new(diagnostics, meta_info);
    visitor_driver.add_visitors(vec![
        &UnknownFunction as &dyn Visitor,
        &InvalidNumberOfArguments,
        &ImmutableAssign,
        &FunctionLints,
        &UnknownStructFieldType,
        &UnknownStruct,
        &StructInitializationFields,
        &StructDefLints,
        &StructFieldAssign,
        &UnnecessaryParens,
        &IfElseReturnValue,
        &MatchLints,
        &ImplBlockLints,
        &EnumDefInitLints,
        &UnknownAccess,
        &BreakContinueReturn,
        &VariableWithoutFunction,
        &ClosureCapturePrimitives,
    ]);

    visitor_driver.visit_exprs(exprs);
}
