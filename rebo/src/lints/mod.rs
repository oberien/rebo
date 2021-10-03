mod visitor;
mod unknown_function;
mod invalid_number_of_arguments;
mod immutable_assign;
mod empty_function_body;
mod unknown_struct_field_type;
mod unknown_struct;
mod struct_initialization_fields;
mod recursive_struct;
mod struct_field_access;
mod unnecessary_parens;
mod ifelse_return_value;
mod match_lints;
mod unknown_impl_block_target;
mod enum_init_is_clike;

use crate::parser::Expr;
use crate::common::MetaInfo;
use diagnostic::Diagnostics;
use crate::lints::visitor::{Visitor, VisitorDriver};
use crate::lints::unknown_function::UnknownFunction;
use crate::lints::invalid_number_of_arguments::InvalidNumberOfArguments;
use crate::lints::immutable_assign::ImmutableAssign;
use crate::lints::empty_function_body::EmptyFunctionBody;
use crate::lints::unknown_struct_field_type::UnknownStructFieldType;
use crate::lints::unknown_struct::UnknownStruct;
use crate::lints::recursive_struct::RecursiveStruct;
use crate::lints::struct_field_access::StructFieldAccess;
use crate::lints::struct_initialization_fields::StructInitializationFields;
use crate::lints::unnecessary_parens::UnnecessaryParens;
use crate::lints::ifelse_return_value::IfElseReturnValue;
use crate::lints::match_lints::MatchLints;
use crate::lints::unknown_impl_block_target::UnknownImplBlockTarget;
use crate::lints::enum_init_is_clike::EnumInitIsClike;

pub fn lint<'a, 'b, 'i>(diagnostics: &'b Diagnostics, meta_info: &'b MetaInfo<'a, 'i>, exprs: &[&'a Expr<'a, 'i>]) {
    let mut visitor_driver = VisitorDriver::new(diagnostics, meta_info);
    visitor_driver.add_visitors(vec![
        &UnknownFunction as &dyn Visitor,
        &InvalidNumberOfArguments,
        &ImmutableAssign,
        &EmptyFunctionBody,
        &UnknownStructFieldType,
        &UnknownStruct,
        &StructInitializationFields,
        &RecursiveStruct,
        &StructFieldAccess,
        &UnnecessaryParens,
        &IfElseReturnValue,
        &MatchLints,
        &UnknownImplBlockTarget,
        &EnumInitIsClike,
    ]);

    visitor_driver.visit_exprs(exprs);
}
