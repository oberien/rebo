use crate::common::{Value, MetaInfo, ExternalFunction, FuzzyFloat};
use crate::vm::VmContext;
use crate as rebo;
use std::borrow::Cow;
use itertools::Itertools;
use diagnostic::{Diagnostics, Span};
use crate::typeck::types::{FunctionType, Type, SpecificType};
use crate::error_codes::ErrorCode;
use crate::parser::Expr;
use typed_arena::Arena;
use crate::ExecError;

mod list;
mod option;
mod result;
mod map;

bitflags::bitflags! {
    pub struct Stdlib: u64 {
        const PRINT = 0x1;
        const ASSERT = 0x2;
        const PANIC = 0x4;
    }
}

pub fn add_to_meta_info<'a, 'i>(stdlib: Stdlib, diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    let option_t = option::add_option(diagnostics, arena, meta_info);
    result::add_result(diagnostics, arena, meta_info);
    let list_t = list::add_list(diagnostics, arena, meta_info, option_t);
    map::add_map(diagnostics, arena, meta_info, option_t, list_t);

    if stdlib.contains(Stdlib::PRINT) {
        meta_info.add_external_function(diagnostics, "print", ExternalFunction {
            typ: FunctionType {
                generics: Cow::Borrowed(&[]),
                args: Cow::Borrowed(&[Type::UntypedVarargs]),
                ret: Type::Specific(SpecificType::Unit),
            },
            imp: print,
        });
    }

    meta_info.add_external_function(diagnostics, "add_one", add_one);

    meta_info.add_external_function(diagnostics, "int::to_float", int_to_float);
    meta_info.add_external_function(diagnostics, "float::to_int", float_to_int);
    meta_info.add_external_function(diagnostics, "bool::to_int", bool_to_int);

    if stdlib.contains(Stdlib::ASSERT) {
        meta_info.add_external_function(diagnostics, "assert", ExternalFunction {
            typ: FunctionType {
                generics: Cow::Borrowed(&[]),
                args: Cow::Borrowed(&[Type::Specific(SpecificType::Bool)]),
                ret: Type::Specific(SpecificType::Unit),
            },
            imp: assert,
        });
    }
    if stdlib.contains(Stdlib::PANIC) {
        meta_info.add_external_function(diagnostics, "panic", ExternalFunction {
            typ: FunctionType {
                generics: Cow::Borrowed(&[]),
                args: Cow::Borrowed(&[Type::Specific(SpecificType::String)]),
                ret: Type::Bottom,
            },
            imp: panic,
        });
    }
}

fn print(_expr_span: Span, _vm: &mut VmContext, values: Vec<Value>) -> Result<Value, ExecError> {
    let joined = values.iter().map(ToString::to_string).join(", ");
    println!("{}", joined);
    Ok(Value::Unit)
}

#[rebo::function]
fn add_one(a: i64) -> i64 {
    a + 1
}

// type conversions
#[rebo::function]
fn int_to_float(i: i64) -> FuzzyFloat {
    FuzzyFloat(i as f64)
}
#[rebo::function]
fn bool_to_int(b: bool) -> i64 {
    b as i64
}
#[rebo::function]
fn float_to_int(f: FuzzyFloat) -> i64 {
    f.0 as i64
}

fn assert(expr_span: Span, vm: &mut VmContext, mut values: Vec<Value>) -> Result<Value, ExecError> {
    if !values.remove(0).expect_bool("assert called with non-bool") {
        vm.diagnostics().error(ErrorCode::AssertionFailed)
            .with_error_label(expr_span, "this assertion failed")
            .emit();
        Err(ExecError::Panic)
    } else {
        Ok(Value::Unit)
    }
}
fn panic(expr_span: Span, vm: &mut VmContext, mut values: Vec<Value>) -> Result<Value, ExecError> {
    vm.diagnostics().error(ErrorCode::Panic)
        .with_error_label(expr_span, values.remove(0).expect_string("panic called with non-string"))
        .emit();
    Err(ExecError::Panic)
}