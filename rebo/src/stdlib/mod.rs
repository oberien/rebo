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

mod list;

pub fn add_to_meta_info<'a, 'i>(diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    list::add_list(diagnostics, arena, meta_info);

    meta_info.add_external_function(diagnostics, "print", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Borrowed(&[]),
            args: Cow::Borrowed(&[Type::Varargs]),
            ret: Type::Specific(SpecificType::Unit),
        },
        imp: print,
    });
    meta_info.add_external_function(diagnostics, "add_one", add_one);

    meta_info.add_external_function(diagnostics, "int::to_float", int_to_float);
    meta_info.add_external_function(diagnostics, "float::to_int", float_to_int);
    meta_info.add_external_function(diagnostics, "bool::to_int", bool_to_int);

    meta_info.add_external_function(diagnostics, "assert", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Borrowed(&[]),
            args: Cow::Borrowed(&[Type::Specific(SpecificType::Bool)]),
            ret: Type::Specific(SpecificType::Unit),
        },
        imp: assert,
    });
    meta_info.add_external_function(diagnostics, "panic", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Borrowed(&[]),
            args: Cow::Borrowed(&[Type::Specific(SpecificType::String)]),
            ret: Type::Bottom,
        },
        imp: panic,
    });
}

fn print(_expr_span: Span, _vm: &mut VmContext, values: Vec<Value>) -> Value {
    let joined = values.iter().map(ToString::to_string).join(", ");
    println!("{}", joined);
    Value::Unit
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

fn assert(expr_span: Span, vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    if !values.remove(0).expect_bool("assert called with non-bool") {
        vm.diagnostics().error(ErrorCode::AssertionFailed)
            .with_error_label(expr_span, "this assertion failed")
            .emit();
        panic!("assertion error in rebo")
    } else {
        Value::Unit
    }
}
fn panic(expr_span: Span, vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    vm.diagnostics().error(ErrorCode::Panic)
        .with_error_label(expr_span, values.remove(0).expect_string("panic called with non-string"))
        .emit();
    panic!("explicit panic in rebo")
}