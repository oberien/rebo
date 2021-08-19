use crate::common::{Value, Function, FunctionImpl, FunctionType, SpecificType, Type, PreInfo};
use crate::scope::Scopes;
use crate as rebo;
use std::borrow::Cow;
use itertools::Itertools;
use diagnostic::Diagnostics;

pub fn add_to_scope(diagnostics: &Diagnostics, pre_info: &mut PreInfo<'_, '_>) {
    pre_info.add_external_function(diagnostics, "print", Function {
        typ: FunctionType {
            args: Cow::Borrowed(&[Type::Varargs]),
            ret: Type::Specific(SpecificType::Unit),
        },
        imp: FunctionImpl::Rust(print),
    });
    pre_info.add_external_function(diagnostics, "add_one", add_one);
    pre_info.add_external_function(diagnostics, "assert", assert);
    pre_info.add_external_function(diagnostics, "panic", Function {
        typ: FunctionType {
            args: Cow::Borrowed(&[Type::Specific(SpecificType::String)]),
            ret: Type::Bottom,
        },
        imp: FunctionImpl::Rust(panic),
    });
}

fn print(_scopes: &mut Scopes, values: Vec<Value>) -> Value {
    let joined = values.iter().map(ToString::to_string).join(", ");
    println!("{}", joined);
    Value::Unit
}

#[rebo::function]
fn add_one(a: i64) -> i64 {
    a + 1
}

#[rebo::function]
fn assert(b: bool) {
    assert!(b);
}
fn panic(_scopes: &mut Scopes, mut values: Vec<Value>) -> Value {
    panic!("{}", values.remove(0).expect_string("panic called with non-string"))
}