use crate::common::{Value, MetaInfo, ExternalFunction};
use crate::scope::Scopes;
use crate as rebo;
use std::borrow::Cow;
use itertools::Itertools;
use diagnostic::Diagnostics;
use crate::typeck::types::{FunctionType, Type, SpecificType};

pub fn add_to_scope(diagnostics: &Diagnostics, meta_info: &mut MetaInfo<'_, '_>) {
    meta_info.add_external_function(diagnostics, "print", ExternalFunction {
        typ: FunctionType {
            args: Cow::Borrowed(&[Type::Varargs]),
            ret: Type::Specific(SpecificType::Unit),
        },
        imp: print,
    });
    meta_info.add_external_function(diagnostics, "add_one", add_one);
    meta_info.add_external_function(diagnostics, "assert", assert);
    meta_info.add_external_function(diagnostics, "panic", ExternalFunction {
        typ: FunctionType {
            args: Cow::Borrowed(&[Type::Specific(SpecificType::String)]),
            ret: Type::Bottom,
        },
        imp: panic,
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