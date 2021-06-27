use crate::common::{Value, Function, FunctionImpl, FunctionType, SpecificType, Type, PreTypeInfo};
use crate::scope::Scopes;
use crate as rebo;
use std::borrow::Cow;

pub fn add_to_scope(pre_info: &mut PreTypeInfo<'_, '_>) {
    pre_info.bindings.extend([pre_info.root_scope.add_external_function("print", Function {
        typ: FunctionType {
            args: Cow::Borrowed(&[Type::Varargs]),
            ret: Type::Specific(SpecificType::Unit),
        },
        imp: FunctionImpl::Rust(print),
    })]);
    pre_info.bindings.extend([pre_info.root_scope.add_external_function("add_one", add_one)]);
    pre_info.bindings.extend([pre_info.root_scope.add_external_function("assert", assert)]);
    pre_info.bindings.extend([pre_info.root_scope.add_external_function("panic", Function {
        typ: FunctionType {
            args: Cow::Borrowed(&[Type::Specific(SpecificType::String)]),
            ret: Type::Bottom,
        },
        imp: FunctionImpl::Rust(panic),
    })]);
}

fn print(_scopes: &mut Scopes, values: Vec<Value>) -> Value {
    for val in values {
        match val {
            Value::Unit => print!("{:<8?}", ()),
            Value::Integer(i) => print!("{:<8}", i),
            Value::Float(i) => print!("{:<8}", i),
            Value::Bool(b) => print!("{:<8}", b),
            Value::String(s) => print!("{:<8?}", s),
            Value::Function(_f) => todo!("function print representation"),
        }
    }
    println!();
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