use std::collections::HashMap;
use crate::common::{Value, Function, FunctionImpl, FunctionType, SpecificType, Type, PreTypeInfo};
use crate::scope::{Scopes, Scope};
use crate as rebo;
use crate::parser::Binding;

pub fn add_to_scope(scope: &mut Scope) -> PreTypeInfo<'static> {
    let mut bindings = HashMap::new();
    bindings.extend([scope.add_external_function("print", Function {
        typ: FunctionType {
            args: &[Type::Varargs],
            ret: Type::Specific(SpecificType::Unit),
        },
        imp: FunctionImpl::Rust(print),
    })]);
    bindings.extend([scope.add_external_function("add_one", add_one)]);
    bindings.extend([scope.add_external_function("assert", assert)]);
    bindings.extend([scope.add_external_function("panic", Function {
        typ: FunctionType {
            args: &[Type::Specific(SpecificType::String)],
            ret: Type::Bottom,
        },
        imp: FunctionImpl::Rust(panic),
    })]);
    PreTypeInfo {
        bindings,
    }
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