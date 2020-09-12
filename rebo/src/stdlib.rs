use crate::types::{Value, Function, Type, FunctionType};
use crate::scope::{RootScope, Scopes};
use crate as rebo;

pub fn add_to_root_scope(scope: &mut RootScope) {
    scope.add_function("print", Function {
        arg_types: &[Type::Varargs],
        return_type: Type::Unit,
        typ: FunctionType::Rust(print)
    });
    scope.add_function("add_one", add_one);
}

fn print(_scopes: &mut Scopes, values: Vec<Value>) -> Value {
    for val in values {
        match val {
            Value::Unit => print!("{:<8?}", ()),
            Value::Integer(i) => print!("{:<8}", i),
            Value::Float(i) => print!("{:<8}", i),
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