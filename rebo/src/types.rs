use std::fmt;

use crate::scope::Scope;

pub trait FromValues {
    fn from_values(values: impl Iterator<Item = Value>) -> Self;
    fn types() -> Vec<Type>;
}

impl<T: FromValue> FromValues for T {
    fn from_values(mut values: impl Iterator<Item = Value>) -> Self {
        let val = values.next().unwrap();
        T::from_value(val)
    }
    fn types() -> Vec<Type> {
        vec![T::TYPE]
    }
}

fn take_generic<T>() -> usize {
    1
}

macro_rules! impl_from_values {
    ($last:ident $($name:ident)*) => {
        impl<$last: FromValues, $($name: FromValue,)*> FromValues for ($($name,)* $last,) {
            fn from_values(mut values: impl Iterator<Item = Value>) -> Self {
                ($($name::from_value(values.next().unwrap()),)* $last::from_values(values),)
            }
            fn types() -> Vec<Type> {
                let mut res = vec![$($name::TYPE,)*];
                res.extend($last::types());
                res
            }
        }
    }
}

impl_from_values!(A);
impl_from_values!(A B);
impl_from_values!(A B C);
impl_from_values!(A B C D);
impl_from_values!(A B C D E);
impl_from_values!(A B C D E F);
impl_from_values!(A B C D E F G);
impl_from_values!(A B C D E F G H);
impl_from_values!(A B C D E F G H I);
impl_from_values!(A B C D E F G H I J);
impl_from_values!(A B C D E F G H I J K);
impl_from_values!(A B C D E F G H I J K L);
impl_from_values!(A B C D E F G H I J K L M);
impl_from_values!(A B C D E F G H I J K L M N);
impl_from_values!(A B C D E F G H I J K L M N O);

pub trait FromValue {
    const TYPE: Type;
    fn from_value(value: Value) -> Self;
}

pub trait IntoValue {
    const TYPE: Type;
    fn into_value(self) -> Value;
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Integer(i64),
    Float(f64),
    String(String),
    Function(Function),
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Integer,
    Float,
    String,
    Function,
    Any,
    Varargs,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arg_types: &'static [Type],
    pub return_type: Type,
    pub typ: FunctionType,
}

#[derive(Clone)]
pub enum FunctionType {
    Rust(fn(&mut Scope, Vec<Value>) -> Value),
}
impl fmt::Debug for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionType::Rust(fun) => write!(f, "FunctionType::Rust(_)"),
        }
    }
}

impl From<&'_ Value> for Type {
    fn from(val: &Value) -> Self {
        match val {
            Value::Unit => Type::Unit,
            Value::Integer(_) => Type::Integer,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::String,
            Value::Function(_) => Type::Function,
        }
    }
}

macro_rules! impl_from_into {
    ($ty:ty, $name:ident) => {
        impl FromValue for $ty {
            const TYPE: Type = Type::$name;
            fn from_value(value: Value) -> Self {
                match value {
                    Value::$name(val) => val,
                    _ => unreachable!(),
                }
            }
        }
        impl IntoValue for $ty {
            const TYPE: Type = Type::$name;
            fn into_value(self) -> Value {
                Value::$name(self)
            }
        }
    }
}

impl FromValue for () {
    const TYPE: Type = Type::Unit;
    fn from_value(value: Value) -> Self {
        match value {
            Value::Unit => (),
            _ => unreachable!(),
        }
    }
}
impl IntoValue for () {
    const TYPE: Type = Type::Unit;
    fn into_value(self) -> Value {
        Value::Unit
    }
}

impl_from_into!(i64, Integer);
impl_from_into!(f64, Float);
impl_from_into!(String, String);
impl_from_into!(Function, Function);
