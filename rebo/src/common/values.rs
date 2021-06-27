use std::fmt;

use crate::scope::{Scopes, BindingId};
use crate::common::{SpecificType, FunctionType};
use itertools::Itertools;

pub trait FromValues {
    fn from_values(values: impl Iterator<Item = Value>) -> Self;
    fn types() -> Vec<SpecificType>;
}

impl<T: FromValue> FromValues for T {
    fn from_values(mut values: impl Iterator<Item = Value>) -> Self {
        let val = values.next().unwrap();
        T::from_value(val)
    }
    fn types() -> Vec<SpecificType> {
        vec![T::TYPE]
    }
}

macro_rules! impl_from_values {
    ($last:ident $($name:ident)*) => {
        impl<$last: FromValues, $($name: FromValue,)*> FromValues for ($($name,)* $last,) {
            #[allow(unused_mut)]
            fn from_values(mut values: impl Iterator<Item = Value>) -> Self {
                ($($name::from_value(values.next().unwrap()),)* $last::from_values(values),)
            }
            fn types() -> Vec<SpecificType> {
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
    const TYPE: SpecificType;
    fn from_value(value: Value) -> Self;
}

pub trait IntoValue {
    const TYPE: SpecificType;
    fn into_value(self) -> Value;
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Function(FunctionImpl),
}

impl Value {
    pub fn expect_bool(self, msg: &'static str) -> bool {
        match self {
            Value::Bool(b) => b,
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_string(self, msg: &'static str) -> String {
        match self {
            Value::String(s) => s,
            _ => panic!("{}", msg),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub typ: FunctionType,
    pub imp: FunctionImpl,
}

#[derive(Clone)]
pub enum FunctionImpl {
    Rust(fn(&mut Scopes, Vec<Value>) -> Value),
    Rebo(BindingId, Vec<BindingId>),
}
impl fmt::Debug for FunctionImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionImpl::Rust(_) => write!(f, "FunctionImpl::Rust(_)"),
            FunctionImpl::Rebo(id, arg_ids) => write!(f, "FunctionImpl::Rebo({}({}))", id, arg_ids.iter().join(", ")),
        }
    }
}

impl From<&'_ Value> for SpecificType {
    fn from(val: &Value) -> Self {
        match val {
            Value::Unit => SpecificType::Unit,
            Value::Integer(_) => SpecificType::Integer,
            Value::Float(_) => SpecificType::Float,
            Value::Bool(_) => SpecificType::Bool,
            Value::String(_) => SpecificType::String,
            Value::Function(_) => todo!(),
        }
    }
}

macro_rules! impl_from_into {
    ($ty:ty, $name:ident) => {
        impl FromValue for $ty {
            const TYPE: SpecificType = SpecificType::$name;
            fn from_value(value: Value) -> Self {
                match value {
                    Value::$name(val) => val,
                    _ => unreachable!(),
                }
            }
        }
        impl IntoValue for $ty {
            const TYPE: SpecificType = SpecificType::$name;
            fn into_value(self) -> Value {
                Value::$name(self)
            }
        }
    }
}

impl FromValue for () {
    const TYPE: SpecificType = SpecificType::Unit;
    fn from_value(value: Value) -> Self {
        match value {
            Value::Unit => (),
            _ => unreachable!(),
        }
    }
}
impl IntoValue for () {
    const TYPE: SpecificType = SpecificType::Unit;
    fn into_value(self) -> Value {
        Value::Unit
    }
}

impl_from_into!(i64, Integer);
impl_from_into!(f64, Float);
impl_from_into!(bool, Bool);
impl_from_into!(String, String);
// impl_from_into!(Function, Function);
