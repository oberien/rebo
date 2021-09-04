use std::fmt;

use crate::scope::{Scopes, BindingId};
use crate::common::{SpecificType, FunctionType};
use itertools::Itertools;
use std::sync::Arc;
use std::fmt::{Display, Formatter, Debug};
use std::ops::{Add, Sub, Mul, Div};
use std::cmp::Ordering;
use parking_lot::ReentrantMutex;
use std::cell::RefCell;
use crate::parser::{ExprLiteral, ExprInteger, ExprFloat, ExprBool, ExprString};

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

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum Value {
    Unit,
    Integer(i64),
    Float(FuzzyFloat),
    Bool(bool),
    String(String),
    Struct(StructArc),
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

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => Debug::fmt(&(), f),
            Value::Integer(i) => Display::fmt(i, f),
            Value::Float(float) => Display::fmt(float, f),
            Value::Bool(b) => Display::fmt(b, f),
            Value::String(s) => Display::fmt(s, f),
            Value::Struct(s) => {
                let s = s.s.lock();
                let s = s.borrow();
                write!(f, "{} {{", s.name)?;
                for (field, value) in &s.fields {
                    write!(f, " {}: {},", field, value)?;
                }
                write!(f, " }}")
            },
        }
    }
}

impl PartialEq<ExprLiteral> for Value {
    fn eq(&self, other: &ExprLiteral) -> bool {
        match (self, other) {
            (Value::Unit, ExprLiteral::Unit(_)) => true,
            (Value::Integer(i), ExprLiteral::Integer(ExprInteger { int })) => *i == int.value,
            (Value::Float(f), ExprLiteral::Float(ExprFloat { float })) => *f == FuzzyFloat(float.value),
            (Value::Bool(val), ExprLiteral::Bool(ExprBool { b })) => *val == b.value,
            (Value::String(val), ExprLiteral::String(ExprString { string })) => val == &string.string,
            _ => false,
        }
    }
}

/// Float with fuzzy equality
#[derive(Debug, Clone, PartialOrd)]
pub struct FuzzyFloat(pub f64);

impl PartialEq for FuzzyFloat {
    fn eq(&self, other: &Self) -> bool {
        let a = self.0;
        let b = other.0;
        // https://stackoverflow.com/a/4915891
        let epsilon = 1e-10;
        let abs_a = a.abs();
        let abs_b = b.abs();
        let diff = (a - b).abs();
        #[allow(clippy::float_cmp)]
        if a == b { // shortcut, handles infinities
            true
        } else if a == 0. || b == 0. || diff < f64::MIN_POSITIVE {
            // a or b is zero or both are extremely close to it
            // relative error is less meaningful here
            diff < (epsilon * f64::MIN_POSITIVE)
        } else { // use relative error
            diff / (abs_a + abs_b) < epsilon
        }
    }
}
impl Add<FuzzyFloat> for FuzzyFloat {
    type Output = FuzzyFloat;
    fn add(self, rhs: FuzzyFloat) -> Self::Output { FuzzyFloat(self.0 + rhs.0) }
}
impl Sub<FuzzyFloat> for FuzzyFloat {
    type Output = FuzzyFloat;
    fn sub(self, rhs: FuzzyFloat) -> Self::Output { FuzzyFloat(self.0 - rhs.0) }
}
impl Mul<FuzzyFloat> for FuzzyFloat {
    type Output = FuzzyFloat;
    fn mul(self, rhs: FuzzyFloat) -> Self::Output { FuzzyFloat(self.0 * rhs.0) }
}
impl Div<FuzzyFloat> for FuzzyFloat {
    type Output = FuzzyFloat;
    fn div(self, rhs: FuzzyFloat) -> Self::Output { FuzzyFloat(self.0 / rhs.0) }
}
impl Display for FuzzyFloat {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct Function {
    pub typ: FunctionType,
    pub imp: FunctionImpl,
}

pub type RustFunction = fn(&mut Scopes, Vec<Value>) -> Value;
#[derive(Clone)]
pub enum FunctionImpl {
    Rust(RustFunction),
    Rebo(String, Vec<BindingId>),
}
impl fmt::Debug for FunctionImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionImpl::Rust(_) => write!(f, "FunctionImpl::Rust(_)"),
            FunctionImpl::Rebo(id, arg_ids) => write!(f, "FunctionImpl::Rebo({}({}))", id, arg_ids.iter().join(", ")),
        }
    }
}
impl PartialOrd for FunctionImpl {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // TODO: make rust functions comparable
        match (self, other) {
            (FunctionImpl::Rust(_), _) => None,
            (_, FunctionImpl::Rust(_)) => None,
            (FunctionImpl::Rebo(a, _), FunctionImpl::Rebo(b, _)) => Some(a.cmp(b)),
        }
    }
}
impl PartialEq for FunctionImpl {
    fn eq(&self, other: &Self) -> bool {
        // TODO: make rust functions comparable
        match (self, other) {
            (FunctionImpl::Rust(_), _) => false,
            (_, FunctionImpl::Rust(_)) => false,
            (FunctionImpl::Rebo(a, _), FunctionImpl::Rebo(b, _)) => a.eq(b),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructArc {
    pub s: Arc<ReentrantMutex<RefCell<Struct>>>,
}
impl PartialOrd for StructArc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.s.lock().borrow().partial_cmp(&other.s.lock().borrow())
    }
}
impl PartialEq for StructArc {
    fn eq(&self, other: &Self) -> bool {
        self.s.lock().borrow().eq(&other.s.lock().borrow())
    }
}
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, Value)>,
}

// impl From<&'_ Value> for SpecificType {
//     fn from(val: &Value) -> Self {
//         match val {
//             Value::Unit => SpecificType::Unit,
//             Value::Integer(_) => SpecificType::Integer,
//             Value::Float(_) => SpecificType::Float,
//             Value::Bool(_) => SpecificType::Bool,
//             Value::String(_) => SpecificType::String,
//             Value::Struct(s) => SpecificType::Struct(s.s.lock().borrow().name.clone()),
//         }
//     }
// }

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
impl_from_into!(FuzzyFloat, Float);
impl_from_into!(bool, Bool);
impl_from_into!(String, String);
