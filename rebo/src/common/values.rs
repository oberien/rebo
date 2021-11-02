use std::fmt;

use crate::vm::VmContext;
use crate::parser::BindingId;
use std::sync::Arc;
use std::fmt::{Display, Formatter, Debug};
use std::ops::{Add, Sub, Mul, Div};
use std::cmp::Ordering;
use parking_lot::ReentrantMutex;
use std::cell::RefCell;
use crate::parser::{ExprLiteral, ExprInteger, ExprFloat, ExprBool, ExprString};
use diagnostic::Span;
use crate::EXTERNAL_SPAN;
use itertools::Itertools;
use crate::typeck::types::{SpecificType, FunctionType};
use crate::common::MetaInfo;
use crate::parser::Spanned;

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
    Enum(EnumArc),
    List(ListArc),
}

impl Value {
    pub fn expect_bool(self, msg: &'static str) -> bool {
        match self {
            Value::Bool(b) => b,
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_int(self, msg: &'static str) -> i64 {
        match self {
            Value::Integer(i) => i,
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_string(self, msg: &'static str) -> String {
        match self {
            Value::String(s) => s,
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_list(self, msg: &'static str) -> ListArc {
        match self {
            Value::List(l) => l,
            _ => panic!("{}", msg),
        }
    }
    pub fn type_name(&self) -> String {
        match self {
            Value::Unit => "unit".to_string(),
            Value::Integer(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::String(_) => "string".to_string(),
            Value::Struct(s) => s.s.lock().borrow().name.clone(),
            Value::Enum(e) => e.e.lock().borrow().name.clone(),
            Value::List(_) => "List".to_string(),
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
            Value::String(s) => Debug::fmt(s, f),
            Value::Struct(s) => {
                let s = s.s.lock();
                let s = s.borrow();
                write!(f, "{} {{", s.name)?;
                for (field, value) in &s.fields {
                    write!(f, " {}: {},", field, value)?;
                }
                write!(f, " }}")
            },
            Value::Enum(e) => {
                let e = e.e.lock();
                let e = e.borrow();
                write!(f, "{}::{}", e.name, e.variant)?;
                if !e.fields.is_empty() {
                    write!(f, "({})", e.fields.iter().join(", "))?;
                }
                Ok(())
            }
            Value::List(l) => {
                let l = l.list.lock();
                let l = l.borrow();
                write!(f, "[{}]", l.iter().join(", "))
            }
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
        Debug::fmt(&self.0, f)
    }
}

pub struct ExternalFunction {
    pub typ: FunctionType,
    pub imp: RustFunction,
}

pub type RustFunction = fn(expr_span: Span, &mut VmContext, Vec<Value>) -> Value;
#[derive(Clone)]
pub enum Function {
    Rust(RustFunction),
    /// function name, argument binding ids, definition span
    Rebo(String, Vec<BindingId>),
    /// enum name, variant name, variant definition span
    EnumInitializer(String, String)
}
impl Function {
    pub fn span(&self, meta_info: &MetaInfo) -> Span {
        match self {
            Function::Rebo(name, _args) => meta_info.rebo_functions[name.as_str()].span(),
            Function::Rust(_) => EXTERNAL_SPAN.lock().unwrap().unwrap(),
            Function::EnumInitializer(enum_name, variant_name) => meta_info.user_types[enum_name.as_str()].variant_initializer_span(variant_name).unwrap(),
        }
    }
}
impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Rust(_) => write!(f, "FunctionImpl::Rust(_)"),
            Function::Rebo(id, arg_ids) => write!(f, "FunctionImpl::Rebo({}({}))", id, arg_ids.iter().join(", ")),
            Function::EnumInitializer(enum_name, variant_name) => write!(f, "FunctionImpl::EnumInitializer({}::{})", enum_name, variant_name),
        }
    }
}
impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // TODO: make rust functions comparable
        match (self, other) {
            (Function::Rust(_), _) => None,
            (_, Function::Rust(_)) => None,
            (Function::Rebo(a, _), Function::Rebo(b, _)) => a.partial_cmp(b),
            (Function::EnumInitializer(enum1, variant1), Function::EnumInitializer(enum2, variant2)) => (enum1, variant1).partial_cmp(&(enum2, variant2)),
            (Function::EnumInitializer(enum_name, variant), Function::Rebo(name, _)) => format!("{}::{}", enum_name, variant).partial_cmp(name),
            (Function::Rebo(name, _), Function::EnumInitializer(enum_name, variant)) => name.partial_cmp(&format!("{}::{}", enum_name, variant)),
        }
    }
}
impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        // TODO: make rust functions comparable
        match (self, other) {
            (Function::Rust(_), _) => false,
            (_, Function::Rust(_)) => false,
            (Function::Rebo(a, _), Function::Rebo(b, _)) => a.eq(b),
            (Function::EnumInitializer(enum1, variant1), Function::EnumInitializer(enum2, variant2)) => (enum1, variant1).eq(&(enum2, variant2)),
            (Function::EnumInitializer(enum_name, variant), Function::Rebo(name, _))
            | (Function::Rebo(name, _), Function::EnumInitializer(enum_name, variant)) => {
                format!("{}::{}", enum_name, variant).eq(name)
            }
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

#[derive(Debug, Clone)]
pub struct EnumArc {
    pub e: Arc<ReentrantMutex<RefCell<Enum>>>,
}
impl PartialOrd for EnumArc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.e.lock().borrow().partial_cmp(&other.e.lock().borrow())
    }
}
impl PartialEq for EnumArc {
    fn eq(&self, other: &Self) -> bool {
        self.e.lock().borrow().eq(&other.e.lock().borrow())
    }
}
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct Enum {
    pub name: String,
    pub variant: String,
    pub fields: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct ListArc {
    pub list: Arc<ReentrantMutex<RefCell<Vec<Value>>>>,
}
impl PartialOrd for ListArc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.list.lock().borrow().partial_cmp(&other.list.lock().borrow())
    }
}
impl PartialEq for ListArc {
    fn eq(&self, other: &Self) -> bool {
        self.list.lock().borrow().eq(&*other.list.lock().borrow())
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
impl_from_into!(FuzzyFloat, Float);
impl_from_into!(bool, Bool);
impl_from_into!(String, String);
