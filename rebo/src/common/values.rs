use std::borrow::Cow;
use crate::vm::{ExecError, VmContext};
use crate::parser::BindingId;
use std::sync::Arc;
use std::hash::Hash;
use std::fmt;
use std::ops::{Add, Sub, Mul, Div};
use std::cmp::Ordering;
use parking_lot::ReentrantMutex;
use std::cell::RefCell;
use crate::parser::{ExprLiteral, ExprInteger, ExprFloat, ExprBool, ExprString};
use diagnostic::Span;
use crate::Type;
use itertools::Itertools;
use crate::typeck::types::SpecificType;
use std::collections::{BTreeMap, BTreeSet};
use std::convert::{Infallible, TryInto};
use std::iter::FromIterator;
use std::marker::PhantomData;
use rt_format::{FormatArgument, Specifier};
use rebo::common::FunctionValue::{Anonymous, Named};
use rebo::common::{SpanWithId, Spanned};
use rebo::FunctionType;
use rebo::vm::Scope;
use std::sync::OnceLock;

pub trait ExternalTypeType {
    type Type: ExternalType;
}
pub trait ExternalType: FromValue + IntoValue + Typed {
    const CODE: &'static str;
    const FILE_NAME: &'static str;
}
pub trait FromValue {
    fn from_value(value: Value) -> Self;
}
pub trait IntoValue {
    fn into_value(self) -> Value;
}
pub trait Typed {
    fn typ() -> SpecificType;
}

pub trait DeepCopy {
    fn deep_copy(&self) -> Self;
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Value {
    Unit,
    Integer(i64),
    Float(FuzzyFloat),
    Bool(bool),
    String(String),
    Struct(StructArc),
    Enum(EnumArc),
    List(ListArc),
    Map(MapArc),
    Set(SetArc),
    Function(FunctionValue),
}
impl DeepCopy for i64 {
    fn deep_copy(&self) -> Self {
        *self
    }
}
impl DeepCopy for bool {
    fn deep_copy(&self) -> Self {
        *self
    }
}
impl DeepCopy for String {
    fn deep_copy(&self) -> Self {
        self.clone()
    }
}
impl<T: DeepCopy> DeepCopy for Vec<T> {
    fn deep_copy(&self) -> Self {
        self.iter().map(DeepCopy::deep_copy).collect()
    }
}
impl<K: DeepCopy + Ord, V: DeepCopy> DeepCopy for BTreeMap<K, V> {
    fn deep_copy(&self) -> Self {
        self.iter().map(|(k, v)| (k.deep_copy(), v.deep_copy())).collect()
    }
}
impl<T: DeepCopy + Ord> DeepCopy for BTreeSet<T> {
    fn deep_copy(&self) -> Self {
        self.iter().map(DeepCopy::deep_copy).collect()
    }
}
impl<A: DeepCopy, B: DeepCopy> DeepCopy for (A, B) {
    fn deep_copy(&self) -> Self {
        (self.0.deep_copy(), self.1.deep_copy())
    }
}
impl DeepCopy for Value {
    fn deep_copy(&self) -> Self {
       match self {
           Value::Unit => Value::Unit,
           Value::Integer(val) => Value::Integer(val.deep_copy()),
           Value::Float(val) => Value::Float(val.deep_copy()),
           Value::Bool(val) => Value::Bool(val.deep_copy()),
           Value::String(val) => Value::String(val.deep_copy()),
           Value::Struct(val) => Value::Struct(val.deep_copy()),
           Value::Enum(val) => Value::Enum(val.deep_copy()),
           Value::List(val) => Value::List(val.deep_copy()),
           Value::Map(val) => Value::Map(val.deep_copy()),
           Value::Set(val) => Value::Set(val.deep_copy()),
           Value::Function(val) => Value::Function(val.deep_copy()),
       }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionValue {
    Named(String),
    Anonymous(Scope, SpanWithId),
}
impl DeepCopy for FunctionValue {
    fn deep_copy(&self) -> Self {
        // the anonymous' function scope should only shallow-copy
        self.clone()
    }
}
impl PartialOrd for FunctionValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Named(_), Anonymous(..)) => Some(Ordering::Greater),
            (Anonymous(..), Named(_)) => Some(Ordering::Less),
            (Named(a), Named(b)) => a.partial_cmp(b),
            (Anonymous(_, a), Anonymous(_, b)) => a.partial_cmp(b),
        }
    }
}
impl Ord for FunctionValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Value {
    pub fn expect_unit(self, msg: &'static str) {
        match self {
            Value::Unit => (),
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_int(self, msg: &'static str) -> i64 {
        match self {
            Value::Integer(i) => i,
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_float(self, msg: &'static str) -> FuzzyFloat {
        match self {
            Value::Float(f) => f,
            _ => panic!("{}", msg),
        }
    }
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
    pub fn expect_list(self, msg: &'static str) -> ListArc {
        match self {
            Value::List(l) => l,
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_map(self, msg: &'static str) -> MapArc {
        match self {
            Value::Map(m) => m,
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_set(self, msg: &'static str) -> SetArc {
        match self {
            Value::Set(s) => s,
            _ => panic!("{}", msg),
        }
    }
    pub fn expect_function(self, msg: &'static str) -> FunctionValue {
        match self {
            Value::Function(fun) => fun,
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
            Value::Function(_) => "function".to_string(),
            Value::List(_) => "List".to_string(),
            Value::Map(_) => "Map".to_string(),
            Value::Set(_) => "Set".to_string(),
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

macro_rules! fmt_value_wrappers {
    ($($fmt:ident, $struct_name:ident, float: $float:tt, bool: $bool:tt, string: $string:tt, debug_enum: $debug_enum:tt, Debug: $debug:tt;)*) => {$(
        pub struct $struct_name<'i>(pub &'i Value);
        impl<'i> fmt::$fmt for $struct_name<'i> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.0 {
                    Value::Unit => fmt::Debug::fmt(&(), f),
                    Value::Integer(i) => fmt::$fmt::fmt(i, f),
                    Value::Float(FuzzyFloat(_float)) => fmt_value_wrappers!(float, $float, $fmt, _float, f),
                    Value::Bool(_b) => fmt_value_wrappers!(bool, $bool, $fmt, _b, f),
                    Value::String(_s) => fmt_value_wrappers!(string, $string, $fmt, _s, f),
                    Value::Struct(s) => {
                        let s = s.s.lock();
                        let s = s.borrow();
                        let mut dbg = f.debug_struct(&s.name);
                        for (name, value) in &s.fields {
                            dbg.field(name, &$struct_name(value));
                        }
                        dbg.finish()
                    }
                    Value::Enum(e) => {
                        let e = e.e.lock();
                        let e = e.borrow();
                        let variant = fmt_value_wrappers!(format_enum, $debug_enum, e);
                        if e.fields.is_empty() {
                            f.write_str(&variant)
                        } else {
                            let mut dbg = f.debug_tuple(&variant);
                            for field in &e.fields {
                                dbg.field(&$struct_name(field));
                            }
                            dbg.finish()
                        }
                    }
                    Value::List(l) => {
                        let l = l.list.lock();
                        let l = l.borrow();
                        f.debug_list()
                            .entries(l.iter().map(|value| $struct_name(value)))
                            .finish()
                    }
                    Value::Map(m) => {
                        let m = m.map.lock();
                        let m = m.borrow();
                        f.debug_map()
                            .entries(m.iter().map(|(k, v)| ($struct_name(k), $struct_name(v))))
                            .finish()
                    }
                    Value::Set(s) => {
                        let s = s.set.lock();
                        let s = s.borrow();
                        f.debug_set()
                            .entries(s.iter().map(|v| $struct_name(v)))
                            .finish()
                    }
                    Value::Function(fun) =>  match fun {
                        FunctionValue::Named(name) => write!(f, "function {}", name),
                        FunctionValue::Anonymous(_, span) => write!(f, "anonymous function at {}:{}:{}", span.diagnostics_span().file, span.diagnostics_span().start, span.diagnostics_span().end),
                    },
                }
            }
        }
        fmt_value_wrappers! { impl_debug, $debug, $fmt, $struct_name }
    )*};
    (format_enum, true, $e:expr) => {
        format!("{}::{}", $e.name, $e.variant)
    };
    (format_enum, false, $e:expr) => {
        format!("{}", $e.variant)
    };
    (float, true, $fmt:ident, $float:expr, $f:expr) => { fmt::$fmt::fmt($float, $f) };
    (float, false, $fmt:ident, $float:expr, $f:expr) => { unreachable!("{} called on float", stringify!($fmt)) };
    // (float, $else:tt, $fmt:ident, $float:expr, $f:expr) => { compile_error!("float only accepts `true` or `false`") };
    (bool, true, $fmt:ident, $bool:expr, $f:expr) => { fmt::$fmt::fmt($bool, $f) };
    (bool, false, $fmt:ident, $bool:expr, $f:expr) => { unreachable!("{} called on bool", stringify!($fmt)) };
    // (bool, $else:tt, $fmt:ident, $bool:expr, $f:expr) => { compile_error!("bool only accepts `true` or `false`") };
    (string, true, $fmt:ident, $string:expr, $f:expr) => { fmt::$fmt::fmt($string, $f) };
    (string, false, $fmt:ident, $string:expr, $f:expr) => { unreachable!("{} called on string", stringify!($fmt)) };
    // (string, $else:tt, $fmt:ident, $string:expr, $f:expr) => { compile_error!("string only accepts `true` or `false`") };
    (impl_debug, true, $fmt:ident, $struct_name:ident) => {
        impl<'i> fmt::Debug for $struct_name<'i> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::$fmt::fmt(self, f)
            }
        }
    };
    (impl_debug, false, $fmt:ident, $struct_name:ident) => {};
    (impl_debug, $else:tt, $fmt:ident, $struct_name:ident) => { compile_error!("Debug only accepts `true` or `false`") };
}

fmt_value_wrappers! {
    Display, DisplayValue, float: true, bool: true, string: true, debug_enum: false, Debug: true;
    Debug, DebugValue, float: true, bool: true, string: true, debug_enum: true, Debug: false;
    Octal, OctalValue, float: false, bool: false, string: false, debug_enum: false, Debug: true;
    LowerHex, LowerHexValue, float: false, bool: false, string: false, debug_enum: false, Debug: true;
    UpperHex, UpperHexValue, float: false, bool: false, string: false, debug_enum: false, Debug: true;
    Binary, BinaryValue, float: false, bool: false, string: false, debug_enum: false, Debug: true;
    LowerExp, LowerExpValue, float: true, bool: false, string: false, debug_enum: false, Debug: true;
    UpperExp, UpperExpValue, float: true, bool: false, string: false, debug_enum: false, Debug: true;
}

impl FormatArgument for Value {
    fn supports_format(&self, _: &Specifier) -> bool {
        // ensured by typeck
        true
    }

    fn fmt_display(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&DisplayValue(self), f)
    }

    fn fmt_debug(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&DebugValue(self), f)
    }

    fn fmt_octal(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Octal::fmt(&OctalValue(self), f)
    }

    fn fmt_lower_hex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::LowerHex::fmt(&LowerHexValue(self), f)
    }

    fn fmt_upper_hex(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::UpperHex::fmt(&UpperHexValue(self), f)
    }

    fn fmt_binary(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Binary::fmt(&BinaryValue(self), f)
    }

    fn fmt_lower_exp(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::LowerExp::fmt(&LowerExpValue(self), f)
    }

    fn fmt_upper_exp(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::UpperExp::fmt(&UpperExpValue(self), f)
    }
}

/// Float with fuzzy equality
#[derive(Debug, Clone)]
pub struct FuzzyFloat(pub f64);
impl DeepCopy for FuzzyFloat {
    fn deep_copy(&self) -> Self {
        self.clone()
    }
}
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
impl PartialOrd for FuzzyFloat {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl Ord for FuzzyFloat {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.0.is_nan() && other.0.is_nan() || self.eq(other) {
            Ordering::Equal
        } else {
            match self.0.partial_cmp(&other.0) {
                Some(val) => val,
                None => Ordering::Equal,
            }
        }
    }
}
impl Eq for FuzzyFloat {}
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

#[derive(Clone)]
pub struct ExternalFunction {
    /// Name to inject the function with into rebo
    pub name: &'static str,
    /// The code that should be shown in error messages.
    /// It must include all generics and the spans of the generics must be used
    /// in the `typ` field with synthetic spans with the file_name.
    pub code: &'static str,
    pub file_name: &'static str,
    pub imp: RustFunction,
}
impl fmt::Debug for ExternalFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExternalFunction")
            .field("name", &self.code)
            .field("code", &self.code)
            .field("file_name", &self.file_name)
            .field("impl", &"...")
            .finish()
    }
}
pub trait RequiredReboFunction {
    const NAME: &'static str;
    const IS_METHOD: bool;
    const GENERICS: &'static [&'static str];
    const GENERICS_FILE_NAME: &'static str;
    const GENERICS_FILE_CONTENT: &'static str;
    fn arg_types() -> Vec<Type>;
    fn ret_type() -> Type;
}
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct RequiredReboFunctionStruct {
    pub name: &'static str,
    pub is_method: bool,
    pub generics: &'static [&'static str],
    pub generics_file_name: &'static str,
    pub generics_file_content: &'static str,
    pub args: Vec<Type>,
    pub ret: Type,
}
impl RequiredReboFunctionStruct {
    pub fn from_required_rebo_function<T: RequiredReboFunction>() -> Self {
        RequiredReboFunctionStruct {
            name: T::NAME,
            is_method: T::IS_METHOD,
            generics: T::GENERICS,
            generics_file_name: T::GENERICS_FILE_NAME,
            generics_file_content: T::GENERICS_FILE_CONTENT,
            args: T::arg_types(),
            ret: T::ret_type(),
        }
    }
}

pub type RustFunction = for<'i> fn(expr_span: Span, &mut VmContext<'i, '_, '_>, Vec<Value>) -> Result<Value, ExecError<'i>>;
#[derive(Clone)]
pub enum Function {
    /// fn-pointer
    Rust(RustFunction),
    /// function name, argument binding ids, definition span
    Rebo(String, Vec<BindingId>),
    /// enum name, variant name, variant definition span
    EnumInitializer(String, String)
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
impl StructArc {
    pub fn new(s: Struct) -> StructArc {
        StructArc { s: Arc::new(ReentrantMutex::new(RefCell::new(s))) }
    }
}
impl DeepCopy for StructArc {
    fn deep_copy(&self) -> Self {
        StructArc::new(self.s.lock().borrow().deep_copy())
    }
}
impl PartialOrd for StructArc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.s.lock().borrow().partial_cmp(&other.s.lock().borrow())
    }
}
impl Ord for StructArc {
    fn cmp(&self, other: &Self) -> Ordering {
        self.s.lock().borrow().cmp(&other.s.lock().borrow())
    }
}
impl PartialEq for StructArc {
    fn eq(&self, other: &Self) -> bool {
        self.s.lock().borrow().eq(&other.s.lock().borrow())
    }
}
impl Eq for StructArc {}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, Value)>,
}
impl DeepCopy for Struct {
    fn deep_copy(&self) -> Self {
        Struct {
            name: self.name.deep_copy(),
            fields: self.fields.deep_copy(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumArc {
    pub e: Arc<ReentrantMutex<RefCell<Enum>>>,
}
impl EnumArc {
    pub fn new(e: Enum) -> EnumArc {
        EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(e))) }
    }
}
impl DeepCopy for EnumArc {
    fn deep_copy(&self) -> Self {
        EnumArc::new(self.e.lock().borrow().deep_copy())
    }
}
impl PartialOrd for EnumArc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.e.lock().borrow().partial_cmp(&other.e.lock().borrow())
    }
}
impl Ord for EnumArc {
    fn cmp(&self, other: &Self) -> Ordering {
        self.e.lock().borrow().cmp(&other.e.lock().borrow())
    }
}
impl PartialEq for EnumArc {
    fn eq(&self, other: &Self) -> bool {
        self.e.lock().borrow().eq(&other.e.lock().borrow())
    }
}
impl Eq for EnumArc {}
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Enum {
    pub name: String,
    pub variant_index: usize,
    pub variant: String,
    pub fields: Vec<Value>,
}
impl DeepCopy for Enum {
    fn deep_copy(&self) -> Self {
        Enum {
            name: self.name.deep_copy(),
            variant_index: self.variant_index,
            variant: self.variant.deep_copy(),
            fields: self.fields.deep_copy(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ListArc {
    pub list: Arc<ReentrantMutex<RefCell<Vec<Value>>>>,
}
impl ListArc {
    pub fn new(values: Vec<Value>) -> ListArc {
        ListArc { list: Arc::new(ReentrantMutex::new(RefCell::new(values))) }
    }
    pub fn clone_list<T, L: FromIterator<T>>(&self) -> L where T: FromValue {
        self.list.lock().borrow().iter().cloned().map(FromValue::from_value).collect()
    }
}
impl DeepCopy for ListArc {
    fn deep_copy(&self) -> Self {
        ListArc::new(self.list.lock().borrow().deep_copy())
    }
}
impl PartialOrd for ListArc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.list.lock().borrow().partial_cmp(&other.list.lock().borrow())
    }
}
impl Ord for ListArc {
    fn cmp(&self, other: &Self) -> Ordering {
        self.list.lock().borrow().cmp(&other.list.lock().borrow())
    }
}
impl PartialEq for ListArc {
    fn eq(&self, other: &Self) -> bool {
        self.list.lock().borrow().eq(&*other.list.lock().borrow())
    }
}
impl Eq for ListArc {}

#[derive(Debug, Clone)]
pub struct MapArc {
    pub map: Arc<ReentrantMutex<RefCell<BTreeMap<Value, Value>>>>,
}
impl MapArc {
    pub fn new(map: BTreeMap<Value, Value>) -> MapArc {
        MapArc { map: Arc::new(ReentrantMutex::new(RefCell::new(map))) }
    }
    pub fn clone_map<K, V, M: FromIterator<(K, V)>>(&self) -> M where K: FromValue, V: FromValue {
        self.map.lock().borrow().iter()
            .map(|(k, v)| (FromValue::from_value(k.clone()), FromValue::from_value(v.clone())))
            .collect()
    }
}
impl DeepCopy for MapArc {
    fn deep_copy(&self) -> Self {
        MapArc::new(self.map.lock().borrow().deep_copy())
    }
}
impl PartialOrd for MapArc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.map.lock().borrow().partial_cmp(&other.map.lock().borrow())
    }
}
impl Ord for MapArc {
    fn cmp(&self, other: &Self) -> Ordering {
        self.map.lock().borrow().cmp(&other.map.lock().borrow())
    }
}
impl PartialEq for MapArc {
    fn eq(&self, other: &Self) -> bool {
        self.map.lock().borrow().eq(&*other.map.lock().borrow())
    }
}
impl Eq for MapArc {}

#[derive(Debug, Clone)]
pub struct SetArc {
    pub set: Arc<ReentrantMutex<RefCell<BTreeSet<Value>>>>,
}
impl SetArc {
    pub fn new(set: BTreeSet<Value>) -> SetArc {
        SetArc { set: Arc::new(ReentrantMutex::new(RefCell::new(set))) }
    }
}
impl DeepCopy for SetArc {
    fn deep_copy(&self) -> Self {
        SetArc::new(self.set.lock().borrow().deep_copy())
    }
}
impl PartialOrd for SetArc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.set.lock().borrow().partial_cmp(&other.set.lock().borrow())
    }
}
impl Ord for SetArc {
    fn cmp(&self, other: &Self) -> Ordering {
        self.set.lock().borrow().cmp(&other.set.lock().borrow())
    }
}
impl PartialEq for SetArc {
    fn eq(&self, other: &Self) -> bool {
        self.set.lock().borrow().eq(&*other.set.lock().borrow())
    }
}
impl Eq for SetArc {}

macro_rules! impl_from_into {
    ($ty:ty, $name:ident) => {
        impl Typed for $ty {
            fn typ() -> SpecificType {
                SpecificType::$name
            }
        }
        impl FromValue for $ty {
            fn from_value(value: Value) -> Self {
                match value {
                    Value::$name(val) => val,
                    _ => unreachable!("expected {}, got {:?}", stringify!($name), value),
                }
            }
        }
        impl IntoValue for $ty {
            fn into_value(self) -> Value {
                Value::$name(self)
            }
        }
    }
}

impl FromValue for () {
    fn from_value(value: Value) -> Self {
        match value {
            Value::Unit => (),
            _ => unreachable!(),
        }
    }
}
impl IntoValue for () {
    fn into_value(self) -> Value {
        Value::Unit
    }
}
impl Typed for () {
    fn typ() -> SpecificType {
        SpecificType::Unit
    }
}

// Never type represented by ::std::convert::Infallible.
//
// We can't use the never type `!` as it's feature gated behind `never_type` and we are using stable.
// We can't use the never-say-never hack as that only allows for crate-local normalization, i.e.,
// it is only a different type within a crate and acts as all types outside of the crate.
// What that means is that you can't impl the trait `Foo` for both `never_say_never::Never` and `crate::Bar`.
// You could vendor the never-say-never crate's code into your crate, which allows implementing
// `Foo` for `crate::Never` and `crate::Bar`.
// However, it doesn't allow anyone from a different crate to implement `Foo` for any of their types:
// ```
// error[E0119]: conflicting implementations of trait `Foo` for type `MyStruct`
//  --> src/main.rs:2:1
//   |
// 2 | impl foo::Foo for MyStruct {}
//   | ^^^^^^^^^^^^^^^^^^^^^^^^^^
//   |
//   = note: conflicting implementation in crate `foo`:
//           - impl Foo for <fn() -> ! as foo::fn_traits::FnOnce<()>>::Output;
//
// For more information about this error, try `rustc --explain E0119`.
// error: could not compile `bar` (bin "bar") due to 1 previous error
// ```
impl IntoValue for Infallible {
    fn into_value(self) -> Value {
        unreachable!("IntoValue::into_value called on Never type")
    }
}
impl FromValue for Infallible {
    fn from_value(_: Value) -> Self {
        unreachable!("FromValue::from_value called on Never type")
    }
}

impl_from_into!(i64, Integer);
impl_from_into!(FuzzyFloat, Float);
impl_from_into!(bool, Bool);
impl_from_into!(String, String);

impl FromValue for Value {
    fn from_value(value: Value) -> Self {
        value
    }
}
impl IntoValue for Value {
    fn into_value(self) -> Value {
        self
    }
}

macro_rules! impl_int_types {
    ($($t:ty),*) => {
        $(
            impl Typed for $t {
                fn typ() -> SpecificType {
                    SpecificType::Integer
                }
            }
            impl IntoValue for $t {
                fn into_value(self) -> Value {
                    Value::Integer(self.try_into().unwrap_or_else(|e| panic!("can't convert `{}_{}` to i64: {}", self, stringify!($t), e)))
                }
            }
            impl FromValue for $t {
                fn from_value(value: Value) -> $t {
                    match value {
                        Value::Integer(i) => {
                            i.try_into().unwrap_or_else(|e| panic!("can't convert `{}_i64` to {}: {}", i, stringify!($t), e))
                        }
                        _ => unreachable!("{}::from_value called with non-integer: {:?}", stringify!($t), value),
                    }
                }
            }
        )*
    }
}

impl_int_types!(u8, i8, u16, i16, u32, i32, u64, usize, isize);
impl Typed for f32 {
    fn typ() -> SpecificType {
        SpecificType::Float
    }
}
impl IntoValue for f32 {
    fn into_value(self) -> Value {
        Value::Float(FuzzyFloat(self as f64))
    }
}
impl FromValue for f32 {
    fn from_value(value: Value) -> f32 {
        match value {
            Value::Float(i) => {
                i.0 as f32
            }
            _ => unreachable!("f32::from_value called with non-float: {:?}", value),
        }
    }
}

impl Typed for f64 {
    fn typ() -> SpecificType {
        SpecificType::Float
    }
}
impl IntoValue for f64 {
    fn into_value(self) -> Value {
        Value::Float(FuzzyFloat(self))
    }
}
impl FromValue for f64 {
    fn from_value(value: Value) -> f64 {
        match value {
            Value::Float(i) => {
                i.0
            }
            _ => unreachable!("f64::from_value called with non-float: {:?}", value),
        }
    }
}

pub trait TypedFunction {
    type Args;
    type Ret: FromValue;
    fn function_value(&self) -> &FunctionValue;
    fn to_value_vec(args: Self::Args) -> Vec<Value>;
}

#[derive(Debug, Clone)]
pub struct TypedFunctionValue<T> {
    function: FunctionValue,
    _marker: PhantomData<T>,
}

#[derive(Debug, Clone)]
pub struct BoundFunctionValue<Ret: FromValue> {
    function: FunctionValue,
    args: Vec<Value>,
    _marker: PhantomData<Ret>
}
impl<Ret: FromValue> Into<(FunctionValue, Vec<Value>)> for BoundFunctionValue<Ret> {
    fn into(self) -> (FunctionValue, Vec<Value>) {
        (self.function, self.args)
    }
}

macro_rules! impl_typed_function {
    ($($generic:ident),*) => {
        impl<$($generic: FromValue + IntoValue,)* R: FromValue + IntoValue> TypedFunctionValue<fn($($generic),*) -> R> {
            pub fn bind(self, args: <Self as TypedFunction>::Args) -> BoundFunctionValue<<Self as TypedFunction>::Ret> {
                BoundFunctionValue {
                    function: self.function,
                    args: Self::to_value_vec(args),
                    _marker: PhantomData,
                }
            }
        }
        impl<$($generic: Typed,)* R: Typed> Typed for TypedFunctionValue<fn($($generic),*) -> R> {
            fn typ() -> SpecificType {
                static TYP: OnceLock<SpecificType> = OnceLock::new();
                TYP.get_or_init(|| {
                    SpecificType::Function(Box::new(FunctionType {
                        is_method: false,
                        generics: Cow::Borrowed(&[]),
                        args: Cow::Owned(vec![
                            $(
                                Type::Specific($generic::typ()),
                            )*
                        ]),
                        ret: Type::Specific(R::typ()),
                    }))
                }).clone()
            }
        }

        impl<$($generic: FromValue + IntoValue,)* R: FromValue + IntoValue> FromValue for TypedFunctionValue<fn($($generic),*) -> R> {
            fn from_value(value: Value) -> Self {
                match value {
                    Value::Function(function) => TypedFunctionValue {
                        function,
                        _marker: PhantomData,
                    },
                    _ => unreachable!("TypedFunctionValue::from_value called with non-function: {:?}", value),
                }
            }
        }
        impl<$($generic: FromValue + IntoValue,)* R: FromValue + IntoValue> IntoValue for TypedFunctionValue<fn($($generic),*) -> R> {
            fn into_value(self) -> Value {
                Value::Function(self.function)
            }
        }
        impl<$($generic: FromValue + IntoValue,)* R: FromValue + IntoValue> TypedFunction for TypedFunctionValue<fn($($generic),*) -> R> {
            #[allow(unused_parens)]
            type Args = ($($generic),*);
            type Ret = R;

            fn function_value(&self) -> &FunctionValue {
                &self.function
            }
            fn to_value_vec(args: Self::Args) -> Vec<Value> {
                #[allow(non_snake_case)]
            #[allow(unused_parens)]
                let ($($generic),*) = args;
                vec![
                    $(
                        $generic.into_value(),
                    )*
                ]
            }
        }
    };
}
impl_typed_function!();
impl_typed_function!(A);
impl_typed_function!(A, B);
impl_typed_function!(A, B, C);
impl_typed_function!(A, B, C, D);
impl_typed_function!(A, B, C, D, E);
impl_typed_function!(A, B, C, D, E, F);
impl_typed_function!(A, B, C, D, E, F, G);
impl_typed_function!(A, B, C, D, E, F, G, H);
impl_typed_function!(A, B, C, D, E, F, G, H, I);
impl_typed_function!(A, B, C, D, E, F, G, H, I, J);
impl_typed_function!(A, B, C, D, E, F, G, H, I, J, K);
impl_typed_function!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_typed_function!(A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_typed_function!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
