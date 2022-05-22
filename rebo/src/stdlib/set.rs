use diagnostic::{Diagnostics, Span};
use typed_arena::Arena;
use crate::parser::Expr;
use crate::common::{MetaInfo, Value, SetArc};
use crate::typeck::types::{Type, SpecificType};
use std::borrow::Cow;
use std::marker::PhantomData;
use rebo::common::DeepCopy;
use crate::{CowVec, ExternalType, FileId, FromValue, IntoValue, Typed, ErrorCode};
use crate::stdlib::list::List;

pub struct Set<T> {
    arc: SetArc,
    _marker: PhantomData<T>,
}
impl<T: IntoValue> Set<T> {
    pub fn new(values: impl IntoIterator<Item = T>) -> Set<T> {
        Set {
            arc: SetArc::new(values.into_iter().map(IntoValue::into_value).collect()),
            _marker: PhantomData,
        }
    }
}

const FILE_NAME: &str = "external-Set.re";
const SET_T: Span = Span::new(FileId::synthetic(FILE_NAME), 11, 12);

impl<T: FromValue + IntoValue> ExternalType for Set<T> {
    const CODE: &'static str = "struct Set<T> {\n    /* ... */\n}";
    const FILE_NAME: &'static str = FILE_NAME;
}
impl<T: FromValue> FromValue for Set<T> {
    fn from_value(value: Value) -> Self {
        match value {
            Value::Set(arc) => Set { arc, _marker: PhantomData },
            _ => unreachable!("Set::from_value called with non-Set"),
        }
    }
}
impl<T: IntoValue> IntoValue for Set<T> {
    fn into_value(self) -> Value {
        Value::Set(self.arc)
    }
}
impl<T> Typed for Set<T> {
    const TYPE: SpecificType = SpecificType::Struct(
        Cow::Borrowed("Set"),
        CowVec::Borrowed(&[(SET_T, Type::Top)]),
    );
}
impl<T> DeepCopy for Set<T> {
    fn deep_copy(&self) -> Self {
        Set {
            arc: self.arc.deep_copy(),
            _marker: PhantomData,
        }
    }
}

pub fn add_set<'a, 'i>(diagnostics: &'i Diagnostics<ErrorCode>, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    meta_info.add_external_type::<Set<Value>>(arena, diagnostics);

    meta_info.add_external_function(arena, diagnostics, set_new);
    meta_info.add_external_function(arena, diagnostics, set_insert);
    meta_info.add_external_function(arena, diagnostics, set_get);
    meta_info.add_external_function(arena, diagnostics, set_remove);
    meta_info.add_external_function(arena, diagnostics, set_values);
    meta_info.add_external_function(arena, diagnostics, set_union);
    meta_info.add_external_function(arena, diagnostics, set_len);
    meta_info.add_external_function(arena, diagnostics, set_clear);
    meta_info.add_external_function(arena, diagnostics, set_is_empty);
}

#[rebo::function("Set::new")]
fn set_new<T>() -> Set<T> {
    Set::new(Vec::new())
}

#[rebo::function("Set::insert")]
fn set_insert<T>(this: Set<T>, value: T) -> bool {
    let this = this.arc.set.lock();
    let mut this = this.borrow_mut();
    this.insert(value)
}

#[rebo::function("Set::contains")]
fn set_get<T>(this: Set<T>, value: T) -> bool {
    let this = this.arc.set.lock();
    let this = this.borrow();
    this.contains(&value)
}

#[rebo::function("Set::remove")]
fn set_remove<T>(this: Set<T>, value: T) -> bool {
    let this = this.arc.set.lock();
    let mut this = this.borrow_mut();
    this.remove(&value)
}

#[rebo::function("Set::values")]
fn set_values<T>(this: Set<T>) -> List<T> {
    let this = this.arc.set.lock();
    let this = this.borrow();
    let values = this.iter().cloned();
    List::new(values)
}

#[rebo::function("Set::union")]
fn set_union<T>(this: Set<T>, other: Set<T>) -> Set<T> {
    Set::new(this.arc.set.lock().borrow().union(&*other.arc.set.lock().borrow()).cloned())
}

#[rebo::function("Set::len")]
fn set_len<T>(this: Set<T>) -> usize {
    this.arc.set.lock().borrow().len()
}
#[rebo::function("Set::clear")]
fn set_clear<T>(this: Set<T>) {
    this.arc.set.lock().borrow_mut().clear()
}
#[rebo::function("Set::is_empty")]
fn set_is_empty<T>(this: Set<T>) -> bool {
    this.arc.set.lock().borrow().is_empty()
}
