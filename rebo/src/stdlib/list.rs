use std::iter::FromIterator;
use diagnostic::Diagnostics;
use typed_arena::Arena;
use crate::parser::Expr;
use crate::common::{MetaInfo, Value, ListArc, DeepCopy};
use crate::typeck::types::{Type, SpecificType};
use std::marker::PhantomData;
use std::sync::OnceLock;
use itertools::Itertools;
use rebo::common::SpanWithId;
use rebo::stdlib::Sliceable;
use crate::{DisplayValue, ExternalType, FileId, FromValue, IntoValue, Typed, ErrorCode};

#[derive(Debug, Clone)]
pub struct List<T> {
    pub arc: ListArc,
    _marker: PhantomData<T>,
}
impl<T: IntoValue> List<T> {
    pub fn new(values: impl IntoIterator<Item = T>) -> List<T> {
        List {
            arc: ListArc::new(values.into_iter().map(IntoValue::into_value).collect()),
            _marker: PhantomData,
        }
    }
    pub fn clone_list<L: FromIterator<T>>(&self) -> L where T: FromValue {
        self.arc.clone_list()
    }
}

const FILE_NAME: &str = "external-List.re";

impl<T: FromValue + IntoValue> ExternalType for List<T> {
    const CODE: &'static str = "struct List<T> {\n    /* ... */\n}";
    const FILE_NAME: &'static str = FILE_NAME;
}
impl<T: FromValue> FromValue for List<T> {
    fn from_value(value: Value) -> Self {
        match value {
            Value::List(arc) => List { arc, _marker: PhantomData },
            _ => unreachable!("List::from_value called with non-List"),
        }
    }
}
impl<T: IntoValue> IntoValue for List<T> {
    fn into_value(self) -> Value {
        Value::List(self.arc)
    }
}
impl<T> Typed for List<T> {
    fn typ() -> SpecificType {
        static TYPE: OnceLock<SpecificType> = OnceLock::new();
        TYPE.get_or_init(|| {
            SpecificType::Struct(
                "List".to_string(),
                vec![(SpanWithId::new(FileId::synthetic_named(FILE_NAME), 12, 13), Type::Top)],
            )
        }).clone()
    }
}

impl<T: IntoValue> IntoValue for Vec<T> {
    fn into_value(self) -> Value {
        Value::List(List::new(self).arc)
    }
}
impl<T: FromValue> FromValue for Vec<T> {
    fn from_value(value: Value) -> Self {
        match value {
            Value::List(arc) => arc.list.lock().borrow().iter().cloned().map(FromValue::from_value).collect(),
            _ => unreachable!("Vec::from_value called with non-List: {:?}", value),
        }
    }
}
impl<T> DeepCopy for List<T> {
    fn deep_copy(&self) -> Self {
        List {
            arc: self.arc.deep_copy(),
            _marker: PhantomData,
        }
    }
}

pub fn add_list<'i>(diagnostics: &'i Diagnostics<ErrorCode>, arena: &'i Arena<Expr<'i>>, meta_info: &mut MetaInfo<'i>) {
    meta_info.add_external_type::<List<Value>>(arena, diagnostics);
    meta_info.add_external_function(arena, diagnostics, list_new);
    meta_info.add_external_function(arena, diagnostics, list_of);
    meta_info.add_external_function(arena, diagnostics, list_push);
    meta_info.add_external_function(arena, diagnostics, list_get);
    meta_info.add_external_function(arena, diagnostics, list_set);
    meta_info.add_external_function(arena, diagnostics, list_len);
    meta_info.add_external_function(arena, diagnostics, list_is_empty);
    meta_info.add_external_function(arena, diagnostics, list_pop);
    meta_info.add_external_function(arena, diagnostics, list_first);
    meta_info.add_external_function(arena, diagnostics, list_last);
    meta_info.add_external_function(arena, diagnostics, list_min);
    meta_info.add_external_function(arena, diagnostics, list_max);
    meta_info.add_external_function(arena, diagnostics, list_contains);
    meta_info.add_external_function(arena, diagnostics, list_extend);
    meta_info.add_external_function(arena, diagnostics, list_repeat);
    meta_info.add_external_function(arena, diagnostics, list_remove);
    meta_info.add_external_function(arena, diagnostics, list_remove_element);
    meta_info.add_external_function(arena, diagnostics, list_swap_remove);
    meta_info.add_external_function(arena, diagnostics, list_join);
    meta_info.add_external_function(arena, diagnostics, list_slice);
    meta_info.add_external_function(arena, diagnostics, list_clear);
    meta_info.add_external_function(arena, diagnostics, list_sort);
    meta_info.add_external_function(arena, diagnostics, list_reverse);
    meta_info.add_external_function(arena, diagnostics, list_range);
}

#[rebo::function("List::new")]
fn list_new<T>() -> List<T> {
    List::new(Vec::new())
}

#[rebo::function(raw("List::of"))]
fn list_of<T>(..: T) -> List<T> {
    List::new(args)
}

#[rebo::function("List::push")]
fn list_push<T>(this: List<T>, value: T) {
    let this = this.arc.list.lock();
    this.borrow_mut().push(value);
}

#[rebo::function("List::get")]
fn list_get<T>(this: List<T>, index: i64) -> Option<T> {
    let this = this.arc.list.lock();
    let this = this.borrow();
    this.get(index as usize).cloned()
}
#[rebo::function("List::set")]
fn list_set<T>(this: List<T>, index: i64, value: T) -> Option<T> {
    let this = this.arc.list.lock();
    let mut this = this.borrow_mut();
    if this.get(index as usize).is_some() {
        this[index as usize] = value.clone();
        Some(value)
    } else {
        None
    }
}
#[rebo::function("List::pop")]
fn list_pop<T>(this: List<T>) -> Option<T> {
    this.arc.list.lock().borrow_mut().pop()
}
#[rebo::function("List::first")]
fn list_first<T>(this: List<T>) -> Option<T> {
    this.arc.list.lock().borrow().first().cloned()
}
#[rebo::function("List::last")]
fn list_last<T>(this: List<T>) -> Option<T> {
    this.arc.list.lock().borrow().last().cloned()
}
#[rebo::function("List::min")]
fn list_min<T>(this: List<T>) -> Option<T> {
    this.arc.list.lock().borrow().iter().min().cloned()
}
#[rebo::function("List::max")]
fn list_max<T>(this: List<T>) -> Option<T> {
    this.arc.list.lock().borrow().iter().max().cloned()
}
#[rebo::function("List::len")]
fn list_len<T>(this: List<T>) -> usize {
    this.arc.list.lock().borrow().len()
}
#[rebo::function("List::is_empty")]
fn list_is_empty<T>(this: List<T>) -> bool {
    this.arc.list.lock().borrow().is_empty()
}
#[rebo::function("List::contains")]
fn list_contains<T>(this: List<T>, item: T) -> bool {
    this.arc.list.lock().borrow().contains(&item)
}
#[rebo::function("List::extend")]
fn list_extend<T>(this: List<T>, other: List<T>) {
    let this = this.arc.list.lock();
    let mut this = this.borrow_mut();
    this.extend(other.arc.list.lock().borrow().iter().cloned());
}
#[rebo::function("List::repeat")]
fn list_repeat<T>(this: List<T>, n: usize) -> List<T> {
    let this = this.arc.list.lock();
    let this = this.borrow();
    List::new(this.iter().cycle().take(this.len() * n).cloned())
}
#[rebo::function("List::remove")]
fn list_remove<T>(this: List<T>, index: i64) -> Option<T> {
    let this = this.arc.list.lock();
    let mut this = this.borrow_mut();
    if this.get(index as usize).is_some() {
        Some(this.remove(index as usize))
    } else {
        None
    }
}
#[rebo::function("List::remove_element")]
fn list_remove_element<T>(this: List<T>, e: T) -> Option<T> {
    let this = this.arc.list.lock();
    let mut this = this.borrow_mut();
    if let Some(pos) = this.iter().position(|v| *v == e) {
        Some(this.remove(pos))
    } else {
        None
    }
}
#[rebo::function("List::swap_remove")]
fn list_swap_remove<T>(this: List<T>, index: i64) -> Option<T> {
    let this = this.arc.list.lock();
    let mut this = this.borrow_mut();
    if this.get(index as usize).is_some() {
        Some(this.swap_remove(index as usize))
    } else {
        None
    }
}
#[rebo::function("List::join")]
fn list_join<T>(this: List<T>, sep: String) -> String {
    this.arc.list.lock().borrow().iter().map(DisplayValue).join(&sep)
}
impl<T> Sliceable for List<T> {
    fn len(&self) -> usize { self.arc.list.lock().borrow().len() }
    fn remove_start(&mut self, num: usize) {
        self.arc.list.lock().borrow_mut().drain(..num);
    }
    fn remove_end(&mut self, num: usize) {
        let lock = self.arc.list.lock();
        let mut lock = lock.borrow_mut();
        let len = lock.len();
        lock.truncate(len - num);
    }
    fn name() -> &'static str { "List" }
}
#[rebo::function(raw("List::slice"))]
fn list_slice<T>(this: List<T>, start: i64, ..: i64) -> List<T> {
    this.deep_copy().slice(vm, expr_span, start, args)?
}
#[rebo::function(raw("List::clear"))]
fn list_clear<T>(this: List<T>) {
    this.arc.list.lock().borrow_mut().clear()
}
#[rebo::function(raw("List::sort"))]
fn list_sort<T>(this: List<T>) {
    this.arc.list.lock().borrow_mut().sort()
}
#[rebo::function(raw("List::reverse"))]
fn list_reverse<T>(this: List<T>) {
    this.arc.list.lock().borrow_mut().reverse()
}
#[rebo::function("List::range")]
fn list_range(from: i64, to: i64) -> List<i64> {
    List::new(from..to)
}
