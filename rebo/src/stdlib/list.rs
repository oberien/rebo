use diagnostic::{Diagnostics, Span};
use typed_arena::Arena;
use crate::parser::Expr;
use crate::common::{MetaInfo, Value, ListArc};
use crate::typeck::types::{Type, SpecificType};
use std::borrow::Cow;
use parking_lot::ReentrantMutex;
use std::sync::Arc;
use std::cell::RefCell;
use std::marker::PhantomData;
use crate::{CowVec, ExternalType, FileId, FromValue, IntoValue, Typed};

pub struct List<T> {
    arc: ListArc,
    _marker: PhantomData<T>,
}
impl<T> List<T> {
    pub fn new(vec: Vec<Value>) -> List<T> {
        List {
            arc: ListArc { list: Arc::new(ReentrantMutex::new(RefCell::new(vec))) },
            _marker: PhantomData,
        }
    }
}

const FILE_NAME: &'static str = "external-List.re";
const LIST_T: Span = Span::new(FileId::synthetic(FILE_NAME), 12, 13);

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
    const TYPE: SpecificType = SpecificType::Struct(
        Cow::Borrowed("List"),
        CowVec::Borrowed(&[(LIST_T, Type::Top)]),
    );
}

pub fn add_list<'a, 'i>(diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    meta_info.add_external_type::<List<Value>>(arena, diagnostics);
    meta_info.add_external_function(arena, diagnostics, list_new);
    meta_info.add_external_function(arena, diagnostics, list_of);
    meta_info.add_external_function(arena, diagnostics, list_push);
    meta_info.add_external_function(arena, diagnostics, list_get);
    meta_info.add_external_function(arena, diagnostics, list_len);
    meta_info.add_external_function(arena, diagnostics, list_pop);
    meta_info.add_external_function(arena, diagnostics, list_last);
}

#[rebo::function("List::new")]
fn list_new<T>() -> List<T> {
    List::new(Vec::new())
}

#[rebo::function(raw("List::of"))]
fn list_of<T>(..: T) -> List<T> {
    List::new(args.collect())
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
    match this.get(index as usize).cloned() {
        Some(v) => Some(v),
        None => None,
    }
}
#[rebo::function("List::pop")]
fn list_pop<T>(this: List<T>) -> Option<T> {
    this.arc.list.lock().borrow_mut().pop()
}
#[rebo::function("List::last")]
fn list_last<T>(this: List<T>) -> Option<T> {
    this.arc.list.lock().borrow().last().cloned()
}
#[rebo::function("List::len")]
fn list_len<T>(this: List<T>) -> usize {
    this.arc.list.lock().borrow().len()
}
