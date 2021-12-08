use diagnostic::{Diagnostics, Span};
use typed_arena::Arena;
use crate::parser::Expr;
use crate::common::{MetaInfo, Value, MapArc};
use crate::typeck::types::{Type, SpecificType};
use std::borrow::Cow;
use parking_lot::ReentrantMutex;
use std::sync::Arc;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::marker::PhantomData;
use crate::{CowVec, ExternalType, FileId, FromValue, IntoValue, Typed};
use crate::stdlib::list::List;

pub struct Map<K, V> {
    arc: MapArc,
    _marker: PhantomData<(K, V)>,
}

const FILE_NAME: &'static str = "external-Map.re";
const MAP_K: Span = Span::new(FileId::synthetic(FILE_NAME), 11, 12);
const MAP_V: Span = Span::new(FileId::synthetic(FILE_NAME), 14, 15);

impl<K: FromValue + IntoValue, V: FromValue + IntoValue> ExternalType for Map<K, V> {
    const CODE: &'static str = "struct Map<K, V> {\n    /* ... */\n}";
    const FILE_NAME: &'static str = FILE_NAME;
}
impl<K: FromValue, V: FromValue> FromValue for Map<K, V> {
    fn from_value(value: Value) -> Self {
        match value {
            Value::Map(arc) => Map { arc, _marker: PhantomData },
            _ => unreachable!("Map::from_value called with non-Map"),
        }
    }
}
impl<K: IntoValue, V: IntoValue> IntoValue for Map<K, V> {
    fn into_value(self) -> Value {
        Value::Map(self.arc)
    }
}
impl<K, V> Typed for Map<K, V> {
    const TYPE: SpecificType = SpecificType::Struct(
        Cow::Borrowed("Map"),
        CowVec::Borrowed(&[(MAP_K, Type::Top), (MAP_V, Type::Top)]),
    );
}

pub fn add_map<'a, 'i>(diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    meta_info.add_external_type::<Map<Value, Value>>(arena, diagnostics);

    meta_info.add_external_function(arena, diagnostics, map_new);
    meta_info.add_external_function(arena, diagnostics, map_insert);
    meta_info.add_external_function(arena, diagnostics, map_get);
    meta_info.add_external_function(arena, diagnostics, map_get_or_insert);
    meta_info.add_external_function(arena, diagnostics, map_remove);
    meta_info.add_external_function(arena, diagnostics, map_keys);
    meta_info.add_external_function(arena, diagnostics, map_values);
    meta_info.add_external_function(arena, diagnostics, map_len);
}

#[rebo::function("Map::new")]
fn map_new<K, V>() -> Map<K, V> {
    Map {
        arc: MapArc { map: Arc::new(ReentrantMutex::new(RefCell::new(BTreeMap::new()))) },
        _marker: PhantomData,
    }
}

#[rebo::function("Map::insert")]
fn map_insert<K, V>(this: Map<K, V>, key: K, value: V) -> Option<V> {
    let this = this.arc.map.lock();
    let mut this = this.borrow_mut();
    match this.insert(key, value) {
        Some(v) => Some(v),
        None => None,
    }
}

#[rebo::function("Map::get")]
fn map_get<K, V>(this: Map<K, V>, key: K) -> Option<V> {
    let this = this.arc.map.lock();
    let this = this.borrow();
    match this.get(&key) {
        Some(v) => Some(v.clone()),
        None => None,
    }
}
#[rebo::function("Map::get_or_insert")]
fn map_get_or_insert<K, V>(this: Map<K, V>, key: K, default: V) -> V {
    let this = this.arc.map.lock();
    let mut this = this.borrow_mut();
    this.entry(key).or_insert(default).clone()
}

#[rebo::function("Map::remove")]
fn map_remove<K, V>(this: Map<K, V>, key: K) -> Option<V> {
    let this = this.arc.map.lock();
    let mut this = this.borrow_mut();
    match this.remove(&key) {
        Some(v) => Some(v),
        None => None,
    }
}

#[rebo::function("Map::keys")]
fn map_keys<K, V>(this: Map<K, V>) -> List<K> {
    let this = this.arc.map.lock();
    let this = this.borrow();
    let keys = this.keys().cloned();
    List::new(keys)
}

#[rebo::function("Map::values")]
fn map_values<K, V>(this: Map<K, V>) -> List<V> {
    let this = this.arc.map.lock();
    let this = this.borrow();
    let values = this.values().cloned();
    List::new(values)
}

#[rebo::function("Map::len")]
fn map_len<K, V>(this: Map<K, V>) -> usize {
    this.arc.map.lock().borrow().len()
}
