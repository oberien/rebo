use diagnostic::Diagnostics;
use typed_arena::Arena;
use crate::parser::Expr;
use crate::common::{MetaInfo, Value, MapArc, DeepCopy};
use crate::typeck::types::{Type, SpecificType};
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use std::marker::PhantomData;
use std::sync::OnceLock;
use crate::{ExternalType, FileId, FromValue, IntoValue, Typed, ErrorCode, SpanWithId};
use crate::stdlib::list::List;

pub struct Map<K, V> {
    arc: MapArc,
    _marker: PhantomData<(K, V)>,
}

impl<K: IntoValue, V: IntoValue> Map<K, V> {
    pub fn new(values: impl IntoIterator<Item=(K, V)>) -> Map<K, V> {
        Map {
            arc: MapArc::new(values.into_iter().map(|(k, v)| (k.into_value(), v.into_value())).collect()),
            _marker: PhantomData,
        }
    }
    pub fn clone_btreemap(&self) -> BTreeMap<K, V> where K: FromValue + Ord, V: FromValue {
        self.arc.map.lock().borrow().iter()
            .map(|(k, v)| (FromValue::from_value(k.clone()), FromValue::from_value(v.clone())))
            .collect()
    }
}

const FILE_NAME: &str = "external-Map.re";

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
    fn typ() -> SpecificType {
        static TYPE: OnceLock<SpecificType> = OnceLock::new();
        TYPE.get_or_init(|| {
            SpecificType::Struct(
                "Map".to_string(),
                vec![
                    (SpanWithId::new(FileId::synthetic_named(FILE_NAME), 11, 12), Type::Top),
                    (SpanWithId::new(FileId::synthetic_named(FILE_NAME), 14, 15), Type::Top),
                ],
            )
        }).clone()
    }
}
impl<K, V> DeepCopy for Map<K, V> {
    fn deep_copy(&self) -> Self {
        Map {
            arc: self.arc.deep_copy(),
            _marker: PhantomData,
        }
    }
}

macro_rules! impl_map {
    ($t:ty where K: $k:ident $(+ $ks:ident)*) => {
        impl<K: $k $(+ $ks)* + IntoValue, V: IntoValue> IntoValue for $t {
            fn into_value(self) -> Value {
                let map = self.into_iter().map(|(k, v)| (k.into_value(), v.into_value())).collect();
                Value::Map(MapArc::new(map))
            }
        }
        impl<K: $k $(+ $ks)* + FromValue, V: FromValue> FromValue for $t {
            fn from_value(value: Value) -> Self {
                match value {
                    Value::Map(map) => map.map.lock().borrow().iter()
                        .map(|(k, v)| (K::from_value(k.clone()), V::from_value(v.clone())))
                        .collect(),
                    _ => unreachable!("HashMap::from_value called with non-map: {:?}", value),
                }
            }
        }
    }
}
impl_map! { HashMap<K, V> where K: Hash + Eq }
impl_map! { BTreeMap<K, V> where K: Ord }

pub fn add_map<'a, 'i>(diagnostics: &'i Diagnostics<ErrorCode>, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    meta_info.add_external_type::<Map<Value, Value>>(arena, diagnostics);
    meta_info.add_external_type::<MapEntry<Value, Value>>(arena, diagnostics);

    meta_info.add_external_function(arena, diagnostics, map_new);
    meta_info.add_external_function(arena, diagnostics, map_insert);
    meta_info.add_external_function(arena, diagnostics, map_get);
    meta_info.add_external_function(arena, diagnostics, map_get_or_insert);
    meta_info.add_external_function(arena, diagnostics, map_remove);
    meta_info.add_external_function(arena, diagnostics, map_keys);
    meta_info.add_external_function(arena, diagnostics, map_values);
    meta_info.add_external_function(arena, diagnostics, map_len);
    meta_info.add_external_function(arena, diagnostics, map_is_empty);
    meta_info.add_external_function(arena, diagnostics, map_entry);

    meta_info.add_external_function(arena, diagnostics, mapentry_or_insert);
}

#[rebo::function("Map::new")]
fn map_new<K, V>() -> Map<K, V> {
    Map::new(Vec::new())
}

#[rebo::function("Map::insert")]
fn map_insert<K, V>(this: Map<K, V>, key: K, value: V) -> Option<V> {
    let this = this.arc.map.lock();
    let mut this = this.borrow_mut();
    this.insert(key, value)
}

#[rebo::function("Map::get")]
fn map_get<K, V>(this: Map<K, V>, key: K) -> Option<V> {
    let this = this.arc.map.lock();
    let this = this.borrow();
    this.get(&key).cloned()
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
    this.remove(&key)
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
#[rebo::function("Map::is_empty")]
fn map_is_empty<K, V>(this: Map<K, V>) -> bool {
    this.arc.map.lock().borrow().is_empty()
}

#[rebo::function("Map::entry")]
fn map_entry<K, V>(this: Map<K, V>, key: K) -> MapEntry<K, V> {
    MapEntry { map: this, key }
}

#[derive(rebo::ExternalType)]
struct MapEntry<K, V> {
    map: Map<K, V>,
    key: K,
}

#[rebo::function("MapEntry::or_insert")]
fn mapentry_or_insert<K, V>(this: MapEntry<K, V>, value: V) -> V {
    let map = this.map.arc.map.lock();
    let mut map = map.borrow_mut();
    map.entry(this.key).or_insert(value).clone()
}
