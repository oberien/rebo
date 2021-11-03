use diagnostic::{Diagnostics, Span};
use typed_arena::Arena;
use crate::parser::{Expr, Parser};
use crate::common::{MetaInfo, Value, ExternalFunction, ListArc, EnumArc, Enum, MapArc};
use crate::lexer::Lexer;
use crate::vm::VmContext;
use crate::typeck::types::{FunctionType, Type, SpecificType};
use std::borrow::Cow;
use parking_lot::ReentrantMutex;
use std::sync::Arc;
use std::cell::RefCell;
use std::collections::BTreeMap;

pub fn add_map<'a, 'i>(diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>, option_t: Span, list_t: Span) {
    let code = "struct Map<K, V> {}".to_string();
    let (file, _) = diagnostics.add_file("map.rs".to_string(), code);

    let lexer = Lexer::new(diagnostics, file);
    let parser = Parser::new(arena, lexer, diagnostics, meta_info);
    let ast = parser.parse_ast().unwrap();
    assert_eq!(ast.exprs.len(), 1);

    let struct_def = match ast.exprs[0] {
        Expr::StructDefinition(struct_def) => struct_def,
        _ => unreachable!(),
    };
    let mut generics = struct_def.generics.as_ref().unwrap().generics.as_ref().unwrap().iter();
    let map_k = generics.next().unwrap().def_ident.span;
    let map_v = generics.next().unwrap().def_ident.span;
    assert!(generics.next().is_none());

    meta_info.add_external_function(diagnostics, "Map::new", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![map_k, map_v]),
            args: Cow::Borrowed(&[]),
            ret: Type::Specific(SpecificType::Struct("Map".to_string(), vec![(map_k, Type::Top), (map_v, Type::Top)]))
        },
        imp: map_new,
    });
    meta_info.add_external_function(diagnostics, "Map::insert", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![map_k, map_v]),
            args: Cow::Owned(vec![
                Type::Specific(SpecificType::Struct("Map".to_string(), vec![(map_k, Type::Top), (map_v, Type::Top)])),
                Type::Specific(SpecificType::Generic(map_k)),
                Type::Specific(SpecificType::Generic(map_v)),
            ]),
            ret: Type::Specific(SpecificType::Enum("Option".to_string(), vec![(option_t, Type::Specific(SpecificType::Generic(map_v)))])),
        },
        imp: map_insert,
    });
    meta_info.add_external_function(diagnostics, "Map::get", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![map_k, map_v]),
            args: Cow::Owned(vec![
                Type::Specific(SpecificType::Struct("Map".to_string(), vec![(map_k, Type::Top), (map_v, Type::Top)])),
                Type::Specific(SpecificType::Generic(map_k)),
            ]),
            ret: Type::Specific(SpecificType::Enum("Option".to_string(), vec![(option_t, Type::Specific(SpecificType::Generic(map_v)))])),
        },
        imp: map_get,
    });
    meta_info.add_external_function(diagnostics, "Map::remove", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![map_k, map_v]),
            args: Cow::Owned(vec![
                Type::Specific(SpecificType::Struct("Map".to_string(), vec![(map_k, Type::Top), (map_v, Type::Top)])),
                Type::Specific(SpecificType::Generic(map_k)),
            ]),
            ret: Type::Specific(SpecificType::Enum("Option".to_string(), vec![(option_t, Type::Specific(SpecificType::Generic(map_v)))])),
        },
        imp: map_remove,
    });
    meta_info.add_external_function(diagnostics, "Map::keys", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![map_k, map_v]),
            args: Cow::Owned(vec![
                Type::Specific(SpecificType::Struct("Map".to_string(), vec![(map_k, Type::Top), (map_v, Type::Top)])),
            ]),
            ret: Type::Specific(SpecificType::Enum("List".to_string(), vec![(list_t, Type::Specific(SpecificType::Generic(map_k)))])),
        },
        imp: map_keys,
    });
    meta_info.add_external_function(diagnostics, "Map::values", ExternalFunction {
        typ: FunctionType {
            generics: Cow::Owned(vec![map_k, map_v]),
            args: Cow::Owned(vec![
                Type::Specific(SpecificType::Struct("Map".to_string(), vec![(map_k, Type::Top), (map_v, Type::Top)])),
            ]),
            ret: Type::Specific(SpecificType::Enum("List".to_string(), vec![(list_t, Type::Specific(SpecificType::Generic(map_v)))])),
        },
        imp: map_values,
    });
}

fn map_new(_expr_span: Span, _vm: &mut VmContext, _values: Vec<Value>) -> Value {
    Value::Map(MapArc { map: Arc::new(ReentrantMutex::new(RefCell::new(BTreeMap::new()))) })
}

fn map_insert(_expr_span: Span, _vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    let map = values.remove(0).expect_map("Map::insert called with non-map self argument");
    let key = values.remove(0);
    let value = values.remove(0);
    let map = map.map.lock();
    let (variant, fields) = match map.borrow_mut().insert(key, value) {
        Some(v) => ("Some".to_string(), vec![v.clone()]),
        None => ("None".to_string(), vec![]),
    };
    Value::Enum(EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
        name: "Option".to_string(),
        variant,
        fields,
    })))})
}

fn map_get(_expr_span: Span, _vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    let map = values.remove(0).expect_map("Map::get called with non-map self argument");
    let key = values.remove(0);
    let map = map.map.lock();
    let (variant, fields) = match map.borrow().get(&key) {
        Some(v) => ("Some".to_string(), vec![v.clone()]),
        None => ("None".to_string(), vec![]),
    };
    Value::Enum(EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
        name: "Option".to_string(),
        variant,
        fields,
    })))})
}

fn map_remove(_expr_span: Span, _vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    let map = values.remove(0).expect_map("Map::remove called with non-map self argument");
    let key = values.remove(0);
    let map = map.map.lock();
    let (variant, fields) = match map.borrow_mut().remove(&key) {
        Some(v) => ("Some".to_string(), vec![v]),
        None => ("None".to_string(), vec![]),
    };
    Value::Enum(EnumArc { e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
        name: "Option".to_string(),
        variant,
        fields,
    })))})
}

fn map_keys(_expr_span: Span, _vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    let map = values.remove(0).expect_map("Map::keys called with non-map self argument");
    let map = map.map.lock();
    let keys = map.borrow().keys().cloned().collect();
    Value::List(ListArc { list: Arc::new(ReentrantMutex::new(RefCell::new(keys))) })
}

fn map_values(_expr_span: Span, _vm: &mut VmContext, mut values: Vec<Value>) -> Value {
    let map = values.remove(0).expect_map("Map::values called with non-map self argument");
    let map = map.map.lock();
    let values = map.borrow().values().cloned().collect();
    Value::List(ListArc { list: Arc::new(ReentrantMutex::new(RefCell::new(values))) })
}
