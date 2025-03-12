use std::cell::RefCell;
use std::sync::{Arc, OnceLock};
use parking_lot::ReentrantMutex;
use serde_json::{Number, Value as JsonValue};
use rebo::ExternalType;
use crate::{Enum, EnumArc, FromValue, IntoValue, Map, SpecificType, Typed, Value};

impl ExternalType for JsonValue {
    const CODE: &'static str = r#"enum JsonValue {
    Null,
    Bool(bool),
    Number(float),
    String(string),
    Array(List<JsonValue>),
    Object(Map<string, JsonValue>),
}

impl JsonValue {
    fn unwrap_null(self) {
        match self {
            JsonValue::Null => (),
            _ => panic(f"called JsonValue::unwrap_null on non-Null value `{self}`"),
        }
    }
    fn unwrap_bool(self) -> bool {
        match self {
            JsonValue::Bool(b) => b,
            _ => panic(f"called JsonValue::unwrap_bool on non-Bool value `{self}`"),
        }
    }
    fn unwrap_number(self) -> float {
        match self {
            JsonValue::Number(num) => num,
            _ => panic(f"called JsonValue::unwrap_number on non-Number value `{self}`"),
        }
    }
    fn unwrap_string(self) -> string {
        match self {
            JsonValue::String(s) => s,
            _ => panic(f"called JsonValue::unwrap_string on non-String value `{self}`"),
        }
    }
    fn unwrap_array(self) -> List<JsonValue> {
        match self {
            JsonValue::Array(list) => list,
            _ => panic(f"called JsonValue::unwrap_array on non-Array value `{self}`"),
        }
    }
    fn unwrap_object(self) -> Map<string, JsonValue> {
        match self {
            JsonValue::Object(map) => map,
            _ => panic(f"called JsonValue::unwrap_object on non-Object value `{self}`"),
        }
    }
}
"#;
    const FILE_NAME: &'static str = "external-JsonValue.re";
}
impl FromValue for JsonValue {
    fn from_value(value: Value) -> Self {
        match value {
            Value::Enum(e) => {
                let e = e.e.lock();
                let e = e.borrow();
                assert_eq!(e.name, "JsonValue", "JsonValue::from_value called with non-JsonValue enum");
                match e.variant.as_str() {
                    "Null" => JsonValue::Null,
                    "Bool" => JsonValue::Bool(e.fields[0].clone().expect_bool("JsonValue::Bool with inner non-bool")),
                    "Number" => {
                        let float = e.fields[0].clone().expect_float("JsonValue::Number with inner non-f64").0;
                        JsonValue::Number(Number::from_f64(float).expect("invalid JSON float"))
                    },
                    "String" => JsonValue::String(e.fields[0].clone().expect_string("JsonValue::String with inner non-string")),
                    "Array" => JsonValue::Array(e.fields[0].clone().expect_list("JsonValue::Array with inner non-list").clone_list()),
                    "Object" => JsonValue::Object(e.fields[0].clone().expect_map("JsonValue::Object with inner non-map").clone_map()),
                    variant => unreachable!("JsonValue::from_value called with JsonValue with unknown variant `{}`", variant),
                }
            }
            _ => unreachable!("JsonValue::from_value called with non-enum"),
        }
    }
}
impl IntoValue for JsonValue {
    fn into_value(self) -> Value {
        let (variant, variant_index, fields) = match self {
            JsonValue::Null => ("Null", 0, vec![]),
            JsonValue::Bool(b) => ("Bool", 1, vec![b.into_value()]),
            JsonValue::Number(num) => ("Number", 2, vec![num.as_f64().into_value()]),
            JsonValue::String(s) => ("String", 3, vec![s.into_value()]),
            JsonValue::Array(array) => ("Array", 4, vec![array.into_value()]),
            JsonValue::Object(map) => ("Object", 5, vec![Map::new(map).into_value()]),
        };
        Value::Enum(EnumArc {
            e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
                name: "JsonValue".to_string(),
                variant_index,
                variant: variant.to_string(),
                fields,
            })))
        })
    }
}
impl Typed for JsonValue {
    fn typ() -> SpecificType {
        static TYPE: OnceLock<SpecificType> = OnceLock::new();
        TYPE.get_or_init(|| {
            SpecificType::Enum(
                "JsonValue".to_string(),
                vec![],
            )
        }).clone()
    }
}

#[cfg(test)]
mod test {
    use crate::{ReturnValue, Value};
    use crate::tests::test;

    #[test]
    fn json_value() {
        test(r#"
            let null = JsonValue::Null;
            assert(null.unwrap_null() == ());
            let boolean = JsonValue::Bool(true);
            assert(boolean.unwrap_bool() == true);
            let number = JsonValue::Number(13.37);
            assert(number.unwrap_number() == 13.37);
            let s = JsonValue::String("uiae");
            assert(s.unwrap_string() == "uiae");
            let array = JsonValue::Array(List::of(JsonValue::Null, JsonValue::Number(42.)));
            assert(array.unwrap_array().get(0).unwrap() == JsonValue::Null);
            assert(array.unwrap_array().get(1).unwrap() == JsonValue::Number(42.));
            assert(array.unwrap_array().get(2).is_none());
            let mut map = Map::new();
            map.insert("key1", JsonValue::Null);
            map.insert("key2", JsonValue::Number(21.));
            let object = JsonValue::Object(map);
            assert(object.unwrap_object().get("key1").unwrap() == JsonValue::Null);
            assert(object.unwrap_object().get("key2").unwrap() == JsonValue::Number(21.));
            assert(object.unwrap_object().get("key3").is_none());
        "#, ReturnValue::Ok(Value::Unit))
    }
}
