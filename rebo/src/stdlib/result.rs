use std::cell::RefCell;
use std::sync::{Arc, OnceLock};
use parking_lot::ReentrantMutex;
use rebo::common::SpanWithId;
use crate::{Enum, EnumArc, ExternalType, FileId, FromValue, IntoValue, SpecificType, Type, Typed, Value};

const FILE_NAME: &str = "external-Result.re";

impl<T: FromValue + IntoValue, E: FromValue + IntoValue> ExternalType for Result<T, E> {
    const CODE: &'static str = r#"enum Result<T, E> {
    Ok(T),
    Err(E),
}

impl Result<T, E> {
    fn unwrap(self) -> T {
        match self {
            Result::Ok(t) => t,
            Result::Err(e) => panic(f"tried to unwrap an error: {e}"),
        }
    }
    fn is_ok(self) -> bool {
        match self {
            Result::Ok(t) => true,
            Result::Err(e) => false,
        }
    }
    fn is_err(self) -> bool {
        match self {
            Result::Ok(t) => false,
            Result::Err(e) => true,
        }
    }
}
"#;
    const FILE_NAME: &'static str = FILE_NAME;
}
impl<T: FromValue, E: FromValue> FromValue for Result<T, E> {
    fn from_value(value: Value) -> Self {
        match value {
            Value::Enum(e) => {
                let e = e.e.lock();
                let e = e.borrow();
                assert_eq!(e.name, "Result", "Result::from_value called with non-Result enum");
                match e.variant.as_str() {
                    "Ok" => Ok(FromValue::from_value(e.fields[0].clone())),
                    "Err" => Err(FromValue::from_value(e.fields[0].clone())),
                    variant => unreachable!("Result::from_value called with Result with unknown variant `{}`", variant),
                }
            }
            _ => unreachable!("Result::from_value called with non-enum"),
        }
    }
}
impl<T: IntoValue, E: IntoValue> IntoValue for Result<T, E> {
    fn into_value(self) -> Value {
        let (variant, variant_index, fields) = match self {
            Ok(t) => ("Ok", 0, vec![t.into_value()]),
            Err(e) => ("Err", 1, vec![e.into_value()]),
        };
        Value::Enum(EnumArc {
            e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
                name: "Result".to_string(),
                variant_index,
                variant: variant.to_string(),
                fields,
            })))
        })
    }
}
impl<T, E> Typed for Result<T, E> {
    fn typ() -> SpecificType {
        static TYP: OnceLock<SpecificType> = OnceLock::new();
        TYP.get_or_init(|| {
            SpecificType::Enum(
                "Result".to_string(),
                vec![
                    (SpanWithId::new(FileId::synthetic_named(FILE_NAME), 12, 13), Type::Top),
                    (SpanWithId::new(FileId::synthetic_named(FILE_NAME), 15, 16), Type::Top),
                ],
            )
        }).clone()
    }
}
