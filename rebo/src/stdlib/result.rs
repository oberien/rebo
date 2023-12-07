use std::borrow::Cow;
use std::cell::RefCell;
use std::sync::Arc;
use parking_lot::ReentrantMutex;
use crate::{CowVec, Enum, EnumArc, ExternalType, FileId, FromValue, IntoValue, Span, SpecificType, Type, Typed, Value};

const FILE_NAME: &str = "external-Result.re";
const RESULT_T: Span = Span::new(FileId::synthetic(FILE_NAME), 12, 13);
const RESULT_E: Span = Span::new(FileId::synthetic(FILE_NAME), 15, 16);

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
    const TYPE: SpecificType = SpecificType::Enum(
        Cow::Borrowed("Result"),
        CowVec::Borrowed(&[(RESULT_T, Type::Top), (RESULT_E, Type::Top)]),
    );
}
