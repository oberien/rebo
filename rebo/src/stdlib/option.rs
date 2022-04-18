use std::borrow::Cow;
use std::cell::RefCell;
use diagnostic::Span;
use std::sync::Arc;
use parking_lot::lock_api::ReentrantMutex;
use crate::{CowVec, Enum, EnumArc, ExternalType, FileId, FromValue, IntoValue, SpecificType, Type, Typed, Value};

const FILE_NAME: &'static str = "external-Option.re";
const OPTION_T: Span = Span::new(FileId::synthetic(FILE_NAME), 12, 13);

impl<T: FromValue + IntoValue> ExternalType for Option<T> {
    const CODE: &'static str = r#"enum Option<T> {
    Some(T),
    None,
}

impl Option<T> {
    fn unwrap(self) -> T {
        match self {
            Option::Some(t) => t,
            Option::None => panic("tried to unwrap a None value"),
        }
    }
    fn unwrap_or(self, default: T) -> T {
        match self {
            Option::Some(t) => t,
            Option::None => default,
        }
    }
    fn or(self, other: Option<T>) -> Option<T> {
        match self {
            Option::Some(t) => self,
            Option::None => other,
        }
    }
    fn is_some(self) -> bool {
        match self {
            Option::Some(t) => true,
            Option::None => false,
        }
    }
    fn is_none(self) -> bool {
        !self.is_some()
    }
}
"#;
    const FILE_NAME: &'static str = FILE_NAME;
}
impl<T: FromValue> FromValue for Option<T> {
    fn from_value(value: Value) -> Self {
        match value {
            Value::Enum(e) => {
                let e = e.e.lock();
                let e = e.borrow();
                assert_eq!(e.name, "Option", "Option::from_value called with non-Option enum");
                match e.variant.as_str() {
                    "Some" => Some(FromValue::from_value(e.fields[0].clone())),
                    "None" => None,
                    variant => unreachable!("Option::from_value called with Option with unknown variant `{}`", variant),
                }
            }
            _ => unreachable!("Option::from_value called with non-enum"),
        }
    }
}
impl<T: IntoValue> IntoValue for Option<T> {
    fn into_value(self) -> Value {
        let (variant, fields) = match self {
            Some(t) => ("Some", vec![t.into_value()]),
            None => ("None", vec![]),
        };
        Value::Enum(EnumArc {
            e: Arc::new(ReentrantMutex::new(RefCell::new(Enum {
                name: "Option".to_string(),
                variant: variant.to_string(),
                fields,
            })))
        })
    }
}
impl<T> Typed for Option<T> {
    const TYPE: SpecificType = SpecificType::Enum(
        Cow::Borrowed("Option"),
        CowVec::Borrowed(&[(OPTION_T, Type::Top)]),
    );
}

#[cfg(test)]
mod test {
    use crate::ReturnValue;
    use crate::tests::test;

    #[test]
    fn option_or() {
        test(r#"
            let some0 = Option::Some(0);
            let some1 = Option::Some(1);
            let none = Option::None;
            assert(some0.or(some1) == some0);
            assert(some0.or(none) == some0);
            assert(some1.or(some0) == some1);
            assert(none.or(some0) == some0);
        "#, ReturnValue::Ok)
    }
}