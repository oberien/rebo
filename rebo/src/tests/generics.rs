use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

#[test]
fn generics() {
    test(r#"
        enum MyOption<T> {
            Some(T),
            None,
        }
        impl MyOption<T> {
            fn unwrap(self) -> T {
                match self {
                    MyOption::Some(t) => t,
                    MyOption::None => panic("tried to unwrap a None value"),
                }
            }
        }
        let a = MyOption::Some(42);
        let b = MyOption::Some("uiae");
        let c: MyOption<float> = MyOption::None;
        print(a.unwrap(), b.unwrap(), c);

        struct Foo<T> {
            t: T,
        }
        struct Bar<A, B, C> {
            a: A,
            b: B,
            c: C,
        }

        fn foo<U, V>(u: U, v: V) -> V { v }
        fn bar<T>(t: T) -> T { foo(42, t) }
    "#, ReturnValue::Ok(Value::Unit));
}
#[test]
fn generic_diagnostics() {
    test(r#"
        // T and V are not unifyable, because V is int
        fn foo<U, V>(u: U, v: V) -> V { v }
        fn bar<T>(t: T) -> T { foo(t, 42) }
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::UnableToInferType)]));
}
