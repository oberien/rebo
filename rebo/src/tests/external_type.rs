use rebo::{ReboConfig, ReturnValue, Value};
use rebo::common::{FunctionValue, FuzzyFloat, TypedFunctionValue};
use crate::tests::test_with_config;

#[test]
fn test_external_type_struct_int() {
    #[derive(rebo::ExternalType)]
    struct Foo {
        i: i64,
    }
    test_with_config(
        ReboConfig::new()
            .add_external_type(Foo),
        r#"
            let foo = Foo { i: 1337 };
            foo.i
        "#,
        ReturnValue::Ok(Value::Integer(1337)),
    );
}

#[test]
fn test_external_type_struct_float() {
    #[derive(rebo::ExternalType)]
    struct Foo {
        f: f64,
    }
    test_with_config(
        ReboConfig::new()
            .add_external_type(Foo),
        r#"
            let foo = Foo { f: 6.9 };
            foo.f
        "#,
        ReturnValue::Ok(Value::Float(FuzzyFloat(6.9))),
    );
}

#[test]
fn test_external_type_struct_string() {
    #[derive(rebo::ExternalType)]
    struct Foo {
        s: String,
    }
    test_with_config(
        ReboConfig::new()
            .add_external_type(Foo),
        r#"
            let foo = Foo { s: "42" };
            foo.s
        "#,
        ReturnValue::Ok(Value::String("42".to_string())),
    );
}

#[test]
fn test_external_type_struct_nested() {
    #[derive(rebo::ExternalType)]
    struct Foo {
        s: String,
    }
    #[derive(rebo::ExternalType)]
    struct Bar {
        foo: Foo,
    }
    test_with_config(
        ReboConfig::new()
            .add_external_type(Foo)
            .add_external_type(Bar),
        r#"
            let bar = Bar { foo: Foo { s: "42" } };
            bar.foo.s
        "#,
        ReturnValue::Ok(Value::String("42".to_string())),
    );
}

#[test]
fn test_external_type_struct_functionvalue() {
    #[derive(rebo::ExternalType)]
    struct Foo {
        f: TypedFunctionValue<fn(i32) -> String>,
    }
    #[rebo::function(raw("external_foo"))]
    fn external_foo(foo: Foo) {
        let ret = vm.call_typed_function(foo.f, 5, expr_span).unwrap();
        assert_eq!(ret, "10");
    }
    test_with_config(
        ReboConfig::new()
            .add_function(external_foo)
            .add_external_type(Foo),
        r#"
            struct Int { val: int };
            let mut x = Int { val: 4 };
            let foo = Foo { f: fn(i: int) -> string { x.val += 1; f"{i + x.val}" } };
            external_foo(foo);
            let f = foo.f;
            f(6)
        "#,
        ReturnValue::Ok(Value::String("12".to_string())),
    );
}
