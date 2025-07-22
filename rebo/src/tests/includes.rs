use rebo::{ReboConfig, Value};
use rebo::tests::test_with_config;
use crate::{IncludeConfig, ReturnValue};

#[test]
fn test_includes() {
    test_with_config(
        ReboConfig::new()
            .include_config(IncludeConfig::DisallowFromFiles)
            .add_external_include("test-include.rs", r#"
                struct MyStruct {
                    x: int,
                }
                enum MyEnum {
                    Variant1(int),
                }
                static FOO: int = 1337;
                fn identity(x: int) -> int {
                    x
                }
                5
            "#),
        r#"
            assert_eq(include "test-include.rs", 5);
            assert_eq(FOO, 1337);
            assert_eq(identity(42), 42);
            let foo = MyStruct { x: 21 };
            assert_eq(foo.x, 21);
            let foo = MyEnum::Variant1(7);
            match foo {
                MyEnum::Variant1(x) => assert_eq(x, 7),
            }
        "#,
        ReturnValue::Ok(Value::Unit)
    )
}