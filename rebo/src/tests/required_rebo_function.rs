use rebo::Value;
use crate::{ReboConfig, ReturnValue};
use crate::tests::test_with_config;

#[test]
fn test_required_rebo_function_returns_int() {
    #[rebo::required_rebo_functions]
    extern "rebo" {
        fn foo() -> i32;
    }
    #[rebo::function(raw("bar"))]
    fn bar() {
        assert_eq!(foo(vm).unwrap(), 42);
    }
    test_with_config(
        ReboConfig::new()
            .add_function(bar)
            .add_required_rebo_function(foo),
        r#"
            fn foo() -> int {
                42
            }
            bar();
        "#,
        ReturnValue::Ok(Value::Unit),
    )
}

#[test]
fn test_required_rebo_function_returns_list() {
    #[rebo::required_rebo_functions]
    extern "rebo" {
        fn foo() -> Vec<i32>;
    }
    #[rebo::function(raw("bar"))]
    fn bar() {
        assert_eq!(foo(vm).unwrap(), vec![42]);
    }
    test_with_config(
        ReboConfig::new()
            .add_function(bar)
            .add_required_rebo_function(foo),
        r#"
            fn foo() -> List<int> {
                List::of(42)
            }
            bar();
        "#,
        ReturnValue::Ok(Value::Unit),
    )
}
