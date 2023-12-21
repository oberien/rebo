use crate::{ReturnValue, Value};
pub use crate::tests::test;

#[test]
fn generator_yield() {
    test(r#"
        gen fn foo() {
           yield yield ();
        }
        let mut foo = foo();
        assert_eq(foo.next(), Option::Some(()));
        assert_eq(foo.next(), Option::Some(()));
        assert_eq(foo.next(), Option::None);
    "#, ReturnValue::Ok(Value::Unit));
}

#[test]
fn generator_block() {
    test(r#"
        gen fn foo() -> int {
           yield { yield 42; 1337 }
        }
        let mut foo = foo();
        assert_eq(foo.next(), Option::Some(42));
        assert_eq(foo.next(), Option::Some(1337));
        assert_eq(foo.next(), Option::None);
    "#, ReturnValue::Ok(Value::Unit));
}

#[test]
fn generator_bool_not() {
    test(r#"
        gen fn foo() -> bool {
           yield !!{
               yield !true;
               !false
           }
        }
        let mut foo = foo();
        assert_eq(foo.next(), Option::Some(false));
        assert_eq(foo.next(), Option::Some(true));
        assert_eq(foo.next(), Option::None);
    "#, ReturnValue::Ok(Value::Unit));
}

#[test]
fn generator_add() {
    test(r#"
        gen fn foo() -> int {
            // expr expr
            yield 1 + 3;
            // expr yield
            yield 10 + { yield 50; 30 };
            // yield expr
            yield { yield 200; 100 } + 300;
            // yield yield
            yield { yield 2000; 1000 } + { yield 5000; 3000 };
        }
        let bar = foo();
        assert_eq(bar.next(), Option::Some(4));
        assert_eq(bar.next(), Option::Some(50));
        assert_eq(bar.next(), Option::Some(40));
        assert_eq(bar.next(), Option::Some(200));
        assert_eq(bar.next(), Option::Some(400));
        assert_eq(bar.next(), Option::Some(2000));
        assert_eq(bar.next(), Option::Some(5000));
        assert_eq(bar.next(), Option::Some(4000));
        assert_eq(bar.next(), Option::None);
    "#, ReturnValue::Ok(Value::Unit));
}
