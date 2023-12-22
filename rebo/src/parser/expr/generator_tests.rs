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

fn test_binop(sym: char, f: impl Fn(i64, i64) -> i64) {
    let a = f(1, 3);
    let b = f(10, 30);
    let c = f(100, 300);
    let d = f(1000, 3000);
    test(&format!(r#"
        gen fn foo() -> int {{
            // expr expr
            yield 1 {sym} 3;
            // expr yield
            yield 10 {sym} {{ yield 50; 30 }};
            // yield expr
            yield {{ yield 200; 100 }} {sym} 300;
            // yield yield
            yield {{ yield 2000; 1000 }} {sym} {{ yield 5000; 3000 }};
        }}
        let bar = foo();
        assert_eq(bar.next(), Option::Some({a}));
        assert_eq(bar.next(), Option::Some(50));
        assert_eq(bar.next(), Option::Some({b}));
        assert_eq(bar.next(), Option::Some(200));
        assert_eq(bar.next(), Option::Some({c}));
        assert_eq(bar.next(), Option::Some(2000));
        assert_eq(bar.next(), Option::Some(5000));
        assert_eq(bar.next(), Option::Some({d}));
        assert_eq(bar.next(), Option::None);
    "#), ReturnValue::Ok(Value::Unit));
}
#[test]
fn generator_add() {
    test_binop('+', |a, b| a + b);
}
#[test]
fn generator_sub() {
    test_binop('-', |a, b| a - b);
}
#[test]
fn generator_mul() {
    test_binop('*', |a, b| a * b);
}
#[test]
fn generator_div() {
    test_binop('/', |a, b| a / b);
}
#[test]
fn generator_mod() {
    test_binop('%', |a, b| a % b);
}
#[test]
fn generator_xor() {
    test_binop('^', |a, b| a ^ b);
}
fn test_bool_binop(sym: &str, f: impl Fn(bool, bool) -> bool) {
    let a = f(true, false);
    test(&format!(r#"
        gen fn foo() -> bool {{
            // expr expr
            yield true {sym} false;
            // expr yield
            yield true {sym} {{ yield true; false }};
            // yield expr
            yield {{ yield false; true }} {sym} false;
            // yield yield
            yield {{ yield true; true }} {sym} {{ yield false; false }};
        }}
        let bar = foo();
        assert_eq(bar.next(), Option::Some({a}));
        assert_eq(bar.next(), Option::Some(true));
        assert_eq(bar.next(), Option::Some({a}));
        assert_eq(bar.next(), Option::Some(false));
        assert_eq(bar.next(), Option::Some({a}));
        assert_eq(bar.next(), Option::Some(true));
        assert_eq(bar.next(), Option::Some(false));
        assert_eq(bar.next(), Option::Some({a}));
        assert_eq(bar.next(), Option::None);
    "#), ReturnValue::Ok(Value::Unit));
}
#[test]
fn generator_bool_and() {
    test_bool_binop("&&", |a, b| a && b);
}
#[test]
fn generator_bool_or() {
    test_bool_binop("||", |a, b| a || b);
}

fn test_compare_binop(sym: &str, f: impl Fn(i64, i64) -> bool) {
    let a = f(1, 3);
    let b = f(10, 30);
    let c = f(100, 300);
    let d = f(1000, 3000);
    test(&format!(r#"
        gen fn foo() -> bool {{
            // expr expr
            yield 1 {sym} 3;
            // expr yield
            yield 10 {sym} {{ yield true; 30 }};
            // yield expr
            yield {{ yield false; 100 }} {sym} 300;
            // yield yield
            yield {{ yield true; 1000 }} {sym} {{ yield false; 3000 }};
        }}
        let bar = foo();
        assert_eq(bar.next(), Option::Some({a}));
        assert_eq(bar.next(), Option::Some(true));
        assert_eq(bar.next(), Option::Some({b}));
        assert_eq(bar.next(), Option::Some(false));
        assert_eq(bar.next(), Option::Some({c}));
        assert_eq(bar.next(), Option::Some(true));
        assert_eq(bar.next(), Option::Some(false));
        assert_eq(bar.next(), Option::Some({d}));
        assert_eq(bar.next(), Option::None);
    "#), ReturnValue::Ok(Value::Unit));
}
#[test]
fn generator_less_than() {
    test_compare_binop("<", |a, b| a < b);
}
#[test]
fn generator_less_equals() {
    test_compare_binop("<=", |a, b| a <= b);
}
#[test]
fn generator_equals() {
    test_compare_binop("==", |a, b| a == b);
}
#[test]
fn generator_not_equals() {
    test_compare_binop("!=", |a, b| a != b);
}
#[test]
fn generator_greater_equals() {
    test_compare_binop(">=", |a, b| a >= b);
}
#[test]
fn generator_greater_than() {
    test_compare_binop(">", |a, b| a > b);
}
