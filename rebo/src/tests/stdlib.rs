use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

#[test]
fn test_panic() {
    test(r#"
        panic("interesting message");
    "#, ReturnValue::Panic);
}

#[test]
fn assert_false() {
    test(r#"
        assert(false);
    "#, ReturnValue::Panic);
}
#[test]
fn assert_true() {
    test(r#"
        assert(true);
    "#, ReturnValue::Ok(Value::Unit));
}

#[test]
fn test_clone() {
    test(r#"
        struct Foo { x: int }
        let mut a = Foo { x: 0 };
        let mut b = a;
        let mut c = a.clone();
        let mut d = c;
        assert_eq(0, a.x);
        assert_eq(0, b.x);
        assert_eq(0, c.x);
        assert_eq(0, d.x);
        a.x = 1;
        assert_eq(1, a.x);
        assert_eq(1, b.x);
        assert_eq(0, c.x);
        assert_eq(0, d.x);
        b.x = 2;
        assert_eq(2, a.x);
        assert_eq(2, b.x);
        assert_eq(0, c.x);
        assert_eq(0, d.x);
        d.x = 3;
        assert_eq(2, a.x);
        assert_eq(2, b.x);
        assert_eq(3, c.x);
        assert_eq(3, d.x);
    "#, ReturnValue::Ok(Value::Unit));
}

#[test]
fn redefine_external_types() {
    test(r#"
        // external type Option redefined
        enum Option<T> {}
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::DuplicateGlobal)]));
}
