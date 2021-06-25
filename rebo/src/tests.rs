#[test]
fn boolean_short_circuiting() {
    let _ = env_logger::builder().is_test(true).try_init();
    rebo::run("test".to_string(), r#"
        assert((true));
        assert(!(false));
        assert(!(true && false));
        assert((true && true));
        assert(!(false && true));
        assert((true || false));
        assert((false || false || true));
        assert((true || true && false));
        assert(!((true || true) && false));

        assert(!(false && panic("")));
        assert(true || panic(""));
    "#.to_string())
}

#[test]
fn test_math_precedence() {
    let _ = env_logger::builder().is_test(true).try_init();
    rebo::run("test".to_string(), r#"
        assert((1 + 2) * 3 == 9);
        assert(1 + 2 * 3 == 7);
        assert(1 * 2 + 3 == 5);
        assert(1 + (2 * 3) == 7);
        assert((1 * 2) + 3 == 5);
        assert(1 * (2 + 3) == 5);
    "#.to_string())
}

#[test]
#[should_panic]
fn test_panic() {
    let _ = env_logger::builder().is_test(true).try_init();
    rebo::run("test".to_string(), r#"
        panic("interesting message");
    "#.to_string())
}

#[test]
#[should_panic]
fn assert_false() {
    let _ = env_logger::builder().is_test(true).try_init();
    rebo::run("test".to_string(), r#"
        assert(false);
    "#.to_string())
}
#[test]
fn assert_true() {
    let _ = env_logger::builder().is_test(true).try_init();
    rebo::run("test".to_string(), r#"
        assert(true);
    "#.to_string())
}
