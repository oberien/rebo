#[test]
fn boolean_exprs() {
    rebo::run("test", r#"
        assert((true));
        assert(!(false));
        assert(!(true && false));
        assert((true && true));
        assert(!(false && true));
        assert((true || false));
        assert((false || false || true));
        assert((true || true && false));
        assert(!((true || true) && false));
    "#.to_string())
}

#[test]
#[should_panic]
fn test_panic() {
    rebo::run("test", r#"
        panic("interesting message");
    "#.to_string())
}

#[test]
#[should_panic]
fn assert_false() {
    rebo::run("test", r#"
        assert(false);
    "#.to_string())
}
#[test]
fn assert_true() {
    rebo::run("test", r#"
        assert(true);
    "#.to_string())
}
