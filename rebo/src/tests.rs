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
        assert(true && true && true);

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
        assert(1 + 2 * 3 * 4 == 25);
    "#.to_string())
}

#[test]
fn test_comparison() {
    let _ = env_logger::builder().is_test(true).try_init();
    rebo::run("test".to_string(), r#"
        // unit
        assert(!(() < ()) && () <= () && () == () && () >= () && !(() > ()));
        // int
        assert(1 < 2 && !(2 < 2) && !(3 < 2));
        assert(1 <= 2 && 2 <= 2 && !(3 <= 2));
        assert(!(1 == 2) && 2 == 2 && !(3 == 2));
        assert(1 != 2 && !(2 != 2) && 3 != 2);
        assert(!(1 >= 2) && 2 >= 2 && 3 >= 2);
        assert(!(1 > 2) && !(2 > 2) && 3 > 2);
        // float
        assert(1. < 2. && !(2. < 2.) && !(3. < 2.));
        assert(1. <= 2. && 2. <= 2. && !(3. <= 2.));
        assert(!(1. ~~ 2.) && 2. ~~ 2. && !(3. ~~ 2.));
        assert(1. !~ 2. && !(2. !~ 2.) && 3. !~ 2.);
        assert(!(1. >= 2.) && 2. >= 2. && 3. >= 2.);
        assert(!(1. > 2.) && !(2. > 2.) && 3. > 2.);
        // string
        assert("a" < "b" && !("b" < "b") && !("c" < "b"));
        assert("a" <= "b" && "b" <= "b" && !("c" <= "b"));
        assert(!("a" == "b") && "b" == "b" && !("c" == "b"));
        assert("a" != "b" && !("b" != "b") && "c" != "b");
        assert(!("a" ~~ "b") && "b" ~~ "b" && !("c" ~~ "b") && "a" ~~ "A" && "ß" ~~ "ss");
        assert("a" !~ "b" && !("b" !~ "b") && "c" !~ "b" && !("a" !~ "A") && !("ß" !~ "ss"));
        assert(!("a" >= "b") && "b" >= "b" && "c" >= "b");
        assert(!("a" > "b") && !("b" > "b") && "c" > "b");
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
