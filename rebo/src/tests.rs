use crate::ReturnValue;

#[test]
fn boolean_short_circuiting() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
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
    "#.to_string()), ReturnValue::Ok);
}

#[test]
fn test_math_precedence() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        assert((1 + 2) * 3 == 9);
        assert(1 + 2 * 3 == 7);
        assert(1 * 2 + 3 == 5);
        assert(1 + (2 * 3) == 7);
        assert((1 * 2) + 3 == 5);
        assert(1 * (2 + 3) == 5);
        assert(1 + 2 * 3 * 4 == 25);
    "#.to_string()), ReturnValue::Ok);
}

#[test]
fn test_comparison() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
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
    "#.to_string()), ReturnValue::Ok);
}

#[test]
#[should_panic]
fn test_panic() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        panic("interesting message");
    "#.to_string()), ReturnValue::Ok);
}

#[test]
#[should_panic]
fn assert_false() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        assert(false);
    "#.to_string()), ReturnValue::Ok);
}
#[test]
fn assert_true() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        assert(true);
    "#.to_string()), ReturnValue::Ok);
}

#[test]
fn functions() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        fn foo(mut x: int, mut y: int) -> int {
            x = x + 10;
            y = y + 20;
            x + y
        }
        assert(foo(10, 20) == 60);
        fn bar(x: int) -> int { x }
        assert(bar(5) == 5);
        fn baz() {}
        assert(baz() == ());
    "#.to_string()), ReturnValue::Ok);
}
#[test]
fn function_diagnostics() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        fn takes_int(x: int) {}
        takes_int(2.0);

        fn add_one() -> () {}
        fn foo() -> int {}
        fn foo(mut x: int, mut y: int) -> int {
            x = x + 10;
            y = y + 20;
            x + y
        }
        fn bar() -> int { 2.0 }
        print(foo(10, 20));
        print(foo(5));
        let bar = 5;
        bar(5);
    "#.to_string()), ReturnValue::Diagnostics(8));
}
#[test]
fn struct_definitions() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        // struct definition
        struct Foo {
            foo: int,
        }

        // creation
        let foo = Foo { foo: 1337 };

        // field usage
        assert(foo.foo == 1337);

        // impl block
        impl Foo {
            fn new(foo: int) -> Foo {
                Foo { foo: foo }
            }
            fn foo(self) -> int {
                self.foo
            }
            fn bar(self) -> int {
                self.foo
            }
        }

        // method call
        let foo = Foo::new(42);
        assert(foo.foo == 42);
        assert(foo.foo() == 42);
        assert(foo.bar() == 42);
        assert(Foo::foo(foo) == 42);
        assert(Foo::bar(foo) == 42);
    "#.to_string()), ReturnValue::Ok);
}

#[test]
fn struct_diagnostics() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        // recursive struct definition
        struct Bar {
            bar: Bar,
        }
    "#.to_string()), ReturnValue::Diagnostics(1));
}
