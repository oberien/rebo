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
        assert(!(1. >= 2.) && 2. >= 2. && 3. >= 2.);
        assert(!(1. > 2.) && !(2. > 2.) && 3. > 2.);
        // string
        assert("a" < "b" && !("b" < "b") && !("c" < "b"));
        assert("a" <= "b" && "b" <= "b" && !("c" <= "b"));
        assert(!("a" == "b") && "b" == "b" && !("c" == "b"));
        assert("a" != "b" && !("b" != "b") && "c" != "b");
        assert(!("a" >= "b") && "b" >= "b" && "c" >= "b");
        assert(!("a" > "b") && !("b" > "b") && "c" > "b");

        // bindings
        let foo = 1337;
        assert(foo == 1337);
        let bar = 1337;
        assert(foo == bar);
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
fn pre_parsed() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        2 + fn foo() {} && true
    "#.to_string()), ReturnValue::Diagnostics(6));
}
#[test]
fn if_else_usage() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        if true {} else { panic(""); }
        if true {} else if true { panic(""); }
        if true {} else if true { panic(""); } else { panic(""); }
        if false { panic(""); } else if true {} else { panic(""); }
        if false { panic(""); } else if false { panic(""); } else {}
        assert(if true { true } else { false });
        assert(1342 == if true { 1337 } else { panic("") } + 5);
    "#.to_string()), ReturnValue::Ok);
}
#[test]
fn if_else_diagnostics() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        if true { 1337 } else { "" }
        if (true) {} else {}
        if true { 1337 }
        if true { 1337 } else {}
        if true { 1337 } else { 42; }
        if 1337 {}
    "#.to_string()), ReturnValue::Diagnostics(8));
}
#[test]
fn match_usage() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        assert(match true { true => (), false => panic("") } == ());
        assert(match false { true => panic(""), _ => () } == ());
        assert(match false { true => panic(""), foo => assert(!foo) } == ());
        assert(match true { true => (), _ => panic("") } == ());
        assert(match true { true => (), foo => panic("") } == ());
        assert(match 1 { 1 => (), _ => panic("") } == ());
        match 1 { 0 => panic(""), foo => assert(foo == 1) };
        assert(match "uiae" { "uiae" => (), _ => panic("") } == ());
        assert(match "uiae" { "foo" => panic(""), _ => () } == ());
        assert(match () { () => () } == ());
        assert(match () { _ => () } == ());
        assert(match () { () => 1337 } == 1337);
        assert(match "foo" {
            "bar" => 1,
            "baz" => 2,
            whatever => match whatever {
                "foo" => 42,
                _ => 1337,
            },
        } == 42);
    "#.to_string()), ReturnValue::Ok);
}
#[test]
fn match_diagnostics() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        // empty body + non-exhaustive
        match true {}
        // unreachable
        match true { _ => (), foo => {} }
        // float match
        match 1.0 { _ => () }
        // missing catchall
        match 1 { 1 => () }
        // struct match
        struct Foo {}
        match Foo {} { _ => () }
        // non-exhaustive
        match true { true => (), }
    "#.to_string()), ReturnValue::Diagnostics(7));
}
#[test]
fn while_usage() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        let mut i = 0;
        while i < 3 {
            i = i + 1;
        }
        assert(i == 3);
    "#.to_string()), ReturnValue::Ok);
}
#[test]
fn while_diagnostics() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        while (true) {}
        while 1337 {}
    "#.to_string()), ReturnValue::Diagnostics(3));
}
#[test]
fn format_strings() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        let foo = 42;
        assert(f"uiae" == "uiae");
        assert(f"{foo}" == "42");
        assert(f"{4 * 10 + 2}" == "42");
        assert(f"\{\}" == "{}");
        assert(f"{f"42"}" == "42");
    "#.to_string()), ReturnValue::Ok);
}
#[test]
fn format_string_diagnostics1() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        f"}";
        f"uiae {";
    "#.to_string()), ReturnValue::Diagnostics(3));
}
#[test]
fn format_string_diagnostics2() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        f"uiae
    "#.to_string()), ReturnValue::Diagnostics(1));
}
#[test]
fn struct_definitions() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        // struct definition
        struct Empty {}
        struct Foo {
            foo: int,
        }
        struct Bar {
            foo: Foo,
            bar: bool,
        }

        // initialization
        let empty = Empty {};
        let empty2 = Empty {  };
        assert(empty == empty2);
        let mut foo = Foo { foo: 1337 };
        let foo2 = Foo { foo: 1337, };
        assert(foo == foo2);
        let bar = Bar { foo: foo, bar: true };
        let bar2 = Bar { foo: foo2, bar: false };
        let bar3 = Bar { foo: Foo { foo: 1337 }, bar: false };
        assert(bar != bar2);
        assert(bar2 == bar3);

        // field usage
        assert(foo.foo == 1337);
        // field assignment
        foo.foo = 42;
        assert(foo.foo == 42);

        // // impl block
        // impl Foo {
        //     fn new(foo: int) -> Foo {
        //         Foo { foo: foo }
        //     }
        //     fn foo(self) -> int {
        //         self.foo
        //     }
        //     fn bar(self) -> int {
        //         self.foo
        //     }
        // }

        // // method call
        // let foo = Foo::new(42);
        // assert(foo.foo == 42);
        // assert(foo.foo() == 42);
        // assert(foo.bar() == 42);
        // assert(Foo::foo(foo) == 42);
        // assert(Foo::bar(foo) == 42);
    "#.to_string()), ReturnValue::Ok);
}

#[test]
fn struct_diagnostics() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        // duplicate struct name
        struct Foo {}
        // recursive struct definition
        struct Foo {
            foo: Foo,
        }

        // mutual recursive struct definition
        struct Foo2 {
            foo: Foo3,
        }
        struct Foo3 {
            foo: Foo2,
        }

        struct Bar {
            i: int,
        }

        // missing struct field
        let bar = Bar {};

        // unknown struct field
        Bar {
            i: 1337,
            uiae: 42,
        };

        // compare between different struct types
        struct Baz {}
        struct Qux {}
        let baz = Baz {};
        let qux = Qux {};
        baz == qux;

        // mutable access to non-mutable variable
        bar.i = 42;
    "#.to_string()), ReturnValue::Diagnostics(9));
}

#[test]
fn associated_functions() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        struct Foo {
            a: int,
            b: string,
        }
        impl Foo {
            fn new(a: int, b: string) -> Foo {
                Foo { a: a, b: b }
            }

            fn foo(a: int) -> int {
                a + 10
            }
        }
        assert(Foo::new(42, "uiae") == Foo { a: 42, b: "uiae", });
        assert(Foo::foo(42) == 52);
    "#.to_string()), ReturnValue::Ok);
}
#[test]
fn associated_function_diagnostics() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        struct Foo {}
        impl Foo {}
    "#.to_string()), ReturnValue::Diagnostics(1));
}
#[test]
fn associated_function_diagnostics2() {
    let _ = env_logger::builder().is_test(true).try_init();
    assert_eq!(rebo::run("test".to_string(), r#"
        impl Foo {
            fn foo() {}
        }
    "#.to_string()), ReturnValue::Diagnostics(1));
}
