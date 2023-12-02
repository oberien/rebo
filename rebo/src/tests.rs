use diagnostic::Emitted;
use rebo::ErrorCode;
use crate::ReturnValue;

trait Sorted {
    fn sorted(&self) -> Self;
}
impl<T: Ord + Clone> Sorted for Vec<T> {
    fn sorted(&self) -> Self {
        let mut v = self.clone();
        v.sort();
        v
    }
}
impl Sorted for ReturnValue {
    fn sorted(&self) -> Self {
        match self {
            ReturnValue::Ok(_) => ReturnValue::Ok,
            ReturnValue::Diagnostics(diags) => ReturnValue::Diagnostics(diags.sorted()),
            ReturnValue::Panic => ReturnValue::Panic,
            ReturnValue::ParseError => ReturnValue::ParseError,
        }
    }
}

pub fn test(code: &str, expected: ReturnValue) {
    let _ = env_logger::builder().is_test(true).try_init();
    let res = rebo::run("test".to_string(), code.to_string());
    assert_eq!(expected.sorted(), res.return_value.sorted());
}

#[test]
fn other_stuff() {
    test(r#"
        // "integer" should not be parsed as Keyword("int"),Ident("eger") but as Ident("integer")
        let integer = 1337;
    "#, ReturnValue::Ok);
}
#[test]
fn other_stuff_diagnostics() {
    test(r#"
        // immutable assign
        let foo = 1;
        foo = 42;
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::ImmutableAssign)]));
}

#[test]
fn boolean_short_circuiting() {
    test(r#"
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
    "#, ReturnValue::Ok);
}

#[test]
fn test_math_precedence() {
    test(r#"
        assert((1 + 2) * 3 == 9);
        assert(1 + 2 * 3 == 7);
        assert(1 * 2 + 3 == 5);
        assert(1 + (2 * 3) == 7);
        assert((1 * 2) + 3 == 5);
        assert(1 * (2 + 3) == 5);
        assert(1 + 2 * 3 * 4 == 25);
    "#, ReturnValue::Ok);
}

#[test]
fn test_comparison() {
    test(r#"
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
    "#, ReturnValue::Ok);
}

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
    "#, ReturnValue::Ok);
}

#[test]
fn functions() {
    test(r#"
        fn foo(mut x: int, mut y: int) -> int {
            x = x + 10;
            y = y + 20;
            x + y
        }
        assert_eq(foo(10, 20), 60);
        fn bar(x: int) -> int { x }
        assert_eq(bar(5), 5);
        fn baz() {}
        assert_eq(baz(), ());

        // allow usage before definition
        fn a() { b() }
        fn b() {}

        // infer argument types correctly
        fn c(x: int) { print(x) }

        // pass by value with references
        struct Foo { x: int }
        let mut foo = Foo { x: 1 };
        fn change(mut foo: Foo) {
            foo.x = 2;
            foo = Foo { x: 3 };
        }
        assert_eq(foo.x, 1);
        change(foo);
        assert_eq(foo.x, 2);

        // closures capture copies of primitives
        let foo = 5;
        let closure = fn() {
            let mut foo = foo;
            assert_eq(5, foo);
            foo = 7;
            assert_eq(7, foo);
        };
        let mut foo = foo;
        foo = 8;
        closure();
        assert_eq(8, foo);
        // closures don't modify their internal state
        closure();

        // closures can modify fields of structs
        let mut foo = Foo { x: 5 };
        let closure = fn() {
            assert_eq(5, foo.x);
            foo.x = 7;
            assert_eq(7, foo.x);
        };
        closure();
        assert_eq(7, foo.x);
        foo.x = 5;
        closure();

        Option::None;
        fn quux() {
            print(Option::None);
        }

        let mut fns = List::new();
        for i in List::of(0,1,2) {
            fns.push(fn() -> int { i });
        }
        assert_eq({ let f = fns.get(0).unwrap(); f() }, 0);
        assert_eq({ let f = fns.get(1).unwrap(); f() }, 1);
        assert_eq({ let f = fns.get(2).unwrap(); f() }, 2);
    "#, ReturnValue::Ok);
}
#[test]
fn free_function_diagnostics() {
    test(r#"
        // overwrite external function
        fn add_one() -> () {}

        // empty body but returns int
        // unable to infer type ({} is not int)
        fn foo() -> int {}
        // overwrite existing function
        fn foo(mut x: int, mut y: int) -> int {
            x = x + 10;
            y = y + 20;
            x + y
        }

        fn takes_int(x: int) {}
        // wrong arg type (float instead of int)
        takes_int(2.0);

        // wrong returned type
        fn bar() -> int { 2.0 }
        // wrong number of arguments
        print(foo(5));
        // unknown function baz
        baz(5);

        struct Foo { x: int }
        // assignment to immutable variable
        fn change_broken(foo: Foo) {
            foo.x = 42;
        }
        fn change(mut foo: Foo) {
            foo.x = 42;
        }
        let foo = Foo { x: 1337 };
        change(foo);
        
        // named function can't capture variable foo
        let binding = ();
        fn qux() {
            binding
        }

        // can't capture mutable primitives
        let mut foo = 42;
        let closure = fn() { foo += 1 };
        closure();

        // can't modify immutably captured primitives
        let foo = 42;
        let closure = fn() { foo += 1 };
        closure();
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::DuplicateGlobal),
        Emitted::Error(ErrorCode::EmptyFunctionBody),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::DuplicateGlobal),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::InvalidNumberOfArguments),
        Emitted::Error(ErrorCode::UnknownFunction),
        Emitted::Error(ErrorCode::ImmutableAssign),
        Emitted::Error(ErrorCode::UnknownIdentifier),
        Emitted::Error(ErrorCode::NamedFunctionCapture),
        Emitted::Error(ErrorCode::ClosureCapturesMutablePrimitive),
        Emitted::Error(ErrorCode::ImmutableAssign),
    ]));
}
#[test]
fn pre_parsed() {
    test(r#"
        // type conflict bool-and targets must be bool
        // type conflict add targets must be float / int
        2 + fn foo() {} && true
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
    ]));
}
#[test]
fn if_else_usage() {
    test(r#"
        if true {} else { panic(""); }
        if true {} else if true { panic(""); }
        if true {} else if true { panic(""); } else { panic(""); }
        if false { panic(""); } else if true {} else { panic(""); }
        if false { panic(""); } else if false { panic(""); } else {}
        assert(if true { true } else { false });
        assert(1342 == if true { 1337 } else { panic("") } + 5);
    "#, ReturnValue::Ok);
}
#[test]
fn if_else_diagnostics() {
    test(r#"
        // unnecessary parens
        if (true) {} else {}
        // mismatched types
        if true { 1337 } else { "" }
        // missing else branch
        if true { 1337 }
        // mismatched types
        if true { 1337 } else {}
        // mismatched types
        if true { 1337 } else { 42; }
        // expected bool
        if 1337 {}
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Warning(ErrorCode::UnnecessaryIfConditionParenthesis),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::MissingElse),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
    ]));
}
#[test]
fn match_usage() {
    test(r#"
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
    "#, ReturnValue::Ok);
}
#[test]
fn match_diagnostics() {
    test(r#"
        // empty body
        // non-exhaustive
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
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Warning(ErrorCode::EmptyMatch),
        Emitted::Error(ErrorCode::NonExhaustiveMatch),
        Emitted::Warning(ErrorCode::UnreachableMatchArm),
        Emitted::Error(ErrorCode::FloatMatch),
        Emitted::Error(ErrorCode::MatchNoCatchall),
        Emitted::Error(ErrorCode::StructMatch),
        Emitted::Error(ErrorCode::NonExhaustiveMatch),
    ]));
}
#[test]
fn while_usage() {
    test(r#"
        let mut i = 0;
        while i < 3 {
            i = i + 1;
        }
        assert(i == 3);
    "#, ReturnValue::Ok);
}
#[test]
fn while_diagnostics() {
    test(r#"
        // unnecessary parens
        while (true) {}
        // type conflict between bool & int
        while 1337 {}
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Warning(ErrorCode::UnnecessaryWhileConditionParenthesis),
        Emitted::Error(ErrorCode::UnableToInferType),
    ]));
}
#[test]
fn format_strings() {
    test(r#"
        let foo = 42;
        assert(f"uiae" == "uiae");
        assert(f"{foo}" == "42");
        assert(f"{4 * 10 + 2}" == "42");
        assert(f"\{\}" == "{}");
        assert(f"{f"42"}" == "42");
    "#, ReturnValue::Ok);
}
#[test]
fn format_string_diagnostics1() {
    test(r#"
        // unescaped format string curly paren
        f"}";
        // unterminated format string arg
        f"uiae {";
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::UnescapedFormatStringCurlyParen),
        Emitted::Error(ErrorCode::UnterminatedFormatStringArg),
    ]));
}
#[test]
fn format_string_diagnostics2() {
    test(r#"
        f"uiae
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::UnterminatedFormatString)]));
}
#[test]
fn struct_definitions() {
    test(r#"
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

        // method and associated function call
        let foo = Foo::new(42);
        assert(foo.foo == 42);
        assert(foo.foo() == 42);
        assert(foo.bar() == 42);
        assert(Foo::foo(foo) == 42);
        assert(Foo::bar(foo) == 42);
    "#, ReturnValue::Ok);
}

#[test]
fn struct_diagnostics() {
    test(r#"
        // duplicate struct name (global)
        struct Foo {}
        struct Foo { foo: int }
        // // recursive struct definition
        // struct Foo {
        //     foo: Foo,
        // }
        //
        // // mutual recursive struct definition
        // struct Foo2 {
        //     foo: Foo3,
        // }
        // // mutual recursive struct definition
        // struct Foo3 {
        //     foo: Foo2,
        // }

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

        // duplicate field
        struct Quux {
            foo: int,
            foo: float,
        }
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::DuplicateGlobal),
        // Emitted::Error(ErrorCode::RecursiveStruct),
        // Emitted::Error(ErrorCode::RecursiveStruct),
        // Emitted::Error(ErrorCode::RecursiveStruct),
        Emitted::Error(ErrorCode::MissingField),
        Emitted::Error(ErrorCode::UnknownFieldInit),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::ImmutableAssign),
        Emitted::Error(ErrorCode::DuplicateStructField),
    ]));
}

#[test]
fn enum_definitions() {
    test(r#"
        // enum definition
        enum Never {}
        enum Foo {
            Foo(int),
        }
        enum Bar {
            Foo(int),
            Bar(bool),
        }

        // initialization
        let mut foo = Foo::Foo(1337);
        let foo2 = Foo::Foo(1337);
        assert(foo == foo2);
        foo = Foo::Foo(42);
        assert(foo != foo2);
        let bar = Bar::Foo(1337);
        let bar2 = Bar::Bar(true);
        let bar3 = Bar::Bar(true);
        assert(bar != bar2);
        assert(bar2 == bar3);

        // match
        match Foo::Foo(42) {
            Foo::Foo(i) => assert(i == 42)
        }
        match Bar::Foo(1337) {
            Bar::Foo(i) => assert(i == 1337),
            _ => panic(""),
        }

        // impl block
        impl Bar {
            fn new(foo: int) -> Bar {
                Bar::Foo(foo)
            }
            fn unwrap_foo(self) -> int {
                match self {
                    Bar::Foo(i) => i,
                    _ => panic(f"expected Bar::Foo, got {self}"),
                }
            }
            fn unwrap_bar(self) -> bool {
                match self {
                    Bar::Bar(b) => b,
                    other => panic(f"expected Bar::Bar, got {other}"),
                }
            }
        }

        // method and associated function call
        let foo = Bar::new(42);
        let bar = Bar::Bar(true);
        assert(match foo { Bar::Foo(i) => i == 42, Bar::Bar(b) => panic("") });
        assert(foo.unwrap_foo() == 42);
        assert(bar.unwrap_bar());
        assert(Bar::unwrap_foo(foo) == 42);
        assert(Bar::unwrap_bar(bar));
    "#, ReturnValue::Ok);
}

#[test]
fn enum_diagnostics() {
    test(r#"
        // duplicate enum name
        enum Foo {}
        // // recursive enum definition
        enum Foo {
            // Foo(Foo),
        }

        // // mutual recursive enum definition
        // enum Foo2 {
        //     Foo(Foo3),
        // }
        // // mutual recursive enum definition
        // enum Foo3 {
        //     Foo(Foo2)
        // }

        enum Bar {
            Bar(int),
        }

        // wrong number of enum fields (arguments)
        let bar = Bar::Bar();
        // wrong number of enum fields (arguments)
        let bar = Bar::Bar(42, true);

        // unknown function
        Bar::Foo;

        // compare between different struct types
        enum Baz { Baz }
        enum Qux { Qux }
        let baz = Baz::Baz;
        let qux = Qux::Qux;
        baz == qux;
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::DuplicateGlobal),
        // Emitted::Error(ErrorCode::RecursiveEnum),
        Emitted::Error(ErrorCode::InvalidNumberOfArguments),
        Emitted::Error(ErrorCode::InvalidNumberOfArguments),
        Emitted::Error(ErrorCode::UnknownFunction),
        Emitted::Error(ErrorCode::UnableToInferType),
    ]));
}

#[test]
fn associated_functions() {
    test(r#"
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
    "#, ReturnValue::Ok);
}
#[test]
fn associated_function_diagnostics() {
    test(r#"
        struct Foo {}
        // empty impl block
        impl Foo {}
    "#, ReturnValue::Diagnostics(vec![Emitted::Warning(ErrorCode::EmptyImplBlock)]));
}
#[test]
fn associated_function_diagnostics2() {
    test(r#"
        // unknown impl block target
        impl Foo {
            fn foo() {}
        }
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::UnknownImplBlockTarget)]));
}

#[test]
fn method_diagnostics() {
    test(r#"
        struct Foo {}
        let foo = Foo {};
        impl Foo {
            fn a() {}
            fn foo(self, a: int, b: string) -> int { a }
        }
        // unknown method
        foo.b();
        // not a method
        // invalid number of arguments
        foo.a();
        // invalid argument type 0
        Foo::foo(1337, 42, "uiae");
        // invalid argument type 1
        foo.foo("uiae", "uiae");
        // invalid argument type 2
        foo.foo(1337, 42);
        // invalid return type
        let a: string = foo.foo(1337, "uiae");
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::UnknownMethod),
        Emitted::Error(ErrorCode::NotAMethod),
        Emitted::Error(ErrorCode::InvalidNumberOfArguments),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
    ]));
}

#[test]
fn redefine_external_types() {
    test(r#"
        // external type Option redefined
        enum Option<T> {}
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::DuplicateGlobal)]));
}

#[test]
fn generics() {
    test(r#"
        enum MyOption<T> {
            Some(T),
            None,
        }
        impl MyOption<T> {
            fn unwrap(self) -> T {
                match self {
                    MyOption::Some(t) => t,
                    MyOption::None => panic("tried to unwrap a None value"),
                }
            }
        }
        let a = MyOption::Some(42);
        let b = MyOption::Some("uiae");
        let c: MyOption<float> = MyOption::None;
        print(a.unwrap(), b.unwrap(), c);

        struct Foo<T> {
            t: T,
        }
        struct Bar<A, B, C> {
            a: A,
            b: B,
            c: C,
        }

        fn foo<U, V>(u: U, v: V) -> V { v }
        fn bar<T>(t: T) -> T { foo(42, t) }
    "#, ReturnValue::Ok);
}
#[test]
fn generic_diagnostics() {
    test(r#"
        // T and V are not unifyable, because V is int
        fn foo<U, V>(u: U, v: V) -> V { v }
        fn bar<T>(t: T) -> T { foo(t, 42) }
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::UnableToInferType)]));
}

#[test]
fn method_arg_number() {
    test(r#"
        struct Foo {}
        impl Foo {
            fn bar(self, a: int) { a; }
        }
        let foo = Foo {};
        foo.bar();
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::InvalidNumberOfArguments)]));
}

#[test]
fn idents_starting_with_underscore() {
    test(r#"
        let _foo = 1337;
        assert(_foo == 1337);
        match true {
            _val => (),
        }
    "#, ReturnValue::Ok);
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
    "#, ReturnValue::Ok);
}
