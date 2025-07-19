use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

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
    "#, ReturnValue::Ok(Value::Unit));
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
