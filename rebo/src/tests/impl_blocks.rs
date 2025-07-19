use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

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
    "#, ReturnValue::Ok(Value::Unit));
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
fn method_arg_number_diagnostic() {
    test(r#"
        struct Foo {}
        impl Foo {
            fn bar(self, a: int) { a; }
        }
        let foo = Foo {};
        foo.bar();
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::InvalidNumberOfArguments)]));
}
