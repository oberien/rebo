use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

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
    "#, ReturnValue::Ok(Value::Unit));
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
