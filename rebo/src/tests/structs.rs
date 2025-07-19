use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

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
    "#, ReturnValue::Ok(Value::Unit));
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
