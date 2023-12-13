# Structs

```rust
// struct declaration
struct Foo {
    foo: int,
    bar: string,
}
// struct initialization
let foo = Foo { foo: 1337, bar: "lorem ipsum" };

// impl-blocks can be used to define functions and methods on a type
impl Foo {
    // it is convention to have a `new` function as constructor
    // this is an associated function as it does not have a `self` parameter as first argument
    fn new(foo: int, bar: string) -> Foo {
        Foo { foo: foo, bar: bar }
    }

    // this is a method, as it has `self` as first argument
    fn foo(self) -> int {
        self.foo
    }
    fn bar(self) -> string {
        self.bar
    }
    fn add_to_foo(mut self, lhs: int) {
        self.foo = self.foo + lhs;
    }
}
// call static associated function on Foo
let mut foo = Foo::new(1337, "dolor sit amet");
// call method
foo.add_to_foo(1);
assert(foo.foo() == 1338);

// structs can be stringified just like everything else
assert_eq(f"{foo}", "Foo { foo: 1338, bar: dolor sit amet }");

let foo2 = Foo { foo: 1338, bar: "consectetur" };
assert(foo != foo2);

// rebo always uses structural equality
foo.bar = "consectetur";
assert_eq(foo, foo2);

// structs can be nested
struct Inner { x: int }
struct Outer { inner: Inner }

let mut outer = Outer {
    inner: Inner {
        x: 1337,
    }
};

assert(outer.inner.x == 1337);

outer.inner.x = 420;
assert(outer.inner.x == 420);

outer.inner = Inner { x: 69 };
assert(outer.inner.x == 69);

let mut other_inner = Inner { x: 42 };
outer.inner = other_inner;
assert(outer.inner.x == 42);

other_inner.x = 21;
assert(outer.inner.x == 21);
```
