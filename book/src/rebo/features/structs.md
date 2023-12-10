# Structs

```rust
struct Foo {
    a: int,
    b: string,
}

let mut foo = Foo { a: 1337, b: "uiae" };
assert(f"{foo}" == "Foo { a: 1337, b: uiae }");

let foo2 = Foo { a: 1337, b: "dtrn" };
assert(foo != foo2);

foo.b = "dtrn";
assert(foo == foo2);

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
