# Methods

```rust
struct Foo {
    a: int,
    b: string,
}

impl Foo {
    fn new(a: int, b: string) -> Foo {
        Foo { a: a, b: b }
    }
    fn a(self) -> int {
        self.a
    }
    fn b(self) -> string {
        self.b
    }
}
assert(f"{Foo::new(1337, "uiae"):?}" == "Foo { a: 1337, b: \"uiae\" }");
let foo = Foo::new(42, "uiae");
assert(foo.a() == 42 && foo.b() == "uiae");
assert(Foo::a(foo) == 42 && Foo::b(foo) == "uiae");

struct Something {}
impl Something {
    fn returns_self(self) -> Something {
        self
    }
}
let foo = Something {};
assert(foo.returns_self().returns_self() == foo);
```
