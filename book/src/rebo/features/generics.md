# Generics

```rust
// generic struct definitions
struct GenericFoo<T> {
    bar: GenericBar<T>,
}
struct GenericBar<U> {
    t: U,
}
// generic impls (must have same generic name)
impl GenericBar<U> {
    fn t(self) -> U {
        self.t
    }
}
// T can be an int
let foo = GenericFoo { bar: GenericBar { t: 1337 } };
assert_eq(foo.bar.t(), 1337);
assert_eq(foo.bar.t + 1, 1338);

// T can also be anything else
let foo = GenericFoo { bar: GenericBar { t: "Hello World!" } };
assert_eq(foo.bar.t(), "Hello World!");

// functions with generic arguments
fn id<T>(t: T) -> T { t }
assert_eq(id(id(1337)), 1337);
fn id1<U, V>(u: U, v: V) -> V { v }
fn id2<T>(t: T) -> T { id1(42, t) }
assert_eq(id2(1337), 1337);

// defined in stdlib:
// enum Option<T> {
//     Some(T),
//     None,
// }
let a = Option::Some(1337);
let b = Option::Some(42);
let c = Option::None;
assert_eq(a.unwrap() + b.unwrap() + c.unwrap_or(21), 1400);

let a = Option::Some(42);
let b: Option<Option<string>> = Option::Some(Option::Some("uiae"));
assert_eq(f"{a}, {b}, {b.unwrap().unwrap()}", "Some(42), Some(Some(uiae)), uiae");

// defined in stdlib:
// enum Result<T, E> {
//     Ok(T),
//     Err(E),
// }
let a = Result::Err("error");
let b = Result::Err("error");
assert_eq(a, b);
let a = Result::Ok(1337);
let b = Result::Ok(42);
assert_eq(f"{a}, {b}, {a.unwrap() + b.unwrap()}", "Ok(1337), Ok(42), 1379");
```
