# Generics

```rust
// generics
struct GenericFoo<T> {
    bar: GenericBar<T>,
}
struct GenericBar<U> {
    t: U,
}
let foo = GenericFoo { bar: GenericBar { t: 1337 } };
assert(foo.bar.t + 1 == 1338);

fn id<T>(t: T) -> T { t }
assert(id(id(1337)) == 1337);
fn id1<U, V>(u: U, v: V) -> V { v }
fn id2<T>(t: T) -> T { id1(42, t) }
assert(id2(1337) == 1337);

// defined in stdlib:
// enum Option<T> {
//     Some(T),
//     None,
// }
let a = Option::Some(1337);
let b = Option::Some(42);
let c = Option::None;
assert(a.unwrap() + b.unwrap() + c.unwrap_or(21) == 1400);

let a = Option::Some(42);
let b: Option<Option<string>> = Option::Some(Option::Some("uiae"));
assert(f"{a}, {b}, {b.unwrap().unwrap()}" == "Some(42), Some(Some(uiae)), uiae");

// defined in stdlib:
// enum Result<T, E> {
//     Ok(T),
//     Err(E),
// }
let a = Result::Err("error");
let b = Result::Err("error");
assert(a == b);
let a = Result::Ok(1337);
let b = Result::Ok(42);
assert(f"{a}, {b}, {a.unwrap() + b.unwrap()}" == "Ok(1337), Ok(42), 1379");
```
