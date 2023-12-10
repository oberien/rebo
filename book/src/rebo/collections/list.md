# List\<T\>

```rust
let list = List::new();
assert(f"{list}" == "[]");

list.push(1337);
list.push(42);

assert(f"{list}" == "[1337, 42]");
assert(list.get(0) == Option::Some(1337));
assert(list.get(1).unwrap() == 42);
assert(list.get(2) == Option::None);

let list2 = List::of(1, 2, 3);
assert(f"{list2}" == "[1, 2, 3]");

list.set(1, 21).unwrap();
assert(list.get(1).unwrap() == 21);
```
