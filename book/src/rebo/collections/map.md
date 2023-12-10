# Map\<K, V\>

```rust
let mut map = Map::new();

map.insert("a", 1337);
map.insert("b", -42);

assert(f"{map}" == "{a: 1337, b: -42}");
assert(f"{map.keys()}" == "[a, b]");
assert(f"{map.values()}" == "[1337, -42]");

assert(map.get("a") == Option::Some(1337));
assert(map.remove("b") == Option::Some(-42));
assert(map.get("b") == Option::None);
```
