# Map\<K, V\>

```rust
// create a new empty map
let mut map = Map::new();

// the type of the key and value will be inferred
map.insert("one", 1);
map.insert("two", 3);
assert_eq(f"{map}", "{one: 1, two: 3}");

// overwrite value; insert returns the previous value or Option::None
assert_eq(map.insert("two", 2), Option::Some(3));

// type-error as it uses a different value-type
//map.insert("two", 2.0);
// type-error as it uses a different key-type
//map.insert(2, 2);

// access values
// Map::get returns an Option.
// If the key exists, Option::Some(_) is returned.
// Otherwise, Option::None is returned.
assert_eq(map.get("one"), Option::Some(1));
assert_eq(map.get("two").unwrap(), 2);
assert_eq(map.get("three"), Option::None);

// iterate over values
assert_eq(f"{map.values()}", "[1, 2]");
for value in map.values() {
    print(value);
}

// iterate over keys
assert_eq(f"{map.keys()}", "[one, two]");
for key in map.keys() {
    print(f"{key} = {map.get(key).unwrap()}");
}
```
