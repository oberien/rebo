# List\<T\>

```rust
// create a new empty list
let mut list = List::new();
assert_eq(f"{list}", "[]");
// type of the list element will be inferred
list.push(1337);
// this would result in a type-error
//list.push("foo");

// create a list from elements
let list = List::of(1337, 42);
assert(f"{list}" == "[1337, 42]");

// access elements of a list / indexing
// List::get returns an Option.
// If the index exists, Option::Some(_) is returned.
// Otherwise, Option::None is returned.
assert(list.get(0) == Option::Some(1337));
assert(list.get(1).unwrap() == 42);
assert(list.get(2) == Option::None);

// iterate over list elements
for element in list {
    print(element + 1);
}

// iterate over indexes
for i in List::range(0, list.len()) {
    print(f"item {i}: {list.get(i).unwrap()}");
}

// elements can be overwritten using List::set
list.set(1, 21).unwrap();
assert_eq(f"{list}", "[1337, 21]");
```
