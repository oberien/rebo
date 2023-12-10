# Set\<T\>

```rust
let mut set = Set::new();

set.insert(1337);
set.insert(42);

assert(set.len() == 2);

assert(set.insert(1337) == false);
assert(set.len() == 2);

assert(set.contains(42));
assert(set.remove(42));
assert(!set.contains(42));
```
