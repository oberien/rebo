# Clone / Deep Copy

Every type in rebo has a `.clone()` function, which performs a deep-copy.

```rust
struct A { l: List<int> };
let mut orig = A { l: List::of(1, 2, 3) };
assert_eq(orig.l, List::of(1, 2, 3));
orig.l.set(1, 5);
assert_eq(orig.l, List::of(1, 5, 3));

let mut clone = orig.clone();
assert_eq(clone.l, List::of(1, 5, 3));
clone.l.set(1, 7);
assert_eq(clone.l, List::of(1, 7, 3));
assert_eq(orig.l, List::of(1, 5, 3));
```
