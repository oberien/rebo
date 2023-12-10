# Control Flow

```rust
// if - else if - else
assert(if true { 1337 } else { panic("F") } + 5 == 1342);


// match
assert(match 1 {
    0 => 21,
    1 => 1337,
    _ => 42,
} == 1337);

let res = match 1 {
    0 => "it was a 0",
    foo => f"it was something else: {foo}",
};
assert_eq(res, "it was something else: 1");

```
