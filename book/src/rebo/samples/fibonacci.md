# fibonacci

Recursive:
```rust
fn fib_rec(i: int) -> int {
    match i {
        0 => 0,
        1 => 1,
        i => fib_rec(i - 1) + fib_rec(i - 2),
    }
}
assert_eq(fib_rec(6), 8);
```

Iterative:
```rust
fn fib_iter(mut i: int) -> int {
    if i <= 1 {
        return i;
    }
    let mut a = 0;
    let mut b = 1;
    i -= 2;
    while i >= 0 {
        let temp = b;
        b += a;
        a = temp;
        i -= 1;
    }
    b
}
assert_eq(fib_iter(90), 2880067194370816120);
```
