# Expressions

Everything in rebo is an expression.

```rust
let a = 0;
let mut a = a + 1;
let mut c = add_one(a);
c += {
    let c = 1337;
    c + 42
};
assert(c == 1381);
```
