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

// everything is an expression, even if-statements and blocks
let foo = {
    let foo = 21;
    // the last expression of a block is the value "returned" from
    // the block / the value the block will evaluate to
    foo
};

// no ternary if-else operator is needed as if-else is an expression
let bar = if true { 42 } else { 1337 };
```
