# Primitive Conversions

```rust
// int::to_float
let i = 175;
assert(i.to_float() == 175.);

// float::to_int
let f = 42.5;
assert(f.to_int() == 42);

// bool::to_int
let b = true;
assert(b.to_int() == 1);

// anything to string: format-strings
let s = f"i = {i}";
```
