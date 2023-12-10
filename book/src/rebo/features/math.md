# Math and Numbers

```rust
assert_eq(1 + 2 * 3, 7);
assert_eq(5 - -5 - 10, 0);
assert_eq(0b1 + 0x2 * 3 * 4, 25);
assert_eq(1 ^ 3, 2);
assert_eq(5 % 2, 1);
assert_eq(float::to_bits(0.0), 0);
assert_eq(float::from_bits(0x4060B66666666666), 133.7);

// parse numbers
let a = "1337";
let b = "42.";
let c = "abc";
let d = "-1337";
let e = "-42.";
assert(a.parse_int() == Result::Ok(1337));
assert(b.parse_int() == Result::Err(()));
assert(c.parse_int() == Result::Err(()));
assert(d.parse_int() == Result::Ok(-1337));
assert(e.parse_int() == Result::Err(()));
assert(a.parse_float() == Result::Ok(1337.));
assert(b.parse_float() == Result::Ok(42.));
assert(c.parse_float() == Result::Err(()));
assert(d.parse_float() == Result::Ok(-1337.));
assert(e.parse_float() == Result::Ok(-42.));
```
