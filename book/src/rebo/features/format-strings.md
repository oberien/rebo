# Format Strings / String Formatting / String Interpolation

All of rust's format specifiers are supported.
See <https://doc.rust-lang.org/std/fmt/index.html> for more details.

```rust
// Format strings can contain embedded expressions in `{}`.
// Those expressions are evaluated with the result being interpolated into the string.
let i = 42;
let f = 1. / 3.;
// use format strings to convert types to strings
let s = f"{i}";
assert_eq(s, "42");
// or to concatenate strings
assert_eq(f"i = {s}", "i = 42");
// formatting specifiers can be defined after the expression after a `:`
assert_eq(f"f = {f:06.2}", "f = 000.33");
```
