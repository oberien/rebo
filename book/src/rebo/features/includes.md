# Includes

```rust
// filename: test-include.re
// mdbook-run-rebo: skip

static MY_INCLUDED_STATIC = "uiae";

fn included_fn() -> int {
    MY_STATIC
}

static DEFINED_IN_INCLUDE = 1;
fn defined_in_include() -> int { 2 };

assert_eq(5, DEFINED_AFTER_INCLUDE);
assert_eq(7, defined_after_include());

"this is the inclusion result"
```

```rust
static mut MY_STATIC = 42;

assert_eq(1, DEFINED_IN_INCLUDE);
assert_eq(2, defined_in_include());

let value = include "test-include.re";
assert_eq(value, "this is the inclusion result");

static DEFINED_AFTER_INCLUDE = 5;

fn defined_after_include() -> int { 7 }

assert(MY_INCLUDED_STATIC == "uiae");
assert(included_fn() == 42);
```
