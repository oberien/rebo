# Statics

```rust
assert(MY_STATIC == 42);

static mut MY_STATIC = 42;

MY_STATIC = 1337;
use_static();
assert(MY_STATIC == 21);

fn use_static() {
    assert(MY_STATIC == 1337);
    MY_STATIC = 21;
}
```
