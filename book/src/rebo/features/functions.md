# Functions

```rust
// functions can be used before their definition
assert(foo(10, 20) == 60);

fn foo(mut x: int, mut y: int) -> int {
    x += 10;
    y += 20;
    x + y
}

fn choose(cond: bool, a: int, b: int) -> int {
    if cond {
        return a;
    }
    b
}

assert_eq(choose(true, 1, 2), 1);
assert_eq(choose(false, 1, 2), 2);
```

### Advanced

```rust
// functions as first-class citizens
let new: fn<T>() -> List<T> = List::new;
let test = assert;
test(f"{new()}" == "[]");
// anonymous functions
struct NewList {
    new: fn<T>() -> List<T>,
}
let new_list = NewList {
    new: fn<T>() -> List<T> {
        List::new()
    },
};
let new = new_list.new;
assert(f"{new()}" == "[]");
```
