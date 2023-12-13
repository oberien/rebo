# Functions

```rust
fn add_two(i: int) -> int {
    // just like blocks, the last expression of a function is returned
    i + 2
}
assert_eq(add_two(5), 7);
// functions can be generic
fn to_string<T>(t: T) -> string {
    f"{t}"
}
assert_eq("1337", to_string(1337));

// functions can be assigned to variables / are first-class citizens
let stringify = to_string;
assert_eq("true", stringify(true));

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
