# Loop, While, For

```rust
// loop
let val = loop {
    break "loop";
};
assert_eq(val, "loop");

// loop labels
let val = 'outer: loop {
    loop {
        // break with value
        break 'outer "outer loop";
    }
};
assert_eq(val, "outer loop");

// while
let mut i = 0;
while i < 3 {
    i += 1;
}
assert_eq(i, 3);

// while with labels
let mut i = 0;
'outer: while true {
    while true {
        i += 1;
        break 'outer;
    }
};
assert_eq(i, 1);

// for loop
let list = List::of(1337, 21);
let mut sum = 0;

for i in list {
    sum += i;
}
assert(sum == 1358);

let mut sum = 0;
'outer: for i in list {
    for i in list {
        sum += i;
        break 'outer;
    }
}
assert_eq(sum, 1337);
```
