# Variables / Bindings

```rust
// `let <name> = expr` assigns the value of `expr` to
// the variable named `<name>`;
let foo = 5;

// shadowing variables / bindings is allowed
let foo = "Hello World";

// reassignment is allowed if the variable is declared as mutable
// foo = "test"; // compile error: assignment to immutable variable
let mut foo = foo;
foo = f"{foo}!";
```
