# Enums

```rust
// enums
enum Value {
    Unit,
    Integer(int),
    Float(float),
}
// enum matching
fn value_to_string(value: Value) -> string {
    match value {
        Value::Unit => "unit",
        Value::Integer(i) => f"integer: {i}",
        _ => "something else",
    }
}

let unit = Value::Unit;
let i = Value::Integer(1337);
let f = Value::Float(42.);
assert(f"{unit}, {i}, {f}" == "Unit, Integer(1337), Float(42)");
assert(value_to_string(unit) == "unit");
assert(value_to_string(i) == "integer: 1337");
assert(value_to_string(f) == "something else");
```