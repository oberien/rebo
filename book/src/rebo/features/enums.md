# Enums

```rust
// enum declaration
enum MathOp {
    Add,
    Negate,
}

// enum matching
fn perform_math(op: MathOp, a: int, b: Option<int>) -> int {
    match op {
        MathOp::Add => a + b.unwrap(),
        MathOp::Negate => -a,
    }
}
let res = perform_math(MathOp::Add, 1, Option::Some(2));
assert(res == 3);
let res = perform_math(MathOp::Negate, 5, Option::None);
assert(res == -5);

// enum variants can also have values
enum MathOpImproved {
    Add(int, int),
    Negate(int),
}
// functions and methods can be implemented on enums
impl MathOpImproved {
    // enum matching with fields
    fn perform_math(self) -> int {
        match self {
            MathOpImproved::Add(a, b) => a + b,
            MathOpImproved::Negate(a) => -a,
        }
    }
}
let op = MathOpImproved::Add(1, 2);
let res = op.perform_math();
assert_eq(res, 3);

// while we're at it, define a Value enum
enum Value {
    Unit,
    Integer(int),
    Float(float),
}

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