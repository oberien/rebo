# Rebo - A statically-typed rust-inspired scripting language

Rebo is an expression-based scripting language with syntax heavily inspired by rust.
It is statically typed and has backwards type inference.
When executing a script, rebo fully analyses and checks the source code
including all types, before it runs.
While that approach is usually used for AOT (Ahead Of Time compilers),
the execution of rebo is interpreted and thus embeddable on all systems.
This approach also allows defining types and functions after they are first used.

## Features

For examples, take a look at the [examples/](examples/) directory.  
The file [test.re](test.re) contains simple usage samples of all features.

#### Types:
* `int`, `float`, `bool`, `string`, `()` (unit / "void"), structs, enums (tagged unions), `List`, `Map`

#### Type Conversion:
```rust
let i = 175;
assert(i.to_float() == 175.);
let f = 42.5;
assert(f.to_int() == 42);
let b = true;
assert(b.to_int() == 1);

// use format strings to convert types to strings
let s = f"i = {i}";
```

#### Format Strings:
```rust
// Format strings can contain embedded expressions in `{}`.
// Those expressions are evaluated with the result being interpolated into the string.
let i = 42;
let f = 1. / 3.;
// use format strings to convert types to strings
let s = f"{i}";
// or to concatenate strings
let s = f"i = {s}";
// formatting specifiers can be defined after the expression after a `:`
let s = f"f = {f:06.2}";
```

#### Expressions:
```rust
// everything is an expression, even if-statements and blocks
let foo = {
    let foo = 21;
    // the last expression of a block is the value "returned" by
    // the block / the value the block will evaluate to
    foo
}
// no ternary if-else operator needed as if-else is an expression
let bar = if true { 42 } else { 1337 };
// shadowing variables (bindings) is allowed
let mut bar = "Hello World";
// reassignment is allowed if the variable is declared as mutable
bar = f"{bar}!";
```

#### Functions:
```rust
fn add_two(i: int) -> int {
    // just like blocks, the last expression of a function is returned
    i + 2
}
assert(add_two(5), 7);
// functions can be generic
fn to_string<T>(t: T) -> string {
    f"{t}"
}
assert("1337" == to_string(1337));

// functions can be assigned to variables / are first-class citizens
let stringify = to_string;
assert("true" == stringify(true));
```

#### Structs / impl:
```rust
// struct declaration
struct Rectangle {
    name: string,
    width: int,
    height: int,
}
// struct initialization
let foo = Foo { foo: 1337, bar: "42" };

// impl-blocks can be used to define functions and methods on a type
impl Foo {
    // it is convention to have a `new` function as constructor
    // this is an associated function as it does not have a `self` parameter as first argument
    fn new(foo: int, bar: string) -> Foo {
        Foo { foo: foo, bar: bar }
    }
  
    // this is a method, as it has `self` as first argument
    fn foo(self) -> int {
        self.foo
    }
    fn bar(self) -> string {
        self.bar
    }
    fn add_to_foo(mut self, lhs: int) {
        self.foo = self.foo + lhs;
    }
}
// call static associated function on Foo
let mut foo = Foo::new(1337, "42");
// call method
foo.add_to_foo(1);
assert(foo.foo() == 1338);
```

#### Enums (Tagged Unions) / impl:
```rust
// enum declaration
enum MathOp {
    Add,
    Negate,
}

fn perform_math(op: MathOp, a: int, b: Option<int>) -> int {
    match op {
        MathOp::Add => a + b.unwrap(),
        MathOp::Negate => -a,
    }
}
let res = perform_math(MathOp::Add, 1, Option::Some(2));
assert(res == 3);
let res = perform_math(MathOp::Negate, 5);
assert(res == -5);

// enum variants can also have values
enum MathOpImproved {
    Add(int, int),
    Negate(int),
}
impl MathOpImproved {
    fn perform_math(self) -> int {
        match self {
            MathOpImproved::Add(a, b) => a + b,
            MathOpImproved::Negate(a) => -a,
        }
    }
}
let op = MathOpImproved::Add(1, 2);
let res = op.perform_math();
assert(res == 3);
```

#### List
```rust
// create a new empty list
let mut list = List::new();
// type of the list element will be inferred
list.push(1337);
// this would result in a type-error
//list.push("foo");

// create a list from elements
let list = List::of(1337, 42);

// access elements of a list / indexing
// List::get returns an Option.
// If the index exists, Option::Some(_) is returned.
// Otherwise, Option::None is returned.
assert(list.get(0) == Option::Some(1337));
assert(list.get(1).unwrap() == 42);
assert(list.get(2) == Option::None);

// iterate over list elements
for element in list {
    print(element + 1);
}
// iterate over indexes
let mut i = 0;
while i < list.len() {
    print(f"item {i}: {list.get(i).unwrap()}");
    i += 1;
}
```

#### Map
```rust
// create a new empty map
let mut map = Map::new();
// the type of the key and value will be inferred
map.insert("one", 1);
map.insert("two", 3);
// overwrite value; insert returns the previous value or Option::None
assert(map.insert("two", 2) == Option::Some(3));
// type-error as it uses a different value-type
//map.insert("two", 2.0);
// type-error as it uses a different key-type
//map.insert(2, 2);

// access values
// Map::get returns an Option.
// If the key exists, Option::Some(_) is returned.
// Otherwise, Option::None is returned.
assert(map.get("one") == Option::Some(1));
assert(map.get("two").unwrap() == 2);
assert(map.get("three") == Option::None);

// iterate over values
for value in map.values() {
    print(value);
}
// iterate over keys
for key in map.keys() {
    print(f"{key} = {map.get(key).unwrap()}");
}
```

## Current Limitations

* no control-flow keywords like `return`, `break` or `continue`
* methods / fields can only be called / accessed on variables (and not on expressions)
    * for example, you can't write `1.to_float()` but instead need to write `let i = 1; i.to_float()`

## Running

* Make sure to have rust installed.
  If you don't have it installed already, follow the guide on <https://rustup.rs/>.
* Clone this repository and navigate into the directory (`git clone https://github.com/oberien/rebo; cd rebo`)
* Run a rebo file: `cargo run --release test.re`

## Embedding
