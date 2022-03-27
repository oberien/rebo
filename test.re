print("Hello", "World!");
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
assert(choose(true, 1, 2) == 1);
assert(choose(false, 1, 2) == 2);
assert(1 + 2 * 3 == 7);
assert(0b1 + 0x2 * 3 * 4 == 25);
let a = 0;
let mut a = a + 1;
let mut c = add_one(a);
c += {
    let c = 1337;
    c + 42
};
assert(c == 1381);
let unit = ();
assert(unit == ());

struct Foo {
    a: int,
    b: string,
}
let mut foo = Foo { a: 1337, b: "uiae" };
assert(f"{foo}" == "Foo { a: 1337, b: uiae }");
let foo2 = Foo { a: 1337, b: "dtrn" };
assert(foo != foo2);
foo.b = "dtrn";
assert(foo == foo2);

struct Inner { x: int }
struct Outer { inner: Inner }
let mut outer = Outer { inner: Inner { x: 1337 } };
assert(outer.inner.x == 1337);
outer.inner.x = 420;
assert(outer.inner.x == 420);
outer.inner = Inner { x: 69 };
assert(outer.inner.x == 69);
let mut other_inner = Inner { x: 42 };
outer.inner = other_inner;
assert(outer.inner.x == 42);
other_inner.x = 21;
assert(outer.inner.x == 21);

// methods
impl Foo {
    fn new(a: int, b: string) -> Foo {
        Foo { a: a, b: b }
    }
    fn a(self) -> int {
        self.a
    }
    fn b(self) -> string {
        self.b
    }
}
let foo = Foo::new(42, "uiae");
assert(foo.a() == 42 && foo.b() == "uiae");
assert(Foo::a(foo) == 42 && Foo::b(foo) == "uiae");

struct Something {}
impl Something {
    fn returns_self(self) -> Something {
        self
    }
}
let foo = Something {};
assert(foo.returns_self().returns_self() == foo);


// if - else if - else
assert(if true { 1337 } else { panic("F") } + 5 == 1342);

// loop
assert(loop { break "loop" } == "loop");
assert('outer: loop { loop { break 'outer "outer loop" } } == "outer loop");

// while
let mut i = 0;
while i < 3 {
    i += 1;
}
assert(i == 3);
let mut i = 0;
'outer: while true {
    while true {
        i += 1;
        break 'outer;
    }
};
assert(i == 1);

// fibonacci sequence
fn fib(i: int) -> int {
    if i == 0 {
        0
    } else if i == 1 {
        1
    } else {
        fib(i - 1) + fib(i - 2)
    }
}
fn fib_iter(mut i: int) -> int {
    if i <= 1 {
        return i;
    }
    let mut a = 0;
    let mut b = 1;
    i -= 2;
    while i >= 0 {
        let temp = b;
        b += a;
        a = temp;
        i -= 1;
    }
    b
}
assert(fib(10) == 55);
assert(fib_iter(90) == 2880067194370816120);

// string lengths
let s = "uiae";
assert(s.len_utf8() == 4);
assert(s.len_utf16() == 4);
assert(s.len_utf32() == 4);
assert(s.len_grapheme_clusters() == 4);
assert(s.len_legacy_grapheme_clusters() == 4);
// format strings
let foo = 4;
assert(f"{foo * 10 + 2} is the answer" == "42 is the answer");
assert(f"{1337:#8x}" == "   0x539");
assert(f"{"uiae":?}" == "\"uiae\"");
assert(f"{Foo::new(1337, "uiae"):?}" == "Foo { a: 1337, b: \"uiae\" }");

// match
assert(match 1 {
    0 => 21,
    1 => 1337,
    _ => 42,
} == 1337);

let res = match 1 {
  0 => "it was a 0",
  foo => f"it was something else: {foo}",
};
assert(res == "it was something else: 1");

// enums with match
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

// generics
struct GenericFoo<T> {
    bar: GenericBar<T>,
}
struct GenericBar<U> {
    t: U,
}
let foo = GenericFoo { bar: GenericBar { t: 1337 } };
assert(foo.bar.t + 1 == 1338);

fn id<T>(t: T) -> T { t }
assert(id(id(1337)) == 1337);
fn id1<U, V>(u: U, v: V) -> V { v }
fn id2<T>(t: T) -> T { id1(42, t) }
assert(id2(1337) == 1337);

// Option<T>
let a = Option::Some(1337);
let b = Option::Some(42);
let c = Option::None;
assert(a.unwrap() + b.unwrap() + c.unwrap_or(21) == 1400);

let a = Option::Some(42);
let b: Option<Option<string>> = Option::Some(Option::Some("uiae"));
assert(f"{a}, {b}, {b.unwrap().unwrap()}" == "Some(42), Some(Some(uiae)), uiae");

// Result<T, E>
let a = Result::Err("error");
let b = Result::Err("error");
assert(a == b);
let a = Result::Ok(1337);
let b = Result::Ok(42);
assert(f"{a}, {b}, {a.unwrap() + b.unwrap()}" == "Ok(1337), Ok(42), 1379");

// List<T>
let list = List::new();
assert(f"{list}" == "[]");
list.push(1337);
list.push(42);
assert(f"{list}" == "[1337, 42]");
assert(list.get(0) == Option::Some(1337));
assert(list.get(1).unwrap() == 42);
assert(list.get(2) == Option::None);
assert(f"{List::of(1, 2, 3)}" == "[1, 2, 3]");
list.set(1, 21).unwrap();
assert(list.get(1).unwrap() == 21);

// for loop
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
assert(sum == 1337);

// Map<K, V>
let mut map = Map::new();
map.insert("a", 1337);
map.insert("b", -42);
assert(f"{map}" == "{a: 1337, b: -42}");
assert(f"{map.keys()}" == "[a, b]");
assert(f"{map.values()}" == "[1337, -42]");
assert(map.get("a") == Option::Some(1337));
assert(map.remove("b") == Option::Some(-42));

// Set<K, V>
let mut set = Set::new();
set.insert(1337);
set.insert(42);
assert(set.len() == 2);
assert(set.insert(1337) == false);
assert(set.len() == 2);
assert(set.contains(42));
assert(set.remove(42));
assert(!set.contains(42));

// statics
assert(MY_STATIC == 42);
static mut MY_STATIC = 42;
MY_STATIC = 1337;
use_static();
assert(MY_STATIC == 21);

fn use_static() {
    assert(MY_STATIC == 1337);
    MY_STATIC = 21;
}
static CURRENT_UI = List::new();

// includes
assert(include "test-include.re" == "this is the inclusion result");
assert(MY_INCLUDED_STATIC == "uiae");
assert(included_fn() == 21);
let content = File::read_to_string("test-include.re");
assert(content.is_ok() && content.unwrap().starts_with("static MY_INCLUDED_STATIC ="));

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

// parse numbers
let a = "1337";
let b = "42.";
let c = "abc";
assert(a.parse_int() == Result::Ok(1337));
assert(b.parse_int() == Result::Err(()));
assert(c.parse_int() == Result::Err(()));
assert(a.parse_float() == Result::Ok(1337.));
assert(b.parse_float() == Result::Ok(42.));
assert(c.parse_float() == Result::Err(()));

// slicing
let s = "abcdef";
assert(s.slice(1) == "bcdef");
assert(s.slice(-1) == "f");
assert(s.slice(1, 3) == "bc");
assert(s.slice(0, -1) == "abcde");
assert(s.slice(1, -1) == "bcde");
assert(s.slice(-2, -1) == "e");
let s = "αβγδεζ";
assert(s.slice(1) == "βγδεζ");
assert(s.slice(-1) == "ζ");
assert(s.slice(1, 3) == "βγ");
assert(s.slice(0, -1) == "αβγδε");
assert(s.slice(1, -1) == "βγδε");
assert(s.slice(-2, -1) == "ε");
