print(foo(10, 20));
fn foo(mut x: int, mut y: int) -> int {
    x = x + 10;
    y = y + 20;
    x + y
}
print(1 + 2 * 3 == 7);
print(1 + 2 * 3 * 4);
let a = 0;
let mut a = a + 1;
let mut c = add_one(a);
c = c + {
    let c = 1337;
    c + 42
};
print(c, 5, 7, 13);
print();
let unit = ();
print(unit);

struct Foo {
    a: int,
    b: string,
}
let mut foo = Foo { a: 1337, b: "uiae" };
print(foo);
let foo2 = Foo { a: 1337, b: "dtrn" };
print(foo, foo2, foo == foo2);
print(foo.a, foo.b);
foo.b = "dtrn";
print(foo, foo2, foo == foo2);

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
print(foo);
print(foo.a(), foo.b());
print(Foo::a(foo), Foo::b(foo));

struct Something {}
impl Something {
    fn returns_self(self) -> Something {
        self
    }
}
let foo = Something {};
print(foo.returns_self().returns_self());


// if - else if - else
print(if true { 1337 } else { panic("F") } + 5);

// while
let mut i = 0;
while i < 3 {
    i = i + 1;
}
print(i);

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
        i
    } else {
        let mut a = 0;
        let mut b = 1;
        i = i - 2;
        while i >= 0 {
            let temp = b;
            b = b + a;
            a = temp;
            i = i - 1;
        }
        b
    }
}
print(fib(10));
print(fib_iter(90));

// format strings
let foo = 4;
print(f"{foo * 10 + 2} is the answer");
print(f"{1337:#8x}");
print(f"{"uiae":?}");
print(f"{Foo::new(1337, "uiae"):?}");

// match
print(match 1 {
    0 => 21,
    1 => 1337,
    _ => 42,
} == 1337);

match 1 {
  0 => print("it was a 0"),
  foo => print(f"it was something else: {foo}"),
}

// enums with match
enum Value {
    Unit,
    Integer(int),
    Float(float),
}
fn print_value(value: Value) {
    match value {
        Value::Unit => print("unit"),
        Value::Integer(i) => print(f"integer: {i}"),
        _ => print("something else"),
    }
}

let unit = Value::Unit;
let i = Value::Integer(1337);
let f = Value::Float(42.);
print(unit, i, f);
print_value(unit);
print_value(i);
print_value(f);

// generics
struct GenericFoo<T> {
    bar: GenericBar<T>,
}
struct GenericBar<U> {
    t: U,
}
let foo = GenericFoo { bar: GenericBar { t: 1337 } };
print(foo.bar.t + 1);

fn id<T>(t: T) -> T { t }
print(id(id(1337)));
fn id1<U, V>(u: U, v: V) -> V { v }
fn id2<T>(t: T) -> T { id1(42, t) }
print(id2(1337));

// Option<T>
let a = Option::Some(1337);
let c = Option::Some(42);
print(a.unwrap() + c.unwrap());

let a = Option::Some(42);
let b: Option<Option<string>> = Option::Some(Option::Some("uiae"));
print(a, b, b.unwrap().unwrap());

// Result<T, E>
let a = Result::Err("error");
let b = Result::Err("error");
assert(a == b);
let a = Result::Ok(1337);
let b = Result::Ok(42);
print(a, b, a.unwrap() + b.unwrap());

// List<T>
let list = List::new();
print(list);
list.push(1337);
list.push(42);
print(list, list.get(0), list.get(1).unwrap(), list.get(2));
print(List::of(1, 2, 3));
list.set(1, 21).unwrap();

// for loop
for i in list {
    print(i);
}

// Map<K, V>
let map = Map::new();
map.insert("a", 1337);
map.insert("b", -42);
print(map, map.keys(), map.values());
print(map.get("a"), map.remove("b"));

// statics
print(MY_STATIC);
static mut MY_STATIC = 42;
MY_STATIC = 1337;
use_static();
print(MY_STATIC);

fn use_static() {
    print(MY_STATIC);
    MY_STATIC = 21;
}
static CURRENT_UI = List::new();

// includes
print(include "test-include.re");
print(MY_INCLUDED_STATIC);
print(included_fn());
print(f"{File::read_to_string("test-include.re"):?}");

// functions as first-class citizens
let new: fn<T>() -> List<T> = List::new;
let out = print;
out(new());
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
print(new());
