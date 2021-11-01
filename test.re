print(foo(10, 20));
fn foo(mut x: int, mut y: int) -> int {
    x = x + 10;
    y = y + 20;
    x + y
}
print(1 + 2 * 3 == 7);
print(1 + 2 * 3 * 4);
let mut a = 1;
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

enum Option<T> {
    Some(T),
    None,
}

impl Option<T> {
    fn unwrap(self) -> T {
        match self {
            Option::Some(t) => t,
            Option::None => panic("tried to unwrap a None value"),
        }
    }
}

let a = Option::Some(1337);
let c = Option::Some(42);
print(a.unwrap() + c.unwrap());

let a = Option::Some(42);
let b: Option<Option<string>> = Option::Some(Option::Some("uiae"));
print(b.unwrap().unwrap());
print(a, b);
