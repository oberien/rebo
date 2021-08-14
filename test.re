fn foo(mut x: int, mut y: int) -> int {
    x = x + 10;
    y = y + 20;
    x + y
}
print(foo(10, 20));
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

print(if true { 1337 } else { panic("F") } + 5);

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

let foo = 4;
print(f"{foo * 10 + 2} is the answer ");

print(match 1 {
    0 => 21,
    1 => 1337,
    _ => 42,
} == 1337)
