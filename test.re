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
