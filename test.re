print(true);
print(false);
print(true && false);
print(true && true);
print(false && true);
print(true || false);
print(false || false || true);
print(true || true && false);
print((true || true) && false);
print(1 + 2 * 3);
print(1 * 2 + 3);
print((1 + 2) * 3);
print(1 + (2 * 3));
print((1 * 2) + 3);
print(1 * (2 + 3));
let mut a = 1;
let b = add_one;
let mut c = b(a);
c = c + {
    let c = 1337;
    c + 42
};
print(c, 5, 7, 13);
print();
let unit = ();
print(unit);