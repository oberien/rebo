print(1 + 2 * 3 == 7);
print(1 + 2 * 3 * 4);
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