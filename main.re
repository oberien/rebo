gen fn foo() -> bool {
    // return within generators must not have an expression
    yield true;
    return;
    yield false;
}
let foo = foo();
print(foo.next());
print(foo.next());
print(foo.next());
print(foo.next());
print(foo.next());

// yield (1330 + {yield 42; 7})

// fn make_1337(x: ()) -> int { 1337 }
// yield make_1337(yield 42);

/*
gen fn range(mut start: int, end: int) -> int {
    while start < end {
        let foo: () = yield start;
        start += 1;
    }
}



CODE:
match expr {
    Value::Int(i) => a(i),
    Value::Float(f) => b(f),
    _ => c(),
}
d();

GRAPH:
node_0:
    expr()
    -> node_1 [Value::Int(i)]
    -> node_2 [Value::Float(f)]
    -> node_3 [_]
node_1:
    a(i);
    -> node_4 [()]
node_2:
    b(f);
    -> node_4 [()]
node_3:
    c();
    -> node_4 [()]
node_4:
    d();

GENERATED:
loop {
    self.state = match self.state {
        0 => match { expr() } {
            Value::Int(i) => {
                state.i = i;
                1
            },
            Value::Float(f) => {
                state.f = f;

            },
            _ => 3
        }
    }
}

let foo = struct Bar { a: int };
*/
