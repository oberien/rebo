gen fn range(mut start: int, end: int) -> int {
    while start < end {
        let foo: () = yield start;
        start += 1;
    }
}

CODE:
if cond() {
    a();
    yield "uiae";
} else if cond2() {
    b();
} else {
    c();
}
d();

GRAPH:
node_0:
    cond()
    -> node_1 [true]
    -> node_2 [false]
node_1a:
    a();
    -> node_1b [()]
node_1b: YIELD "uiae"
    -> node_5 [()]
node_2:
    cond2()
    -> node_3 [true]
    -> node_4 [false]
node_3:
    b();
    -> node_5 [()]
node_4:
    c();
    -> node_5 [()]
node_5:
    d();

GENERATED:
loop {
    self.state = match self.state {
        0 => match { cond() } {
            true => 1,
            false => 2,
        },
        1a => match { a(); } {
            () => 1b,
        },
        1b => {
            self.state = 5;
            return Option::Some("uiae");
        },
        2 => match { cond2() } {
            true => 3,
            false => 4,
        },
        3 => match { b(); } {
            () => 5,
        },
        4 => match { c(); } {
            () => 5,
        },
        5 => {
            d();
            () => return Option::None,
        }
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
                2
            },
            _ => 3
        }
    }
}

let foo = struct Bar { a: int };
