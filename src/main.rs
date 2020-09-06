#[macro_use]
extern crate log;

use std::fs;

use typed_arena::Arena;

mod span;
mod lexer;
mod parser;
mod vm;

fn main() {
    env_logger::init();
    let code = fs::read_to_string("test.re").unwrap();
    let tokens = lexer::lex(&code).unwrap();
    println!("{}", tokens);
    let arena = Arena::new();
    let ast = parser::parse(&arena, tokens);
    for expr in &ast.exprs {
        println!("{:?}", expr.typ);
    }
    println!("{}", ast);
    let result = vm::run(&ast);
    println!("{:?}", result);
}
