#[macro_use]
extern crate log;

use std::fs;

use typed_arena::Arena;
use crate::diagnostics::Diagnostics;

mod diagnostics;
mod lexer;
mod parser;
mod vm;

fn main() {
    env_logger::init();

    let source_arena = Arena::new();
    let diagnostics = Diagnostics::new(&source_arena);

    let code = fs::read_to_string("test.re").unwrap();
    let (file, code) = diagnostics.add_file("test.re".to_string(), code);
    let tokens = lexer::lex(&diagnostics, file, code).unwrap();
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
