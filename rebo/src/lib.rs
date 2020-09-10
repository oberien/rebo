#[macro_use]
extern crate log;
extern crate self as rebo;

use std::fs;

use typed_arena::Arena;
use crate::diagnostics::Diagnostics;

use scope::Scope;

pub use rebo_derive::function;

mod diagnostics;
mod lexer;
mod parser;
mod typeck;
mod vm;
mod scope;
mod types;
mod stdlib;

pub fn do_stuff() {
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

    let mut root_scope = Scope::new();
    stdlib::add_to_root_scope(&mut root_scope);
    let mut scope = Scope::with_parent_scope(root_scope);

    let result = vm::run(&mut scope, &ast);
    println!("{:?}", result);
}
