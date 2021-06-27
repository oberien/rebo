#[macro_use]
extern crate log;
extern crate self as rebo;

use std::sync::Mutex;

use typed_arena::Arena;
use diagnostic::{Diagnostics, Span};

use crate::parser::{Parser, Ast};
use crate::vm::Vm;
use crate::scope::Scope;

mod error_codes;
mod lexer;
mod parser;
mod typeck;
mod vm;
mod scope;
mod stdlib;
mod util;
mod common;
#[cfg(test)]
mod tests;

pub use rebo_derive::function;
use crate::typeck::Typechecker;

const EXTERNAL_SOURCE: &str = "defined externally";
lazy_static::lazy_static! {
    static ref EXTERNAL_SPAN: Mutex<Option<Span>> = Mutex::new(None);
}

pub fn run(filename: String, code: String) {
    let diagnostics = Diagnostics::new();
    // register file 0 for external sources
    let external = diagnostics.add_file("external".to_string(), EXTERNAL_SOURCE.to_string());
    *EXTERNAL_SPAN.lock().unwrap() = Some(Span::new(external.0, 0, EXTERNAL_SOURCE.len()));

    let (file, code) = diagnostics.add_file(filename, code);

    // lex
    let tokens = lexer::lex(&diagnostics, file, code).unwrap();
    println!("TOKENS:\n{}\n", tokens);

    let mut scope = Scope::new();
    let mut pre_info = stdlib::add_to_scope(&mut scope);

    let arena = Arena::new();
    let parser = Parser::new(&arena, tokens, &diagnostics, &mut pre_info);
    let ast = parser.parse().unwrap();
    println!("AST:\n{}\n", ast);
    let Ast { exprs, bindings: _ } = ast;
    Typechecker::new(&diagnostics, &mut pre_info).typeck(&exprs);

    if diagnostics.errors_printed() > 0 {
        eprintln!("Aborted due to errors");
        return;
    }

    let vm = Vm::new(scope);
    let result = vm.run(&exprs);
    println!("RESULT: {:?}", result);
}
