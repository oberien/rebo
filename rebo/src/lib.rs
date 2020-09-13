#[macro_use]
extern crate log;
extern crate self as rebo;

use std::fs;

use typed_arena::Arena;

use crate::parser::{Parser, Ast};
use crate::diagnostics::Diagnostics;
use crate::vm::Vm;
use crate::scope::RootScope;

mod diagnostics;
mod lexer;
mod parser;
mod typeck;
mod vm;
mod scope;
mod types;
mod stdlib;

pub use rebo_derive::function;

const EXTERNAL_SOURCE: &str = "defined externally";

pub fn do_stuff() {
    let source_arena = Arena::new();
    let diagnostics = Diagnostics::new(&source_arena);
    // register file 0 for external sources
    diagnostics.add_file("external".to_string(), EXTERNAL_SOURCE.to_string());

    let code = fs::read_to_string("test.re").unwrap();
    let (file, code) = diagnostics.add_file("test.re".to_string(), code);
    let tokens = lexer::lex(&diagnostics, file, code).unwrap();
    println!("{}", tokens);

    let mut root_scope = RootScope::new();
    stdlib::add_to_root_scope(&mut root_scope);

    let arena = Arena::new();
    let parser = Parser::new(&arena, tokens, &diagnostics, root_scope.binding_id_mapping());
    let ast = parser.parse().unwrap();
    println!("{}", ast);
    let Ast { exprs, bindings } = ast;

    if diagnostics.error_printed() {
        eprintln!("Aborted due to errors");
        return;
    }

    let vm = Vm::new(root_scope);
    let result = vm.run(&exprs);
    println!("{:?}", result);
}
