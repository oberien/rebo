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
use crate::typeck::{BindingTypes, Typechecker};

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

    let mut binding_types = BindingTypes::new();
    let mut root_scope = RootScope::new(&mut binding_types);
    stdlib::add_to_root_scope(&mut root_scope);
    let (root_scope, binding_id_mapping) = root_scope.into_inner();

    let arena = Arena::new();
    let parser = Parser::new(&arena, tokens, &diagnostics, &binding_id_mapping);
    let ast = parser.parse().unwrap();
    println!("{}", ast);
    let Ast { exprs, bindings } = ast;
    Typechecker::new(&diagnostics, &mut binding_types).typeck(&exprs);

    if diagnostics.error_printed() {
        eprintln!("Aborted due to errors");
        return;
    }

    let vm = Vm::new(root_scope);
    let result = vm.run(&exprs);
    println!("{:?}", result);
}
