#[macro_use]
extern crate log;
extern crate self as rebo;

use std::sync::Mutex;

use typed_arena::Arena;
use diagnostic::{Diagnostics, Span};

use crate::parser::{Parser, Ast};
use crate::vm::Vm;
use crate::scope::RootScope;

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
use crate::typeck::{BindingTypes, Typechecker};
use std::path::Path;

const EXTERNAL_SOURCE: &str = "defined externally";
lazy_static::lazy_static! {
    static ref EXTERNAL_SPAN: Mutex<Option<Span>> = Mutex::new(None);
}



pub fn run<P: AsRef<Path>>(path: P, code: String) {
    let diagnostics = Diagnostics::new();
    // register file 0 for external sources
    let external = diagnostics.add_file("external".to_string(), EXTERNAL_SOURCE.to_string());
    *EXTERNAL_SPAN.lock().unwrap() = Some(Span::new(external.0, 0, EXTERNAL_SOURCE.len()));

    let (file, code) = diagnostics.add_file(path.as_ref().to_string_lossy().into(), code);
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
    let Ast { exprs, bindings: _ } = ast;
    Typechecker::new(&diagnostics, &mut binding_types).typeck(&exprs);

    if diagnostics.error_printed() {
        eprintln!("Aborted due to errors");
        return;
    }

    let vm = Vm::new(root_scope);
    let result = vm.run(&exprs);
    println!("{:?}", result);
}
