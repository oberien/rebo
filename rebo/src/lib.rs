#[macro_use]
extern crate log;
extern crate self as rebo;

use std::sync::Mutex;
use std::time::Instant;

pub use diagnostic::{Diagnostics, Span};
use itertools::Itertools;
use typed_arena::Arena;

use crate::common::MetaInfo;
use crate::lexer::Lexer;
use crate::parser::{Ast, Parser};
use crate::vm::Vm;

mod error_codes;
mod lexer;
mod parser;
mod typeck;
mod lints;
mod vm;
mod stdlib;
mod util;
mod common;
#[cfg(test)]
mod tests;

pub use rebo_derive::function;
pub use vm::VmContext;
pub use common::{Value, FromValue, FromValues, IntoValue, ExternalFunction};
pub use typeck::types::{Type, FunctionType};

const EXTERNAL_SOURCE: &str = "defined externally";
lazy_static::lazy_static! {
    pub static ref EXTERNAL_SPAN: Mutex<Option<Span>> = Mutex::new(None);
}

#[derive(Debug, PartialEq, Eq)]
pub enum ReturnValue {
    Ok,
    Diagnostics(u32),
}

pub fn run(filename: String, code: String) -> ReturnValue {
    run_with_functions(filename, code, [])
}
pub fn run_with_functions(filename: String, code: String, functions: impl IntoIterator<Item = (&'static str, ExternalFunction)>) -> ReturnValue {
    let diagnostics = Diagnostics::new();
    // register file 0 for external sources
    let external = diagnostics.add_file("external".to_string(), EXTERNAL_SOURCE.to_string());
    *EXTERNAL_SPAN.lock().unwrap() = Some(Span::new(external.0, 0, EXTERNAL_SOURCE.len()));

    // stdlib
    let arena = Arena::new();
    let mut meta_info = MetaInfo::new();
    stdlib::add_to_meta_info(&diagnostics, &arena, &mut meta_info);

    // add external functions defined by library user
    for (name, function) in functions {
        meta_info.add_external_function(&diagnostics, name, function);
    }

    // lex
    let (file, _code) = diagnostics.add_file(filename, code);
    let time = Instant::now();
    let lexer = Lexer::new(&diagnostics, file);
    info!("Lexing took {}μs", time.elapsed().as_micros());
    info!("TOKENS:\n{}\n", lexer.iter().map(|token| token.to_string()).join(""));

    // parse
    let time = Instant::now();
    let parser = Parser::new(&arena, lexer, &diagnostics, &mut meta_info);
    let ast = parser.parse_ast().unwrap();
    info!("Parsing took {}μs", time.elapsed().as_micros());
    info!("AST:\n{}\n", ast);
    let Ast { exprs, bindings: _ } = ast;

    // typeck
    let time = Instant::now();
    typeck::typeck(&diagnostics, &mut meta_info, &exprs);
    info!("Typechecking took {}μs", time.elapsed().as_micros());

    // lint
    let time = Instant::now();
    lints::lint(&diagnostics, &meta_info, &exprs);
    info!("Linting took {}μs", time.elapsed().as_micros());

    let errors = diagnostics.errors_printed();
    let diags = diagnostics.bugs_printed()
        + errors
        + diagnostics.warnings_printed()
        + diagnostics.notes_printed()
        + diagnostics.helps_printed();
    if  errors > 0 {
        eprintln!("Aborted due to errors");
        return ReturnValue::Diagnostics(diags);
    }

    // run
    let time = Instant::now();
    let vm = Vm::new(&diagnostics, meta_info);
    let result = vm.run(&exprs);
    info!("Execution took {}μs", time.elapsed().as_micros());
    println!("RESULT: {:?}", result);
    if diags > 0 {
        ReturnValue::Diagnostics(diags)
    } else {
        ReturnValue::Ok
    }
}
