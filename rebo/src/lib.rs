#[macro_use]
extern crate log;
extern crate self as rebo;

use std::sync::Mutex;

use typed_arena::Arena;
use diagnostic::{Diagnostics, Span};

use crate::parser::{Parser, Ast};
use crate::vm::Vm;

mod error_codes;
mod lexer;
mod parser;
// mod typeck;
mod typeck2;
mod lints;
mod vm;
mod scope;
mod stdlib;
mod util;
mod common;
#[cfg(test)]
mod tests;

pub use rebo_derive::function;
// use crate::typeck::Typechecker;
use crate::common::MetaInfo;
use std::time::Instant;
use crate::lexer::Lexer;
use itertools::Itertools;

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
    let diagnostics = Diagnostics::new();
    // register file 0 for external sources
    let external = diagnostics.add_file("external".to_string(), EXTERNAL_SOURCE.to_string());
    *EXTERNAL_SPAN.lock().unwrap() = Some(Span::new(external.0, 0, EXTERNAL_SOURCE.len()));

    let (file, _code) = diagnostics.add_file(filename, code);

    // lex
    let time = Instant::now();
    let lexer = Lexer::new(&diagnostics, file);
    info!("Lexing took {}μs", time.elapsed().as_micros());
    info!("TOKENS:\n{}\n", lexer.iter().map(|token| token.to_string()).join(""));

    let mut meta_info = MetaInfo::new();
    stdlib::add_to_scope(&diagnostics, &mut meta_info);

    let time = Instant::now();
    let arena = Arena::new();
    let parser = Parser::new(&arena, lexer, &diagnostics, &mut meta_info);
    let ast = parser.parse_ast().unwrap();
    info!("Parsing took {}μs", time.elapsed().as_micros());
    info!("AST:\n{}\n", ast);
    let Ast { exprs, bindings: _ } = ast;

    let time = Instant::now();
    // Typechecker::new(&diagnostics, &mut meta_info).typeck(&exprs);
    typeck2::typeck(&diagnostics, &mut meta_info, &exprs);
    info!("Typechecking took {}μs", time.elapsed().as_micros());

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

    let time = Instant::now();
    let vm = Vm::new(meta_info);
    let result = vm.run(&exprs);
    info!("Execution took {}μs", time.elapsed().as_micros());
    println!("RESULT: {:?}", result);
    if diags > 0 {
        ReturnValue::Diagnostics(diags)
    } else {
        ReturnValue::Ok
    }
}
