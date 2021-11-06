#[macro_use]
extern crate log;
extern crate self as rebo;

use std::time::Instant;

pub use diagnostic::{Diagnostics, Span, Output, FileId};
use itertools::Itertools;
use typed_arena::Arena;

use crate::common::MetaInfo;
use crate::lexer::Lexer;
use crate::parser::{Ast, Expr, Parser};
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

pub use rebo_derive::{function, ExternalType};
pub use vm::{VmContext, ExecError};
pub use common::{Value, FromValue, IntoValue, Typed, ExternalFunction, ExternalType};
#[doc(hidden)] // only used for the derive macros
pub use common::{StructArc, Struct, EnumArc, Enum};
pub use typeck::types::{Type, FunctionType, SpecificType};
pub use stdlib::Stdlib;
pub use util::CowVec;
use std::path::PathBuf;

const EXTERNAL_SOURCE: &str = "defined externally";
const EXTERNAL_SPAN: Span = Span::new(FileId::synthetic("external.re"), 0, EXTERNAL_SOURCE.len());

#[derive(Debug, PartialEq, Eq)]
pub enum ReturnValue {
    Ok,
    Diagnostics(u32),
}

pub struct ExternalTypes<'a, 'b, 'i> {
    arena: &'a Arena<Expr<'a, 'i>>,
    diagnostics: &'i Diagnostics,
    meta_info: &'b mut MetaInfo<'a, 'i>,
}
impl<'a, 'b, 'i> ExternalTypes<'a, 'b, 'i> {
    pub fn add_external_type<T: ExternalType>(&mut self) {
        self.meta_info.add_external_type::<T>(self.arena, self.diagnostics);
    }
}

pub struct ReboConfig {
    stdlib: Stdlib,
    functions: Vec<ExternalFunction>,
    interrupt_interval: u32,
    interrupt_function: fn(&mut VmContext) -> Result<(), ExecError>,
    diagnostic_output: Output,
    include_directory: Option<PathBuf>,
    external_type_adder: fn(&mut ExternalTypes),
}
impl ReboConfig {
    pub fn new() -> ReboConfig {
        ReboConfig {
            stdlib: Stdlib::all(),
            functions: vec![],
            interrupt_interval: 10000,
            interrupt_function: |_| Ok(()),
            diagnostic_output: Output::stderr(),
            include_directory: None,
            external_type_adder: |_| (),
        }
    }
    pub fn stdlib(mut self, stdlib: Stdlib) -> Self {
        self.stdlib = stdlib;
        self
    }
    pub fn add_function(mut self, function: ExternalFunction) -> Self {
        self.functions.push(function);
        self
    }
    pub fn interrupt_interval(mut self, interval: u32) -> Self {
        self.interrupt_interval = interval;
        self
    }
    pub fn interrupt_function(mut self, function: fn(&mut VmContext) -> Result<(), ExecError>) -> Self {
        self.interrupt_function = function;
        self
    }
    pub fn diagnostic_output(mut self, output: Output) -> Self {
        self.diagnostic_output = output;
        self
    }
    pub fn include_directory(mut self, dir: PathBuf) -> Self {
        self.include_directory = Some(dir);
        self
    }
    pub fn external_type_adder(mut self, adder: fn(&mut ExternalTypes)) -> Self {
        self.external_type_adder = adder;
        self
    }
}

pub fn run(filename: String, code: String) -> ReturnValue {
    run_with_config(filename, code, ReboConfig::new())
}
pub fn run_with_config(filename: String, code: String, config: ReboConfig) -> ReturnValue {
    let ReboConfig { stdlib, functions, interrupt_interval, interrupt_function, diagnostic_output, include_directory, external_type_adder } = config;

    let diagnostics = Diagnostics::with_output(diagnostic_output);
    // register file for external sources
    diagnostics.add_synthetic_file("external.re", EXTERNAL_SOURCE.to_string());

    // stdlib
    let arena = Arena::new();
    let mut meta_info = MetaInfo::new();
    stdlib::add_to_meta_info(stdlib, &diagnostics, &arena, &mut meta_info);

    // add external types defined by library user
    let mut external_types = ExternalTypes {
        arena: &arena,
        diagnostics: &diagnostics,
        meta_info: &mut meta_info
    };
    external_type_adder(&mut external_types);

    // add external functions defined by library user
    for function in functions {
        meta_info.add_external_function(&diagnostics, function);
    }

    // lex
    let (file, _code) = diagnostics.add_file(filename, code);
    let time = Instant::now();
    let lexer = Lexer::new(&diagnostics, file);
    info!("Lexing took {}μs", time.elapsed().as_micros());
    info!("TOKENS:\n{}\n", lexer.iter().map(|token| token.to_string()).join(""));

    // parse
    let include_directory = include_directory.unwrap_or_else(|| std::env::current_dir().expect("can't get current working directory"));
    let time = Instant::now();
    let parser = Parser::new(include_directory, &arena, lexer, &diagnostics, &mut meta_info);
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
    let vm = Vm::new(&diagnostics, meta_info, interrupt_interval, interrupt_function);
    let result = vm.run(&exprs);
    info!("Execution took {}μs", time.elapsed().as_micros());
    println!("RESULT: {:?}", result);
    if diags > 0 {
        ReturnValue::Diagnostics(diags)
    } else {
        ReturnValue::Ok
    }
}
