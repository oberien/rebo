#[macro_use]
extern crate log;
extern crate self as rebo;

use std::collections::HashMap;
use instant::Instant;

pub use diagnostic::{Diagnostics, Span, Output, FileId};
use itertools::Itertools;
use typed_arena::Arena;

use crate::common::{MetaInfo, RequiredReboFunctionStruct, SpanWithId};
use crate::lexer::{Lexer, TokenIdent};
use crate::parser::{Ast, Binding, Expr, Parser};
use crate::vm::Vm;

mod error_codes;
mod lexer;
mod parser;
mod typeck;
mod lints;
mod vm;
mod stdlib;
mod util;
mod xdot;
mod common;
#[cfg(test)]
mod tests;

pub use rebo_derive::{function, required_rebo_functions, ExternalType};
pub use vm::{VmContext, ExecError};
pub use common::{Value, FromValue, IntoValue, Typed, ExternalFunction, RequiredReboFunction, ExternalType, ExternalTypeType, DisplayValue, DebugValue, OctalValue, LowerHexValue, UpperHexValue, BinaryValue, LowerExpValue, UpperExpValue};
pub(crate) use common::FunctionValue;
#[doc(hidden)] // only used for the derive macros
pub use common::{StructArc, Struct, EnumArc, Enum};
pub use typeck::types::{Type, FunctionType, SpecificType};
pub use stdlib::{Stdlib, List, Map};
pub use common::expr_gen::*;
use std::path::{PathBuf, Path};
use std::sync::LazyLock;
use diagnostic::Emitted;
use rebo::parser::BindingId;
use crate::error_codes::ErrorCode;

const EXTERNAL_SOURCE: &str = "defined externally";
const EXTERNAL_SPAN: LazyLock<SpanWithId> = LazyLock::new(|| SpanWithId::new(FileId::synthetic_named("external.re"), 0, EXTERNAL_SOURCE.len()));

#[derive(Debug, PartialEq, Eq)]
pub struct RunResult {
    pub return_value: ReturnValue,
    /// dot graph
    pub type_graph_before: Option<String>,
    /// dot graph
    pub type_graph_after: Option<String>,
    /// function-name -> (dot-state-graph, generated-code)
    pub generators: HashMap<String, (String, String)>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ReturnValue {
    Ok(Value),
    ParseError,
    Diagnostics(Vec<Emitted<ErrorCode>>),
    Panic,
}

#[derive(Debug, Clone)]
pub enum IncludeDirectory {
    Path(PathBuf),
    Everywhere,
}
impl IncludeDirectory {
    pub fn unwrap_path(&self) -> &Path {
        match self {
            IncludeDirectory::Path(path) => path,
            IncludeDirectory::Everywhere => panic!("IncludeDirectory::unwrap_path called with everywhere"),
        }
    }
}
#[derive(Debug, Clone)]
pub enum IncludeDirectoryConfig {
    Workdir,
    Path(PathBuf),
    Everywhere,
}

pub type ExternalTypeAdderFunction = for<'i, 'b> fn(&'i Arena<Expr<'i>>, &'i Diagnostics<ErrorCode>, &'b mut MetaInfo<'i>);
pub struct ReboConfig {
    stdlib: Stdlib,
    functions: Vec<ExternalFunction>,
    external_values: Vec<(String, Value)>,
    interrupt_interval: u32,
    interrupt_function: for<'i> fn(&mut VmContext<'i, '_, '_>) -> Result<(), ExecError<'i>>,
    diagnostic_output: Output,
    include_directory: IncludeDirectoryConfig,
    external_type_adder_functions: Vec<ExternalTypeAdderFunction>,
    required_rebo_functions: Vec<RequiredReboFunctionStruct>,
}
impl ReboConfig {
    pub fn new() -> ReboConfig {
        ReboConfig {
            stdlib: Stdlib::all(),
            functions: vec![],
            external_values: vec![],
            interrupt_interval: 10000,
            interrupt_function: |_| Ok(()),
            diagnostic_output: Output::stderr(),
            include_directory: IncludeDirectoryConfig::Workdir,
            external_type_adder_functions: Vec::new(),
            required_rebo_functions: Vec::new(),
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
    pub fn add_external_value<T: IntoValue>(mut self, name: String, value: T) -> Self {
        self.external_values.push((name, value.into_value()));
        self
    }
    pub fn interrupt_interval(mut self, interval: u32) -> Self {
        self.interrupt_interval = interval;
        self
    }
    pub fn interrupt_function(mut self, function: for <'i> fn(&mut VmContext<'i, '_, '_>) -> Result<(), ExecError<'i>>) -> Self {
        self.interrupt_function = function;
        self
    }
    pub fn diagnostic_output(mut self, output: Output) -> Self {
        self.diagnostic_output = output;
        self
    }
    pub fn include_directory(mut self, dir: IncludeDirectoryConfig) -> Self {
        self.include_directory = dir;
        self
    }
    pub fn add_external_type<T: ExternalTypeType>(mut self, _: T) -> Self {
        pub fn add_external_type<'i, 'b, T: ExternalType>(arena: &'i Arena<Expr<'i>>, diagnostics: &'i Diagnostics<ErrorCode>, meta_info: &'b mut MetaInfo<'i>) {
            meta_info.add_external_type::<T>(arena, diagnostics);
        }
        self.external_type_adder_functions.push(add_external_type::<T::Type>);
        self
    }
    pub fn add_required_rebo_function<T: RequiredReboFunction>(mut self, _: T) -> Self {
        self.required_rebo_functions.push(RequiredReboFunctionStruct::from_required_rebo_function::<T>());
        self
    }
}
impl Default for ReboConfig {
    fn default() -> Self {
        Self::new()
    }
}

pub fn run(filename: String, code: String) -> RunResult {
    run_with_config(filename, code, ReboConfig::new())
}
pub fn run_with_config(filename: String, code: String, config: ReboConfig) -> RunResult {
    let ReboConfig { stdlib, functions, external_values, interrupt_interval, interrupt_function, diagnostic_output, include_directory, external_type_adder_functions, required_rebo_functions } = config;

    let diagnostics = Diagnostics::with_output(diagnostic_output);
    // register file for external sources
    diagnostics.add_synthetic_named_file("external.re", EXTERNAL_SOURCE.to_string());

    // stdlib
    let arena = Arena::new();
    let mut meta_info = MetaInfo::new();
    stdlib::add_to_meta_info(stdlib, &diagnostics, &arena, &mut meta_info);

    // add external types defined by library user
    for adder_function in external_type_adder_functions {
        adder_function(&arena, &diagnostics, &mut meta_info);
    }

    // add external functions defined by library user
    for function in functions {
        meta_info.add_external_function(&arena, &diagnostics, function);
    }

    // add externally provided variables
    for (name, value) in external_values {
        let (fileid, code) = diagnostics.add_file(
            format!("__file_for_external_value_{name}"),
            format!("let {name} = /* defined externally */;")
        );
        let binding = Binding {
            id: BindingId::unique(),
            mutable: None,
            ident: TokenIdent {
                span: SpanWithId::new(fileid, 4, 4 + name.len()),
                ident: &code[4..][..name.len()],
            },
            rogue: false,
            span: SpanWithId::new(fileid, 4, 4 + name.len()),
        };
        meta_info.external_values.insert(binding, value);
    }

    // add required rebo functions
    for rrf in required_rebo_functions {
        meta_info.add_required_rebo_function(rrf, &diagnostics);
    }

    // lex
    let (file, _code) = diagnostics.add_file(filename, code);
    let time = Instant::now();
    let lexer = Lexer::new(&diagnostics, file);
    info!("Lexing took {}μs", time.elapsed().as_micros());
    debug!("TOKENS:\n{}\n", lexer.iter().map(|token| token.to_string()).join(""));

    // parse
    let include_directory = match include_directory {
        IncludeDirectoryConfig::Everywhere => IncludeDirectory::Everywhere,
        IncludeDirectoryConfig::Workdir => IncludeDirectory::Path(std::env::current_dir()
            .expect("can't get current working directory")
            .canonicalize()
            .expect("can't canonicalize current working directory")),
        IncludeDirectoryConfig::Path(path) => IncludeDirectory::Path(path),
    };
    let time = Instant::now();
    let parser = Parser::new(include_directory.clone(), &arena, lexer, &diagnostics, &mut meta_info, true);
    let ast = match parser.parse_ast() {
        Ok(ast) => ast,
        Err(e) => {
            match e {
                parser::Error::UnexpectedEof(span) => diagnostics.error(ErrorCode::UnexpectedEof)
                    .with_error_label(span, "this expression is not complete")
                    .emit(),
                parser::Error::Abort => (),
            }
            return RunResult {
                return_value: ReturnValue::ParseError,
                type_graph_before: None,
                type_graph_after: None,
                generators: meta_info.generators,
            }
        },
    };
    info!("Parsing took {}μs", time.elapsed().as_micros());
    debug!("AST:\n{}\n", ast);
    let Ast { exprs } = ast;

    // typeck
    let time = Instant::now();
    let (type_graph_before, type_graph_after) = typeck::typeck(&diagnostics, &mut meta_info, &exprs);
    info!("Typechecking took {}μs", time.elapsed().as_micros());

    // lint
    let time = Instant::now();
    lints::lint(&diagnostics, &meta_info, &exprs);
    info!("Linting took {}μs", time.elapsed().as_micros());

    let errors = diagnostics.emitted().into_iter().filter(|e| matches!(e, Emitted::Error(_))).count();
    let mut diags = diagnostics.emitted();
    diags.sort();
    let diags = diags;
    if  errors > 0 {
        eprintln!("Aborted due to errors");
        return RunResult {
            return_value: ReturnValue::Diagnostics(diags),
            type_graph_before: Some(type_graph_before),
            type_graph_after: Some(type_graph_after),
            generators: meta_info.generators,
        }
    }

    // run
    let time = Instant::now();
    let vm = Vm::new(include_directory, &diagnostics, &meta_info, interrupt_interval, interrupt_function);
    let result = vm.run(&exprs);
    info!("Execution took {}μs", time.elapsed().as_micros());
    error!("RESULT: {:?}", result);
    let return_value = match result {
        Ok(_) if !diags.is_empty() => ReturnValue::Diagnostics(diags),
        Ok(value) => ReturnValue::Ok(value),
        Err(ExecError::Panic) => ReturnValue::Panic,
        Err(ExecError::Continue(_)) => unreachable!("continue returned from Vm::run"),
        Err(ExecError::Break(..)) => unreachable!("break returned from Vm::run"),
        Err(ExecError::Return(_)) => unreachable!("return returned from Vm::run"),
    };
    RunResult {
        return_value,
        type_graph_before: Some(type_graph_before),
        type_graph_after: Some(type_graph_after),
        generators: meta_info.generators,
    }
}
