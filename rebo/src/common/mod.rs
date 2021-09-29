mod values;
mod types;

pub use values::{Value, Function, FunctionImpl, IntoValue, FromValues, FromValue, Struct, StructArc, FuzzyFloat};
pub use types::{Type, SpecificType, SpecificTypeDiscriminants, FunctionType, StructType};
use std::fmt::{self, Display, Formatter};
use indexmap::map::IndexMap;
use diagnostic::{Span, Diagnostics};
use crate::parser::{Binding, ExprFunctionDefinition, ExprPatternTyped, ExprPatternUntyped, Spanned};
use std::borrow::Cow;
use crate::error_codes::ErrorCode;
use crate::EXTERNAL_SPAN;
use crate::typeck::TypeVar;

/// Metadata / information needed before and/or during static analyses
pub struct MetaInfo<'a, 'i> {
    /// types of bindings of the root scope / stdlib and function definitions of the first parser pass
    ///
    /// Available after the parser's first-pass.
    pub bindings: IndexMap<Binding<'i>, SpecificType>,
    /// map of all functions or associated functions for function resolution
    ///
    /// Available after the parser's first-pass.
    pub functions: IndexMap<Cow<'i, str>, Function>,
    /// functions or associated functions found in the code
    ///
    /// Available after the parser's first-pass.
    pub rebo_functions: IndexMap<Cow<'i, str>, &'a ExprFunctionDefinition<'a, 'i>>,
    /// struct definitions found in the code
    ///
    /// Available after the parser's first-pass.
    pub structs: IndexMap<&'i str, (StructType, Span)>,
    /// resolved types
    ///
    /// Available after typeck.
    pub types: IndexMap<TypeVar, Type>,
}
impl<'a, 'i> MetaInfo<'a, 'i> {
    pub fn new() -> Self {
        MetaInfo {
            bindings: IndexMap::new(),
            functions: IndexMap::new(),
            rebo_functions: IndexMap::new(),
            structs: IndexMap::new(),
            types: IndexMap::new(),
        }
    }

    pub fn add_function(&mut self, diagnostics: &Diagnostics, name: Cow<'i, str>, fun: &'a ExprFunctionDefinition<'a, 'i>) {
        let typ = FunctionType {
            args: fun.args.iter().map(|pattern| {
                Type::Specific(SpecificType::from(&pattern.typ))
            }).collect(),
            ret: Type::Specific(fun.ret_type.as_ref().map(|(_, typ)| SpecificType::from(typ)).unwrap_or(SpecificType::Unit)),
        };
        let arg_binding_ids = fun.args.iter().map(|ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }| binding.id).collect();
        let function = Function {
            typ,
            imp: FunctionImpl::Rebo(name.to_string(), arg_binding_ids, fun.span()),
        };
        if self.check_existing_function(diagnostics, &name, fun.name.span) {
            return;
        }
        self.functions.insert(name.clone(), function);
        self.rebo_functions.insert(name, fun);
    }
    pub fn add_external_function(&mut self, diagnostics: &Diagnostics, name: &'static str, fun: Function) {
        if self.check_existing_function(diagnostics, name, EXTERNAL_SPAN.lock().unwrap().unwrap()) {
            return;
        }
        self.functions.insert(Cow::Borrowed(name), fun);
    }
    fn check_existing_function(&self, diagnostics: &Diagnostics, name: &str, span: Span) -> bool {
        if let Some(existing) = self.functions.get(name) {
            match &existing.imp {
                FunctionImpl::Rebo(existing_name, _, _) => {
                    let mut spans = [self.rebo_functions[existing_name.as_str()].name.span, span];
                    spans.sort();
                    diagnostics.error(ErrorCode::DuplicateGlobal)
                        .with_info_label(spans[0], format!("`{}` first defined here", name))
                        .with_error_label(spans[1], "but also defined here")
                        .emit();
                }
                FunctionImpl::Rust(_) => diagnostics.error(ErrorCode::DuplicateGlobal)
                    .with_error_label(span, "a function with the same name is already provided externally")
                    .with_note("use a different name")
                    .emit()
            }
            return true;
        }
        false
    }
}

pub struct Depth {
    last_list: Vec<bool>,
}
impl Depth {
    pub fn start() -> Depth {
        Depth {
            last_list: vec![false],
        }
    }
    pub fn next(&self) -> Depth {
        Depth {
            last_list: self.last_list.iter().cloned().chain(Some(false)).collect(),
        }
    }
    pub fn last(&self) -> Depth {
        Depth {
            last_list: self.last_list.iter().cloned().chain(Some(true)).collect(),
        }
    }
}

impl Display for Depth {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for last in &self.last_list[..self.last_list.len() - 1] {
            let last = match last {
                false => "│   ",
                true => "    ",
            };
            write!(f, "{}", last)?;
        }
        let last = match self.last_list.last().unwrap() {
            false => "├──",
            true => "└──",
        };
        write!(f, "{}", last)
    }
}
