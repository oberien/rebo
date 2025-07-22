use std::borrow::Cow;
use std::fmt::{self, Display, Formatter};

use diagnostic::{Diagnostics, Span};
use indexmap::map::{Entry, IndexMap};
use indexmap::set::IndexSet;
use typed_arena::Arena;

pub use values::{BinaryValue, DebugValue, DeepCopy, DisplayValue, Enum, EnumArc, ExternalFunction, ExternalType, ExternalTypeType, FromValue, Function, FunctionValue, FuzzyFloat, IntoValue, ListArc, LowerExpValue, LowerHexValue, MapArc, OctalValue, RequiredReboFunction, RequiredReboFunctionStruct, SetArc, Struct, StructArc, Typed, UpperExpValue, UpperHexValue, Value};

use crate::error_codes::ErrorCode;
use crate::{FileId, IncludeConfig, SpecificType};
use crate::lexer::{Lexer, TokenIdent};
use crate::parser::{AddClone, Binding, BindingId, Expr, ExprEnumDefinition, ExprFunctionDefinition, ExprFunctionSignature, ExprGenerics, ExprLabel, ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprStatic, ExprStructDefinition, Parse, Parser};
use crate::typeck::types::{EnumType, FunctionType, StructType, Type};
use crate::typeck::TypeVar;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::FromIterator;
use intervaltree::{Element, IntervalTree};
use rebo::parser::ScopeType;

mod values;
pub mod expr_gen;
mod spanned;
pub use spanned::*;

#[derive(Debug)]
pub enum UserType<'i> {
    Struct(&'i ExprStructDefinition<'i>),
    Enum(&'i ExprEnumDefinition<'i>),
}
impl<'i> UserType<'i> {
    pub fn variant_initializer_diagnostics_span(&self, variant_name: &str) -> Option<Span> {
        match self {
            UserType::Enum(enum_def) => enum_def.variants.iter()
                .filter(|variant| variant.name.ident == variant_name)
                .map(|variant| variant.name.diagnostics_span())
                .next(),
            _ => None,
        }
    }
    pub fn name_span(&self) -> SpanWithId {
        match self {
            UserType::Struct(s) => s.name.span,
            UserType::Enum(e) => e.name.span,
        }
    }
    pub fn ident(&self) -> TokenIdent<'i> {
        match self {
            UserType::Struct(s) => s.name,
            UserType::Enum(e) => e.name,
        }
    }
    pub fn generics(&self) -> Option<&'i ExprGenerics<'i>> {
        match self {
            UserType::Struct(s) => s.generics.as_ref(),
            UserType::Enum(e) => e.generics.as_ref(),
        }
    }
    pub fn unwrap_enum(&self) -> &'i ExprEnumDefinition<'i> {
        match self {
            UserType::Struct(_) => panic!("UserType::unwrap_enum called with struct"),
            UserType::Enum(e) => e,
        }
    }
}
impl<'i> Spanned for UserType<'i> {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            UserType::Struct(s) => s.span_with_id(),
            UserType::Enum(e) => e.span_with_id(),
        }
    }
}

/// Metadata / information needed before and/or during static analyses
pub struct MetaInfo<'i> {
    /// Tree of all the spans of expressions
    ///
    /// Available after the parser
    pub expression_spans: IntervalTree<(FileId, usize), &'i Expr<'i>>,
    /// map from `ExprInclude::span` to its FileId
    ///
    /// Available after parser's first-pass.
    // key must be Span as it's parsed multiple times resulting in different SpanIds
    pub included_files: IndexMap<Span, FileId>,
    /// map of all rust / rebo functions or associated functions to their implementation reference
    ///
    /// Available after the parser's first-pass.
    /// Used for function resolution in the parser's second pass.
    /// Also used in the vm to look up function implementations.
    pub functions: IndexMap<Cow<'i, str>, Function>,
    /// map of the bindings of functions to their function-names
    ///
    /// Available after the parser.
    pub function_bindings: IndexMap<Binding<'i>, String>,
    /// function names found in code
    ///
    /// Available after the parser's first pass.
    pub rebo_function_names: IndexSet<(String, TokenIdent<'i>)>,
    /// functions or associated functions found in the code
    ///
    /// Available after the parser.
    pub rebo_functions: IndexMap<Cow<'i, str>, &'i ExprFunctionDefinition<'i>>,
    /// anonymous functions found in the code
    ///
    /// Available after the parser.
    pub anonymous_rebo_functions: IndexMap<SpanWithId, (Vec<BindingId>, &'i ExprFunctionDefinition<'i>)>,
    /// functions defined in rust
    ///
    /// Available before the parser.
    pub external_functions: IndexMap<&'static str, ExternalFunction>,
    /// functions defined in rust
    ///
    /// Available before the parser.
    pub external_function_signatures: IndexMap<&'static str, ExprFunctionSignature<'i>>,
    /// variables defined by the user
    ///
    /// Available before the parser
    pub external_values: IndexMap<Binding<'i>, Value>,
    /// map names to enum / struct definitions found in the code
    ///
    /// Available after the first-pass of the parser.
    pub user_types: IndexMap<&'i str, UserType<'i>>,
    /// static variables defined in the code
    ///
    /// Available after the parser.
    pub statics: IndexMap<&'i str, &'i ExprStatic<'i>>,
    /// static variable names defined in the code
    ///
    /// Available after the first-pass of the parser
    pub static_bindings: IndexSet<Binding<'i>>,
    /// function types with resolved argument and return types
    ///
    /// Contains external function types before parser.
    /// Contains all function types after the beginning of typeck.
    pub function_types: IndexMap<Cow<'i, str>, FunctionType>,
    /// struct types with resolved field types
    ///
    /// Available after the beginning of typeck.
    pub struct_types: IndexMap<&'i str, StructType>,
    /// enum types with resolved field types
    ///
    /// Available after the beginning of typeck.
    pub enum_types: IndexMap<&'i str, EnumType>,
    /// resolved types
    ///
    /// Available after typeck.
    pub types: IndexMap<TypeVar, Type>,
    pub external_types: IndexMap<String, SpecificType>,
    pub required_rebo_functions: IndexSet<RequiredReboFunctionStruct>,
    /// function-name -> (dot-state-graph, generated-code)
    ///
    /// Available after the parser. Used for debugging
    pub generators: HashMap<String, (String, String)>,
}
impl<'i> MetaInfo<'i> {
    pub fn new() -> Self {
        MetaInfo {
            expression_spans: <IntervalTree<_, _> as FromIterator<Element<_, _>>>::from_iter([]),
            included_files: IndexMap::new(),
            functions: IndexMap::new(),
            function_bindings: IndexMap::new(),
            rebo_function_names: IndexSet::new(),
            rebo_functions: IndexMap::new(),
            anonymous_rebo_functions: IndexMap::new(),
            external_functions: IndexMap::new(),
            external_function_signatures: IndexMap::new(),
            external_values: IndexMap::new(),
            user_types: IndexMap::new(),
            statics: IndexMap::new(),
            static_bindings: IndexSet::new(),
            function_types: IndexMap::new(),
            struct_types: IndexMap::new(),
            enum_types: IndexMap::new(),
            types: IndexMap::new(),
            external_types: IndexMap::new(),
            required_rebo_functions: IndexSet::new(),
            generators: HashMap::new(),
        }
    }

    pub fn get_function_signature(&self, name: &str, def_span: SpanWithId) -> Option<&ExprFunctionSignature<'i>> {
        if let Some(fun) = self.anonymous_rebo_functions.get(&def_span) {
            return Some(&fun.1.sig);
        }
        if let Some(sig) = self.external_function_signatures.get(name) {
            return Some(sig);
        }
        if let Some(fun) = self.rebo_functions.get(name) {
            return Some(&fun.sig);
        }
        None
    }
    pub fn add_function(&mut self, diagnostics: &Diagnostics<ErrorCode>, name: Option<Cow<'i, str>>, fun: &'i ExprFunctionDefinition<'i>) {
        let arg_binding_ids = fun.sig.args.iter().map(|ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }| binding.id).collect();
        match name {
            Some(name) => {
                let function = Function::Rebo(name.to_string(), arg_binding_ids);
                if self.check_existing_function(diagnostics, &name, fun.sig.name.as_ref().unwrap().diagnostics_span()) {
                    return;
                }
                self.functions.insert(name.clone(), function);
                self.rebo_functions.insert(name, fun);
            }
            None => {
                self.anonymous_rebo_functions.insert(fun.span_with_id(), (arg_binding_ids, fun));
            }
        }
    }
    pub fn add_external_function(&mut self, arena: &'i Arena<Expr<'i>>, diagnostics: &'i Diagnostics<ErrorCode>, fun: ExternalFunction) {
        let (file, _) = diagnostics.add_synthetic_named_file(fun.file_name, fun.code.to_string());
        if self.check_existing_function(diagnostics, fun.name, Span::new(FileId::synthetic_named(fun.file_name), 0, fun.code.len())) {
            return;
        }
        self.external_functions.insert(fun.name, fun.clone());
        self.functions.insert(Cow::Borrowed(fun.name), Function::Rust(fun.imp));
        let lexer = Lexer::new(diagnostics, file);
        let mut parser = Parser::new(IncludeConfig::DisallowFromFiles, HashMap::new(), arena, lexer, diagnostics, self, AddClone::No);
        let _guard = parser.push_scope(ScopeType::Global);
        let _guard = parser.push_scope(ScopeType::File);
        let sig = ExprFunctionSignature::parse(&mut parser, Depth::start()).unwrap();
        self.external_function_signatures.insert(fun.name, sig);
    }
    fn add_enum_initializer_function(&mut self, diagnostics: &Diagnostics<ErrorCode>, enum_name: String, variant_name: String) {
        let name = format!("{}::{}", enum_name, variant_name);
        let diagnostics_span = self.user_types[enum_name.as_str()].variant_initializer_diagnostics_span(&variant_name).unwrap();
        if self.check_existing_function(diagnostics, &name, diagnostics_span) {
            return;
        }
        self.functions.insert(Cow::Owned(name), Function::EnumInitializer(enum_name, variant_name));
    }
    fn check_existing_function(&self, diagnostics: &Diagnostics<ErrorCode>, name: &str, diagnostics_span: Span) -> bool {
        let duplicate = |a: Span, b: Span| {
            let mut spans = [a, b];
            spans.sort();
            diagnostics.error(ErrorCode::DuplicateGlobal)
                .with_info_label(spans[0], format!("`{}` first defined here", name))
                .with_error_label(spans[1], "but also defined here")
                .emit();
        };
        if let Some(existing) = self.functions.get(name) {
            match existing {
                Function::Rebo(existing_name, _) => {
                    duplicate(self.rebo_functions[existing_name.as_str()].sig.name.as_ref().unwrap().diagnostics_span(), diagnostics_span);
                }
                Function::Rust(_) => diagnostics.error(ErrorCode::DuplicateGlobal)
                    .with_error_label(diagnostics_span, "a function with the same name is already provided externally")
                    .with_note("use a different name")
                    .emit(),
                Function::EnumInitializer(enum_name, variant_name) => {
                    let new_diagnostics_span = self.user_types[enum_name.as_str()].variant_initializer_diagnostics_span(variant_name).unwrap();
                    duplicate(new_diagnostics_span, diagnostics_span);
                }
            }
            return true;
        }
        false
    }
    pub fn add_required_rebo_function(&mut self, required_rebo_function: RequiredReboFunctionStruct, diagnostics: &'i Diagnostics<ErrorCode>) {
        if self.required_rebo_functions.contains(&required_rebo_function) {
            panic!("Required rebo function `{}` already added previously", required_rebo_function.name);
        }
        diagnostics.add_synthetic_named_file(required_rebo_function.generics_file_name, required_rebo_function.generics_file_content.to_string());
        self.required_rebo_functions.insert(required_rebo_function);
    }
    pub fn add_external_type<T: ExternalType>(&mut self, arena: &'i Arena<Expr<'i>>, diagnostics: &'i Diagnostics<ErrorCode>) {
        let (file, _) = diagnostics.add_synthetic_named_file(T::FILE_NAME, T::CODE.to_string());
        self.external_types.insert(T::typ().type_name(), T::typ());

        let lexer = Lexer::new(diagnostics, file);
        let parser = Parser::new(IncludeConfig::DisallowFromFiles, HashMap::new(), arena, lexer, diagnostics, self, AddClone::No);
        parser.parse_ast().unwrap();
    }
    pub fn add_external_code(&mut self, filename: String, code: String, arena: &'i Arena<Expr<'i>>, diagnostics: &'i Diagnostics<ErrorCode>, add_clone: AddClone) {
        let (file, _) = diagnostics.add_file(filename, code);

        let lexer = Lexer::new(diagnostics, file);
        let parser = Parser::new(IncludeConfig::DisallowFromFiles, HashMap::new(), arena, lexer, diagnostics, self, add_clone);
        parser.parse_ast().unwrap();
    }
    pub fn add_enum(&mut self, diagnostics: &Diagnostics<ErrorCode>, enum_def: &'i ExprEnumDefinition<'i>) {
        self.add_user_type(diagnostics, enum_def.name.ident, UserType::Enum(enum_def));
        for variant in enum_def.variants.iter() {
            if variant.fields.is_some() {
                let enum_name = enum_def.name.ident.to_string();
                let variant_name = variant.name.ident.to_string();
                self.add_enum_initializer_function(diagnostics, enum_name, variant_name);
            }
        }
    }
    pub fn add_struct(&mut self, diagnostics: &Diagnostics<ErrorCode>, struct_def: &'i ExprStructDefinition<'i>) {
        self.add_user_type(diagnostics, struct_def.name.ident, UserType::Struct(struct_def))
    }
    pub fn add_user_type(&mut self, diagnostics: &Diagnostics<ErrorCode>, name: &'i str, user_type: UserType<'i>) {
        match self.user_types.entry(name) {
            Entry::Vacant(vacant) => { vacant.insert(user_type); },
            Entry::Occupied(occupied) => {
                let old = occupied.get();
                let mut spans = [old.diagnostics_span(), user_type.diagnostics_span()];
                spans.sort();
                diagnostics.error(ErrorCode::DuplicateGlobal)
                    .with_info_label(spans[0], "first defined here")
                    .with_error_label(spans[1], "also defined here")
                    .emit();
            }
        }
    }
    pub fn add_static(&mut self, diagnostics: &Diagnostics<ErrorCode>, static_def: &'i ExprStatic<'i>) {
        let name = match &static_def.sig.pattern {
            ExprPattern::Untyped(untyped) => untyped.binding.ident.ident,
            ExprPattern::Typed(typed) => typed.pattern.binding.ident.ident,
        };
        match self.statics.entry(name) {
            Entry::Vacant(vacant) => { vacant.insert(static_def); }
            Entry::Occupied(occupied) => {
                let old = occupied.get();
                let mut spans = [old.diagnostics_span(), static_def.diagnostics_span()];
                spans.sort();
                diagnostics.error(ErrorCode::DuplicateGlobal)
                    .with_info_label(spans[0], "first defined here")
                    .with_error_label(spans[1], "also defined here")
                    .emit();
            }
        }
    }
}
impl<'i> Default for MetaInfo<'i> {
    fn default() -> Self {
        Self::new()
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
    pub fn duplicate(&self) -> Depth {
        Depth {
            last_list: self.last_list.clone(),
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

#[derive(Debug)]
pub struct BlockStack<'i, T> {
    blocks: Rc<RefCell<Vec<(BlockType<'i>, T)>>>,
}
#[derive(Clone, Debug)]
pub enum BlockType<'i> {
    Function,
    Loop(Option<&'i ExprLabel<'i>>),
    While(Option<&'i ExprLabel<'i>>),
    For(Option<&'i ExprLabel<'i>>),
}
#[must_use]
pub struct BlockGuard<'i, T> {
    blocks: Rc<RefCell<Vec<(BlockType<'i>, T)>>>
}
impl<'i, T> Drop for BlockGuard<'i, T> {
    fn drop(&mut self) {
        self.blocks.borrow_mut().pop();
    }
}
impl<'i, T: Clone> BlockStack<'i, T> {
    pub fn new() -> Self {
        BlockStack { blocks: Rc::new(RefCell::new(Vec::new())) }
    }
    pub fn push_block(&self, typ: BlockType<'i>, data: T) -> BlockGuard<'i, T> {
        self.blocks.borrow_mut().push((typ, data));
        BlockGuard { blocks: Rc::clone(&self.blocks) }
    }
    pub fn get_loop_like(&self, label: Option<&ExprLabel<'_>>) -> Option<(BlockType<'i>, T)> {
        match label {
            Some(label) => self.get_loop_like_named(label),
            None => self.get_loop_like_unnamed(),
        }
    }
    fn get_loop_like_unnamed(&self) -> Option<(BlockType<'i>, T)> {
        self.blocks.borrow().iter()
            .rev()
            .find(|(typ, _)| match typ {
                BlockType::Function => false,
                BlockType::Loop(_) | BlockType::While(_) | BlockType::For(_) => true,
            }).cloned()
    }
    fn get_loop_like_named(&self, label: &ExprLabel<'_>) -> Option<(BlockType<'i>, T)> {
        self.blocks.borrow().iter()
            .rev()
            .find(|(typ, _)| match typ {
                BlockType::Function => false,
                BlockType::Loop(Some(l))
                | BlockType::While(Some(l))
                | BlockType::For(Some(l)) => *l == label,
                BlockType::Loop(None) | BlockType::While(None) | BlockType::For(None) => false,
            }).cloned()
    }
    pub fn get_function(&self) -> Option<(BlockType<'i>, T)> {
        self.blocks.borrow().iter()
            .rev()
            .find(|(typ, _)| match typ {
                BlockType::Function => true,
                BlockType::Loop(_) | BlockType::While(_) | BlockType::For(_) => false,
            }).cloned()
    }
}
