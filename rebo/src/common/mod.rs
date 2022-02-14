use std::borrow::Cow;
use std::fmt::{self, Display, Formatter};
use std::path::PathBuf;

use diagnostic::{Diagnostics, Span};
use indexmap::map::IndexMap;
use indexmap::set::IndexSet;
use typed_arena::Arena;

pub use values::{Typed, FromValue, Function, ExternalFunction, RequiredReboFunction, RequiredReboFunctionStruct, ExternalType, ExternalTypeType, FuzzyFloat, IntoValue, Struct, StructArc, Enum, EnumArc, Value, FunctionValue, ListArc, MapArc, DisplayValue, DebugValue, OctalValue, LowerHexValue, UpperHexValue, BinaryValue, LowerExpValue, UpperExpValue};

use crate::error_codes::ErrorCode;
use crate::{FileId, SpecificType, IncludeDirectory};
use crate::lexer::Lexer;
use crate::parser::{ExprEnumDefinition, ExprFunctionDefinition, ExprPatternTyped, ExprPatternUntyped, ExprStructDefinition, Spanned, ExprGenerics, ExprStatic, ExprPattern, Expr, Parser, Binding, ExprFunctionSignature, Parse, BindingId, ExprLabel};
use crate::typeck::types::{EnumType, FunctionType, StructType, Type};
use crate::typeck::TypeVar;
use std::rc::Rc;
use std::cell::RefCell;

mod values;

pub enum UserType<'a, 'i> {
    Struct(&'a ExprStructDefinition<'a, 'i>),
    Enum(&'a ExprEnumDefinition<'a, 'i>),
}
impl<'a, 'i> UserType<'a, 'i> {
    pub fn span(&self) -> Span {
        match self {
            UserType::Struct(s) => s.span(),
            UserType::Enum(e) => e.span(),
        }
    }
    pub fn variant_initializer_span(&self, variant_name: &str) -> Option<Span> {
        match self {
            UserType::Enum(enum_def) => enum_def.variants.iter()
                .filter(|variant| variant.name.ident == variant_name)
                .map(|variant| variant.name.span())
                .next(),
            _ => None,
        }
    }
    pub fn name_span(&self) -> Span {
        match self {
            UserType::Struct(s) => s.name.span,
            UserType::Enum(e) => e.name.span,
        }
    }
    pub fn generics(&self) -> Option<&'a ExprGenerics<'a, 'i>> {
        match self {
            UserType::Struct(s) => s.generics.as_ref(),
            UserType::Enum(e) => e.generics.as_ref(),
        }
    }
    pub fn unwrap_enum(&self) -> &'a ExprEnumDefinition<'a, 'i> {
        match self {
            UserType::Struct(_) => panic!("UserType::unwrap_enum called with struct"),
            UserType::Enum(e) => e,
        }
    }
}

/// Metadata / information needed before and/or during static analyses
pub struct MetaInfo<'a, 'i> {
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
    /// functions or associated functions found in the code
    ///
    /// Available after the parser.
    pub rebo_functions: IndexMap<Cow<'i, str>, &'a ExprFunctionDefinition<'a, 'i>>,
    /// anonymous functions found in the code
    ///
    /// Available after the parser.
    pub anonymous_rebo_functions: IndexMap<Span, (Vec<BindingId>, &'a ExprFunctionDefinition<'a, 'i>)>,
    /// functions defined in rust
    ///
    /// Available before the parser.
    pub external_functions: IndexMap<&'static str, ExternalFunction>,
    /// functions defined in rust
    ///
    /// Available before the parser.
    pub external_function_signatures: IndexMap<&'static str, ExprFunctionSignature<'a, 'i>>,
    /// enum and struct definitions found in the code
    ///
    /// Available after the first-pass of the parser.
    pub user_types: IndexMap<&'i str, UserType<'a, 'i>>,
    /// static variables defined in the code
    ///
    /// Available after the parser.
    pub statics: IndexMap<&'i str, &'a ExprStatic<'a, 'i>>,
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
}
impl<'a, 'i> MetaInfo<'a, 'i> {
    pub fn new() -> Self {
        MetaInfo {
            functions: IndexMap::new(),
            function_bindings: IndexMap::new(),
            rebo_functions: IndexMap::new(),
            anonymous_rebo_functions: IndexMap::new(),
            external_functions: IndexMap::new(),
            external_function_signatures: IndexMap::new(),
            user_types: IndexMap::new(),
            statics: IndexMap::new(),
            static_bindings: IndexSet::new(),
            function_types: IndexMap::new(),
            struct_types: IndexMap::new(),
            enum_types: IndexMap::new(),
            types: IndexMap::new(),
            external_types: IndexMap::new(),
            required_rebo_functions: IndexSet::new(),
        }
    }

    pub fn add_function(&mut self, diagnostics: &Diagnostics, name: Option<Cow<'i, str>>, fun: &'a ExprFunctionDefinition<'a, 'i>) {
        let arg_binding_ids = fun.sig.args.iter().map(|ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }| binding.id).collect();
        match name {
            Some(name) => {
                let function = Function::Rebo(name.to_string(), arg_binding_ids);
                if self.check_existing_function(diagnostics, &name, fun.sig.name.as_ref().unwrap().span) {
                    return;
                }
                self.functions.insert(name.clone(), function);
                self.rebo_functions.insert(name, fun);
            }
            None => {
                self.anonymous_rebo_functions.insert(fun.span(), (arg_binding_ids, fun));
            }
        }
    }
    pub fn add_external_function(&mut self, arena: &'a Arena<Expr<'a, 'i>>, diagnostics: &'i Diagnostics, fun: ExternalFunction) {
        let (file, _) = diagnostics.add_synthetic_file(fun.file_name, fun.code.to_string());
        if self.check_existing_function(diagnostics, fun.name, Span::new(FileId::synthetic(fun.file_name), 0, fun.code.len())) {
            return;
        }
        self.external_functions.insert(fun.name, fun.clone());
        self.functions.insert(Cow::Borrowed(fun.name), Function::Rust(fun.imp));
        let lexer = Lexer::new(diagnostics, file);
        let mut parser = Parser::new(IncludeDirectory::Path(PathBuf::new()), arena, lexer, diagnostics, self);
        let sig = ExprFunctionSignature::parse(&mut parser, Depth::start()).unwrap();
        self.external_function_signatures.insert(fun.name, sig);
    }
    fn add_enum_initializer_function(&mut self, diagnostics: &Diagnostics, enum_name: String, variant_name: String) {
        let name = format!("{}::{}", enum_name, variant_name);
        let span = self.user_types[enum_name.as_str()].variant_initializer_span(&variant_name).unwrap();
        if self.check_existing_function(diagnostics, &name, span) {
            return;
        }
        self.functions.insert(Cow::Owned(name), Function::EnumInitializer(enum_name, variant_name));
    }
    fn check_existing_function(&self, diagnostics: &Diagnostics, name: &str, span: Span) -> bool {
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
                    duplicate(self.rebo_functions[existing_name.as_str()].sig.name.as_ref().unwrap().span, span);
                }
                Function::Rust(_) => diagnostics.error(ErrorCode::DuplicateGlobal)
                    .with_error_label(span, "a function with the same name is already provided externally")
                    .with_note("use a different name")
                    .emit(),
                Function::EnumInitializer(enum_name, variant_name) => {
                    let new_span = self.user_types[enum_name.as_str()].variant_initializer_span(&variant_name).unwrap();
                    duplicate(new_span, span);
                }
            }
            return true;
        }
        false
    }
    pub fn add_required_rebo_function(&mut self, required_rebo_function: RequiredReboFunctionStruct) {
        if self.required_rebo_functions.contains(&required_rebo_function) {
            panic!("Required rebo function `{}` already added previously", required_rebo_function.name);
        }
        self.required_rebo_functions.insert(required_rebo_function);
    }
    pub fn add_external_type<T: ExternalType>(&mut self, arena: &'a Arena<Expr<'a, 'i>>, diagnostics: &'i Diagnostics) {
        let (file, _) = diagnostics.add_synthetic_file(T::FILE_NAME, T::CODE.to_string());
        self.external_types.insert(T::TYPE.type_name(), T::TYPE);

        let lexer = Lexer::new(diagnostics, file);
        let parser = Parser::new(IncludeDirectory::Path(PathBuf::new()), arena, lexer, diagnostics, self);
        parser.parse_ast().unwrap();
    }
    pub fn add_enum(&mut self, diagnostics: &Diagnostics, enum_def: &'a ExprEnumDefinition<'a, 'i>) {
        self.add_user_type(diagnostics, enum_def.name.ident, UserType::Enum(enum_def));
        for variant in enum_def.variants.iter() {
            if variant.fields.is_some() {
                let enum_name = enum_def.name.ident.to_string();
                let variant_name = variant.name.ident.to_string();
                self.add_enum_initializer_function(diagnostics, enum_name, variant_name);
            }
        }
    }
    pub fn add_struct(&mut self, diagnostics: &Diagnostics, struct_def: &'a ExprStructDefinition<'a, 'i>) {
        self.add_user_type(diagnostics, struct_def.name.ident, UserType::Struct(struct_def))
    }
    pub fn add_user_type(&mut self, diagnostics: &Diagnostics, name: &'i str, user_type: UserType<'a, 'i>) {
        let new_span = user_type.span();
        if let Some(old) = self.user_types.insert(name, user_type) {
            let mut spans = [old.span(), new_span];
            spans.sort();
            diagnostics.error(ErrorCode::DuplicateGlobal)
                .with_info_label(spans[0], "first defined here")
                .with_error_label(spans[1], "also defined here")
                .emit();
        }
    }
    pub fn add_static(&mut self, diagnostics: &Diagnostics, static_def: &'a ExprStatic<'a, 'i>) {
        let new_span = static_def.span();
        let name = match &static_def.sig.pattern {
            ExprPattern::Untyped(untyped) => untyped.binding.ident.ident,
            ExprPattern::Typed(typed) => typed.pattern.binding.ident.ident,
        };
        if let Some(old) = self.statics.insert(name, static_def) {
            let mut spans = [old.span(), new_span];
            spans.sort();
            diagnostics.error(ErrorCode::DuplicateGlobal)
                .with_info_label(spans[0], "first defined here")
                .with_error_label(spans[1], "also defined here")
                .emit();
        }
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

#[derive(Debug)]
pub struct BlockStack<'a, 'i, T> {
    blocks: Rc<RefCell<Vec<(BlockType<'a, 'i>, T)>>>,
}
#[derive(Clone, Debug)]
pub enum BlockType<'a, 'i> {
    Function,
    Loop(Option<&'a ExprLabel<'i>>),
    While(Option<&'a ExprLabel<'i>>),
    For(Option<&'a ExprLabel<'i>>),
}
#[must_use]
pub struct BlockGuard<'a, 'i, T> {
    blocks: Rc<RefCell<Vec<(BlockType<'a, 'i>, T)>>>
}
impl<'a, 'i, T> Drop for BlockGuard<'a, 'i, T> {
    fn drop(&mut self) {
        self.blocks.borrow_mut().pop();
    }
}
impl<'a, 'i, T: Clone> BlockStack<'a, 'i, T> {
    pub fn new() -> Self {
        BlockStack { blocks: Rc::new(RefCell::new(Vec::new())) }
    }
    pub fn push_block(&self, typ: BlockType<'a, 'i>, data: T) -> BlockGuard<'a, 'i, T> {
        self.blocks.borrow_mut().push((typ, data));
        BlockGuard { blocks: Rc::clone(&self.blocks) }
    }
    pub fn get_loop_like(&self, label: Option<&ExprLabel<'_>>) -> Option<(BlockType<'a, 'i>, T)> {
        match label {
            Some(label) => self.get_loop_like_named(label),
            None => self.get_loop_like_unnamed(),
        }
    }
    fn get_loop_like_unnamed(&self) -> Option<(BlockType<'a, 'i>, T)> {
        self.blocks.borrow().iter()
            .rev()
            .find(|(typ, _)| match typ {
                BlockType::Function => false,
                BlockType::Loop(_) | BlockType::While(_) | BlockType::For(_) => true,
            }).cloned()
    }
    fn get_loop_like_named(&self, label: &ExprLabel<'_>) -> Option<(BlockType<'a, 'i>, T)> {
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
    pub fn get_function(&self) -> Option<(BlockType<'a, 'i>, T)> {
        self.blocks.borrow().iter()
            .rev()
            .find(|(typ, _)| match typ {
                BlockType::Function => true,
                BlockType::Loop(_) | BlockType::While(_) | BlockType::For(_) => false,
            }).cloned()
    }
}
