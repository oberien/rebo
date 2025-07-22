use std::{fmt, mem};
use std::borrow::Cow;
use std::collections::{btree_map::Entry, BTreeMap, BTreeSet, HashMap, HashSet};

use typed_arena::Arena;
use diagnostic::{DiagnosticBuilder, Diagnostics, FileId, Span};

use crate::lexer::{Lexer, Token, TokenBlockComment, TokenIdent, TokenLineComment, TokenMut, TokenType};
use crate::error_codes::ErrorCode;

mod expr;
mod precedence;
mod parse;
mod scope;
mod first_pass;

pub use expr::*;
pub use parse::{Parse, Separated};
pub use scope::{BindingId, ScopeType};
use crate::common::{Depth, Function, MetaInfo};
use indexmap::map::IndexMap;
use itertools::Itertools;
use crate::parser::scope::Scope;
use std::cell::RefCell;
use std::ops::Range;
use std::rc::{Rc, Weak};
use indexmap::set::IndexSet;
use intervaltree::Element;
use rebo::common::{SpanWithId, Spanned};
use crate::IncludeConfig;

#[derive(Debug, Clone)]
pub enum Error {
    /// Parsing encountered an unrecoverable error and a diagnostic was emitted. Abort.
    Abort,
    /// An unexpected EOF was encountered. No diagnostics has been emitted yet.
    UnexpectedEof(Span),
}
#[derive(Debug, Clone)]
pub enum InternalError {
    /// We can't recover, return error to caller
    Error(Error),
    /// This function doesn't handle the tokens in the token stream. Everything was rolled back,
    /// the next function should be checked.
    Backtrack(Backtrack),
}
impl From<Error> for InternalError {
    fn from(e: Error) -> Self {
        InternalError::Error(e)
    }
}
impl From<crate::lexer::Error> for InternalError {
    fn from(e: crate::lexer::Error) -> Self {
        match e {
            crate::lexer::Error::Abort => InternalError::Error(Error::Abort),
            crate::lexer::Error::UnexpectedEof(span) => InternalError::Error(Error::UnexpectedEof(span)),
        }
    }
}
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Backtrack {
    span: Span,
    expected: Cow<'static, [Expected]>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Expected {
    Token(TokenType),
    Type,
}
impl Expected {
    const MATH_OP: &'static [Expected] = &[
        Expected::Token(TokenType::Plus),
        Expected::Token(TokenType::Minus),
        Expected::Token(TokenType::Star),
        Expected::Token(TokenType::Slash),
        Expected::Token(TokenType::Percent),
        Expected::Token(TokenType::Circumflex),
    ];
    const BOOL_OP: &'static [Expected] = &[
        Expected::Token(TokenType::DoubleAmp),
        Expected::Token(TokenType::DoublePipe),
    ];
    const COMPARE_OP: &'static [Expected] = &[
        Expected::Token(TokenType::LessThan),
        Expected::Token(TokenType::LessEquals),
        Expected::Token(TokenType::Equals),
        Expected::Token(TokenType::NotEquals),
        Expected::Token(TokenType::GreaterEquals),
        Expected::Token(TokenType::GreaterThan),
    ];
}

#[derive(Debug)]
pub struct Ast<'i> {
    pub exprs: Vec<&'i Expr<'i>>,
}

impl<'i> fmt::Display for Ast<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for expr in &self.exprs {
            writeln!(f, "{}", expr)?;
        }
        Ok(())
    }
}

pub struct Parser<'i, 'm> {
    /// directory to search in when including files
    include_config: IncludeConfig,
    /// externally provided fixed includes; name/path -> content
    external_includes: HashMap<String, String>,
    /// arena to allocate expressions into
    arena: &'i Arena<Expr<'i>>,
    /// tokens to be consumed
    lexer: Lexer<'i>,
    diagnostics: &'i Diagnostics<ErrorCode>,
    /// pre-info to add first-pass definitions to
    meta_info: &'m mut MetaInfo<'i>,
    /// already parsed expressions in the first-pass, to be consumed by the second pass
    ///
    /// (FileId, expr-start) -> Expr
    pre_parsed: HashMap<(FileId, usize), &'i Expr<'i>>,
    /// stack of scopes with bindings that are still live
    scopes: Rc<RefCell<Vec<Scope<'i>>>>,
    /// track captures within closures
    captures: Option<IndexSet<Binding<'i>>>,
    memoization: IndexMap<(FileId, usize), (&'i Expr<'i>, ParseUntil)>,
    binding_memoization: BTreeMap<Span, Binding<'i>>,
    generic_memoization: HashSet<SpanWithId>,
    /// List of all encountered backtracks.
    ///
    /// If an error should be printed out, the backtrack that starts furthest in the code is used.
    /// For example `List::of(1,2;3)` is parsed as `Variable(List::of)` followed by a `Parenthesized`
    /// error at the first `,` because it's not allowed in `Parenthesized`.
    /// By using this list, we can see that the parser that actually got furthest was `FunctionCall`
    /// and print its error message instead.
    backtracks: BTreeSet<Backtrack>,
    /// Whether to add clone impls after the first passes.
    ///
    /// Adding clone impls parses a new ast, at which point the clone impls would be added again,
    /// resulting in a stack overflow.
    /// Setting this to `AddClone::Yes` during that process fixes the problem.
    add_clone: AddClone,
}

pub struct ScopeGuard<'i> {
    scopes: Weak<RefCell<Vec<Scope<'i>>>>,
}
impl<'i> ScopeGuard<'i> {
    /// Don't remove this scope, it'll exist until all scopes are dropped
    fn dont_remove(mut self) {
        let weak = mem::replace(&mut self.scopes, Weak::new());
        // this drop only drops the Weak and doesn't pop the scope from the Scopes
        drop(weak);
    }
}
impl<'i> Drop for ScopeGuard<'i> {
    fn drop(&mut self) {
        // only pop the scope if we want to remove it (i.e. dont_remove hasn't been called)
        if let Some(scopes) = self.scopes.upgrade(){
            scopes.borrow_mut().pop().unwrap();
        }
    }
}

pub enum AddClone {
    Yes,
    No,
}

/// All expression parsing function consume whitespace and comments before tokens, but not after.
impl<'i, 'm> Parser<'i, 'm> {
    pub fn new(
        include_config: IncludeConfig,
        external_includes: HashMap<String, String>,
        arena: &'i Arena<Expr<'i>>,
        lexer: Lexer<'i>,
        diagnostics: &'i Diagnostics<ErrorCode>,
        meta_info: &'m mut MetaInfo<'i>,
        add_clone: AddClone,
    ) -> Self {
        Parser {
            include_config,
            external_includes,
            arena,
            lexer,
            diagnostics,
            meta_info,
            pre_parsed: HashMap::new(),
            scopes: Rc::new(RefCell::new(vec![])),
            captures: None,
            memoization: IndexMap::new(),
            binding_memoization: BTreeMap::new(),
            generic_memoization: HashSet::new(),
            backtracks: BTreeSet::new(),
            add_clone,
        }
    }

    pub fn parse_ast(mut self) -> Result<Ast<'i>, Error> {
        trace!("parse_ast");
        self.first_pass(Depth::start());
        self.second_pass(Depth::start());
        // add statics and external values to global scope
        let _guard = self.push_scope(ScopeType::Global);
        self.add_statics();
        for binding in self.meta_info.external_values.keys().copied() {
            self.scopes.borrow_mut().last_mut().unwrap().idents.insert(binding.ident.ident.to_string(), binding);
        }

        let body: BlockBody = self.parse_file_content()?;
        // make sure everything parsed during first-pass was consumed and used by the second pass
        assert!(self.pre_parsed.is_empty(), "not everything from first-pass was consumed: {:?}", self.pre_parsed);
        assert!(matches!(self.peek_token(0), Ok(Token::Eof(_))), "not all tokens were consumed: {}", self.lexer.iter().map(|t| format!("    {:?}", t)).join("\n"));

        // build expression span IntervalTree
        self.meta_info.expression_spans = body.exprs.iter().copied()
            .map(|expr| Element {
                range: Range {
                    start: (expr.file_id(), expr.start()),
                    end: (expr.file_id(), expr.end()),
                },
                value: expr,
            }).collect();

        Ok(Ast { exprs: body.exprs })
    }
    fn parse_file_content(&mut self) -> Result<BlockBody<'i>, Error> {
        // remove all scopes except the global one when parsing a new file
        let old_scopes: Vec<_> = self.scopes.borrow_mut().drain(1..).collect();
        // file scope
        let guard = self.push_scope(ScopeType::File);
        let res = match self.parse(Depth::start()) {
            Ok(body) => Ok(body),
            Err(InternalError::Backtrack(backtrack)) => {
                self.backtracks.insert(backtrack);
                let last = self.backtracks.iter().next_back().unwrap();
                self.diagnostic_expected(ErrorCode::InvalidExpression, last.span, &last.expected);
                Err(Error::Abort)
            },
            Err(InternalError::Error(e)) => Err(e),
        };
        // restore old scopes
        drop(guard);
        assert_eq!(1, self.scopes.borrow().len());
        self.scopes.borrow_mut().extend(old_scopes);
        res
    }

    fn add_statics(&mut self) {
        // statics
        for binding in self.meta_info.static_bindings.clone() {
            // the memoized binding will be used
            self.add_binding_internal(binding.ident.ident.to_string(), binding.ident, binding.mutable, false);
        }
        // functions
        for (name, function) in self.meta_info.functions.clone() {
            let ident = match function {
                Function::Rust(_) => {
                    let sig = &self.meta_info.external_function_signatures[name.as_ref()];
                    sig.name.unwrap()
                },
                Function::EnumInitializer(enum_name, variant) => {
                    self.meta_info.user_types[enum_name.as_str()].unwrap_enum().variants.iter()
                        .map(|var| var.name)
                        .find(|name| name.ident == variant)
                        .unwrap()
                }
                // added separately
                Function::Rebo(..) => continue,
            };
            let name = name.clone().into_owned();
            let binding = self.add_binding_internal(name.clone(), ident, None, false);
            self.meta_info.function_bindings.insert(binding, name);
        }
        for (name, ident) in self.meta_info.rebo_function_names.clone() {
            let binding = self.add_binding_internal(name.clone(), ident, None, false);
            self.meta_info.function_bindings.insert(binding, name);
        }

        // add <T>::clone() method to all types
        if let AddClone::Yes = self.add_clone {
            let types: Vec<_> = self.meta_info.user_types.iter().map(|(name, typ)| (*name, typ.generics())).collect();
            let mut clone_code = String::new();
            for (type_name, generics) in types {
                let name = format!("{type_name}::clone");
                if self.get_binding(&name).is_some() {
                    continue;
                }

                let generics = generics.map(ToString::to_string).unwrap_or_default();
                clone_code.push_str(&format!("impl {type_name}{generics} {{\n    fn clone(self) -> {type_name}{generics} {{\n        __internal_clone_(self)\n    }}\n}}\n"));
            }
            self.meta_info.add_external_code("external-clone.re".to_string(), clone_code, self.arena, self.diagnostics, AddClone::No);
        }
    }

    fn add_binding(&mut self, ident: TokenIdent<'i>, mutable: Option<TokenMut>) -> Binding<'i> {
        self.add_binding_internal(ident.ident.to_string(), ident, mutable, false)
    }
    fn add_rogue_binding(&mut self, ident: TokenIdent<'i>, mutable: Option<TokenMut>) -> Binding<'i> {
        self.add_binding_internal(ident.ident.to_string(), ident, mutable, true)
    }
    fn add_binding_internal(&mut self, name: String, ident: TokenIdent<'i>, mutable: Option<TokenMut>, rogue: bool) -> Binding<'i> {
        // can't use SpanWithId as the same span can be parsed multiple times
        let binding = match self.binding_memoization.entry(ident.diagnostics_span()) {
            Entry::Vacant(vacant) => {
                let id = BindingId::unique();
                let span = match mutable {
                    Some(mutable) => mutable.span | ident.span,
                    None => SpanWithId::from(ident.span.diagnostics_span()),
                };
                let binding = Binding { id, mutable, ident, rogue, span };
                vacant.insert(binding);
                binding
            }
            Entry::Occupied(occupied) => *occupied.get(),
        };
        self.scopes.borrow_mut().last_mut().unwrap().idents.insert(name, binding);
        binding
    }
    fn get_binding(&mut self, ident: &str) -> Option<Binding<'i>> {
        let mut track_capture = false;
        for scope in self.scopes.borrow().iter().rev() {
            if let Some(ident) = scope.idents.get(ident) {
                if track_capture && !matches!(scope.typ, ScopeType::Global) {
                    if let Some(captures) = self.captures.as_mut() {
                        captures.insert(*ident);
                    }
                }
                return Some(*ident);
            }
            if matches!(scope.typ, ScopeType::Function) {
                track_capture = true;
            }
        }
        None
    }
    /// THIS DOES NOT PERFORM CLOSURE CAPTURE ANALYSIS - USE AT OWN RISK
    fn get_binding_unsafe_unsafe_unsafe(&self, ident: &str) -> Option<Binding<'i>> {
        for scope in self.scopes.borrow().iter().rev() {
            if let Some(binding) = scope.idents.get(ident) {
                return Some(binding.clone())
            }
        }
        None
    }
    fn add_generic(&mut self, generic: Generic<'i>) {
        if self.get_generic(generic.def_ident.ident).is_some() && self.generic_memoization.contains(&generic.def_ident.span_with_id()) {
            return;
        }
        self.generic_memoization.insert(generic.def_ident.span_with_id());
        match self.get_generic(generic.def_ident.ident) {
            Some(prev) => self.diagnostics.error(ErrorCode::DuplicateGeneric)
                .with_error_label(generic.def_ident.diagnostics_span(), "this generic has already been defined")
                .with_info_label(prev.def_ident.diagnostics_span(), "previously defined here")
                .emit(),
            None => {
                self.scopes.borrow_mut().last_mut().unwrap().generics.insert(generic.def_ident.ident, generic);
            }
        }
    }
    fn get_generic(&self, ident: &str) -> Option<Generic<'i>> {
        self.scopes.borrow().iter().rev()
            .filter_map(|scope| scope.generics.get(ident))
            .cloned()
            .next()
    }
    fn generic_names(&self) -> Vec<&'i str> {
        self.scopes.borrow().iter().rev()
            .flat_map(|scope| scope.generics.keys().copied())
            .collect()
    }
    fn generics(&self) -> Vec<Generic<'i>> {
        self.scopes.borrow().iter().rev()
            .flat_map(|scope| scope.generics.values())
            .cloned()
            .collect()
    }
    #[must_use]
    pub fn push_scope(&self, typ: ScopeType<'i>) -> ScopeGuard<'i> {
        self.scopes.borrow_mut().push(Scope { idents: IndexMap::new(), generics: IndexMap::new(), typ });
        ScopeGuard {
            scopes: Rc::downgrade(&self.scopes),
        }
    }

    fn next_token(&mut self) -> Result<Token<'i>, InternalError> {
        self.consume_comments();
        Ok(self.lexer.next()?)
    }
    fn peek_token(&mut self, index: usize) -> Result<Token<'i>, InternalError> {
        let mut non_comments = 0;
        for i in 0.. {
            match self.lexer.peek(i)? {
                Token::BlockComment(_) | Token::LineComment(_) => (),
                _ => {
                    non_comments += 1;
                    if non_comments - 1 == index {
                        return Ok(self.lexer.peek(i)?);
                    }
                }
            }
        }
        unreachable!()
    }

    pub(in crate::parser) fn parse<T: Parse<'i>>(&mut self, depth: Depth) -> Result<T, InternalError> {
        T::parse(self, depth)
    }
    pub(in crate::parser) fn parse_scoped<T: Parse<'i>>(&mut self, depth: Depth) -> Result<T, InternalError> {
        T::parse_scoped(self, depth)
    }

    fn similar_ident(&self, ident: &str) -> Option<String> {
        crate::util::similar_name(ident, self.scopes.borrow().iter().flat_map(|scope| scope.idents.keys())).map(|s| s.to_string())
    }

    fn diagnostic_unknown_identifier(&mut self, ident: TokenIdent<'i>, f: impl for<'d> FnOnce(DiagnosticBuilder<'d, ErrorCode>) -> DiagnosticBuilder<'d, ErrorCode>) -> Binding<'i> {
        let mut d = self.diagnostics.error(ErrorCode::UnknownIdentifier)
            .with_error_label(ident.diagnostics_span(), format!("variable `{}` doesn't exist", ident.ident));
        d = f(d);
        if let Some(similar) = self.similar_ident(ident.ident) {
            d = d.with_info_label(ident.diagnostics_span(), format!("did you mean `{}`", similar));
        }
        d.emit();
        // introduce rogue variable
        self.add_rogue_binding(ident, Some(TokenMut { span: ident.span }))
    }

    fn diagnostic_expected(&self, code: ErrorCode, span: Span, expected: &[Expected]) {
        let expected: Vec<_> = expected.iter()
            .map(|&expected| {
                match expected {
                    Expected::Type => "a type",
                    Expected::Token(token) => token.as_str(),
                }
            }).collect();
        let joined = expected.join(", ");
        let mut d = self.diagnostics.error(code);
        if expected.len() == 1 {
            d = d.with_error_label(span, format!("expected {}", joined));
        } else {
            d = d.with_error_label(span, format!("expected one of {}", joined));
        }
        d.emit()
    }

    fn add_free_function_to_meta_info(&mut self, fun: &'i ExprFunctionDefinition<'i>) {
        if let Some(self_arg) = &fun.sig.self_arg {
            self.diagnostics.error(ErrorCode::SelfBinding)
                .with_error_label(self_arg.diagnostics_span(), "self-argument not allowed here")
                .with_info_label(self_arg.diagnostics_span(), "self-argument is only allowed in methods")
                .emit();
        }

        let name = fun.sig.name.map(|name| Cow::Borrowed(name.ident));
        self.meta_info.add_function(self.diagnostics, name, fun);
    }
    fn add_impl_block_functions_to_meta_info(&mut self, impl_block: &'i ExprImplBlock<'i>) {
        for fun in &impl_block.functions {
            match fun.sig.name {
                Some(name) => {
                    let path = format!("{}::{}", impl_block.name.ident, name.ident);
                    self.meta_info.add_function(self.diagnostics, Some(Cow::Owned(path)), fun);
                }
                None => self.diagnostics.error(ErrorCode::MissingFunctionName)
                    .with_error_label(fun.sig.diagnostics_span(), "functions in impl-blocks must have names")
                    .emit(),
            }
        }
    }
    fn add_struct_to_meta_info(&mut self, struct_def: &'i ExprStructDefinition<'i>) {
        self.meta_info.add_struct(self.diagnostics, struct_def);
    }
    fn add_enum_to_meta_info(&mut self, enum_def: &'i ExprEnumDefinition<'i>) {
        self.meta_info.add_enum(self.diagnostics, enum_def);
    }

    fn consume_comments(&mut self) {
        #[allow(clippy::while_let_loop)]
        loop {
            match self.lexer.peek(0) {
                Ok(Token::LineComment(TokenLineComment { .. }))
                | Ok(Token::BlockComment(TokenBlockComment { .. })) => {
                    drop(self.lexer.next().unwrap());
                },
                _ => break,
            }
        }
    }
}
