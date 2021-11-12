use std::fmt;
use std::borrow::Cow;
use std::collections::{HashMap, BTreeMap, btree_map::Entry, HashSet};

use typed_arena::Arena;
use diagnostic::{Span, Diagnostics, DiagnosticBuilder, FileId};

use crate::lexer::{Lexer, Token, TokenType, TokenMut, TokenIdent, TokenLineComment, TokenBlockComment};
use crate::error_codes::ErrorCode;

mod expr;
mod precedence;
mod parse;
mod scope;
mod first_pass;

pub use expr::*;
pub use parse::{Parse, Spanned, Separated};
pub use scope::BindingId;
use crate::common::{MetaInfo, Depth, Function};
use indexmap::map::IndexMap;
use itertools::Itertools;
use crate::parser::scope::Scope;
use std::cell::RefCell;
use std::rc::Rc;
use std::path::PathBuf;
use indexmap::set::IndexSet;

#[derive(Debug, Clone)]
pub enum Error {
    /// Parsing encountered an unrecoverable error and a diagnostic was emitted. Abort.
    Abort,
}
#[derive(Debug, Clone)]
pub enum InternalError {
    /// We can't recover, return error to caller
    Error(Error),
    /// This function doesn't handle the tokens in the token stream. Everything was rolled back,
    /// the next function should be checked.
    Backtrack(Span, Cow<'static, [Expected]>),
}
impl From<Error> for InternalError {
    fn from(e: Error) -> Self {
        InternalError::Error(e)
    }
}
impl From<crate::lexer::Error> for InternalError {
    fn from(_: crate::lexer::Error) -> Self {
        InternalError::Error(Error::Abort)
    }
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
pub struct Ast<'a, 'i> {
    pub exprs: Vec<&'a Expr<'a, 'i>>,
    pub bindings: Vec<Binding<'i>>,
}

impl<'a, 'i> fmt::Display for Ast<'a, 'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for expr in &self.exprs {
            writeln!(f, "{}", expr)?;
        }
        Ok(())
    }
}

pub struct Parser<'a, 'b, 'i> {
    /// directory to search in when including files
    include_directory: PathBuf,
    /// arena to allocate expressions into
    arena: &'a Arena<Expr<'a, 'i>>,
    /// tokens to be consumed
    lexer: Lexer<'i>,
    diagnostics: &'i Diagnostics,
    /// finished bindings that aren't live anymore
    bindings: Vec<Binding<'i>>,
    /// pre-info to add first-pass definitions to
    meta_info: &'b mut MetaInfo<'a, 'i>,
    /// already parsed expressions in the first-pass, to be consumed by the second pass
    pre_parsed: HashMap<(FileId, usize), &'a Expr<'a, 'i>>,
    /// stack of scopes with bindings that are still live
    scopes: Rc<RefCell<Vec<Scope<'i>>>>,
    memoization: IndexMap<(FileId, usize), (&'a Expr<'a, 'i>, ParseUntil)>,
    binding_memoization: BTreeMap<Span, Binding<'i>>,
    generic_memoization: HashSet<Span>,
    rebo_function_names: IndexSet<(String, TokenIdent<'i>)>,
}

pub struct ScopeGuard<'i> {
    scopes: Rc<RefCell<Vec<Scope<'i>>>>,
}
impl<'i> Drop for ScopeGuard<'i> {
    fn drop(&mut self) {
        self.scopes.borrow_mut().pop().unwrap();
    }
}

/// All expression parsing function consume whitespace and comments before tokens, but not after.
impl<'a, 'b, 'i> Parser<'a, 'b, 'i> {
    pub fn new(include_directory: PathBuf, arena: &'a Arena<Expr<'a, 'i>>, lexer: Lexer<'i>, diagnostics: &'i Diagnostics, meta_info: &'b mut MetaInfo<'a, 'i>) -> Self {
        Parser {
            include_directory,
            arena,
            lexer,
            diagnostics,
            bindings: Vec::new(),
            meta_info,
            pre_parsed: HashMap::new(),
            scopes: Rc::new(RefCell::new(vec![Scope { idents: IndexMap::new(), generics: IndexMap::new() }])),
            memoization: IndexMap::new(),
            binding_memoization: BTreeMap::new(),
            generic_memoization: HashSet::new(),
            rebo_function_names: IndexSet::new(),
        }
    }

    pub fn parse_ast(mut self) -> Result<Ast<'a, 'i>, Error> {
        trace!("parse_ast");
        let body: BlockBody = match self.parse_file_content() {
            Ok(body) => body,
            Err(InternalError::Backtrack(span, expected)) => {
                self.diagnostic_expected(ErrorCode::InvalidExpression, span, &expected);
                return Err(Error::Abort)
            },
            Err(InternalError::Error(e)) => return Err(e),
        };
        // make sure everything parsed during first-pass was consumed and used by the second pass
        assert!(self.pre_parsed.is_empty(), "not everything from first-pass was consumed: {:?}", self.pre_parsed);
        assert!(matches!(self.peek_token(0), Ok(Token::Eof(_))), "not all tokens were consumed: {}", self.lexer.iter().map(|t| format!("    {:?}", t)).join("\n"));
        Ok(Ast {
            exprs: body.exprs,
            bindings: self.bindings,
        })
    }
    fn parse_file_content(&mut self) -> Result<BlockBody<'a, 'i>, InternalError> {
        self.first_pass();
        self.second_pass();
        // add statics to global scope
        self.add_statics();
        // file scope
        Ok(self.parse(Depth::start())?)
    }

    fn add_statics(&mut self) {
        for binding in self.meta_info.static_bindings.clone() {
            // the memoized binding will be used
            self.add_binding(binding.ident, binding.mutable);
        }
        // add functions to scope
        for (name, function) in self.meta_info.functions.clone() {
            let ident = match function {
                Function::Rust(_) => {
                    let ext = &self.meta_info.external_functions[name.as_ref()];
                    TokenIdent {
                        span: Span::new(FileId::synthetic(ext.file_name), 0, ext.code.len()),
                        ident: match name {
                            Cow::Borrowed(s) => s,
                            Cow::Owned(_) => unreachable!("external function name isn't &'static str: {}", name),
                        }
                    }
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
            let binding = self.add_binding_raw(name.clone().into_owned(), ident);
            self.meta_info.function_bindings.insert(binding, name.into_owned());
        }
        for (name, ident) in self.rebo_function_names.clone() {
            let binding = self.add_binding_raw(name.clone(), ident);
            self.meta_info.function_bindings.insert(binding, name);
        }
    }

    fn add_binding(&mut self, ident: TokenIdent<'i>, mutable: Option<TokenMut>) -> Binding<'i> {
        self.add_binding_internal(ident, mutable, false)
    }
    fn add_rogue_binding(&mut self, ident: TokenIdent<'i>, mutable: Option<TokenMut>) -> Binding<'i> {
        self.add_binding_internal(ident, mutable, true)
    }
    fn add_binding_internal(&mut self, ident: TokenIdent<'i>, mutable: Option<TokenMut>, rogue: bool) -> Binding<'i> {
        let binding = match self.binding_memoization.entry(ident.span()) {
            Entry::Vacant(vacant) => {
                let id = BindingId::unique();
                let binding = Binding { id, mutable, ident, rogue };
                vacant.insert(binding);
                binding
            }
            Entry::Occupied(occupied) => *occupied.get(),
        };
        self.scopes.borrow_mut().last_mut().unwrap().idents.insert(binding.ident.ident.to_string(), binding);
        binding
    }
    /// Add a raw binding that not come from the source-code itself. Used to add functions as bindings.
    fn add_binding_raw(&mut self, name: String, ident: TokenIdent<'i>) -> Binding<'i> {
        let id = BindingId::unique();
        let binding = Binding { id, mutable: None, ident, rogue: false };
        self.scopes.borrow_mut().last_mut().unwrap().idents.insert(name, binding);
        binding
    }
    fn get_binding(&self, ident: &str) -> Option<Binding<'i>> {
        self.scopes.borrow().iter().rev()
            .filter_map(|scope| scope.idents.get(ident))
            .cloned()
            .next()
    }
    fn add_generic(&mut self, generic: Generic<'i>) {
        if self.get_generic(generic.def_ident.ident).is_some() && self.generic_memoization.contains(&generic.def_ident.span) {
            return;
        }
        self.generic_memoization.insert(generic.def_ident.span);
        match self.get_generic(generic.def_ident.ident) {
            Some(prev) => self.diagnostics.error(ErrorCode::DuplicateGeneric)
                .with_error_label(generic.def_ident.span, "this generic has already been defined")
                .with_info_label(prev.def_ident.span, "previously defined here")
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
    fn push_scope(&self) -> ScopeGuard<'i> {
        self.scopes.borrow_mut().push(Scope { idents: IndexMap::new(), generics: IndexMap::new() });
        ScopeGuard {
            scopes: Rc::clone(&self.scopes)
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

    pub(in crate::parser) fn parse<T: Parse<'a, 'i>>(&mut self, depth: Depth) -> Result<T, InternalError> {
        T::parse(self, depth)
    }
    pub(in crate::parser) fn parse_scoped<T: Parse<'a, 'i>>(&mut self, depth: Depth) -> Result<T, InternalError> {
        T::parse_scoped(self, depth)
    }

    fn similar_ident(&self, ident: &str) -> Option<String> {
        crate::util::similar_name(ident, self.scopes.borrow().iter().flat_map(|scope| scope.idents.keys())).map(|s| s.to_string())
    }

    fn diagnostic_unknown_identifier(&mut self, ident: TokenIdent<'i>, f: impl for<'d> FnOnce(DiagnosticBuilder<'d, ErrorCode>) -> DiagnosticBuilder<'d, ErrorCode>) -> Binding<'i> {
        let mut d = self.diagnostics.error(ErrorCode::UnknownIdentifier)
            .with_error_label(ident.span, format!("variable `{}` doesn't exist", ident.ident));
        d = f(d);
        if let Some(similar) = self.similar_ident(ident.ident) {
            d = d.with_info_label(ident.span, format!("did you mean `{}`", similar));
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

    fn consume_comments(&mut self) {
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
