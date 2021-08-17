use std::fmt;
use std::borrow::Cow;
use std::collections::{HashMap, BTreeMap, btree_map::Entry};

use typed_arena::Arena;
use diagnostic::{Span, Diagnostics, DiagnosticBuilder, FileId};

use crate::lexer::{Lexer, Token, TokenType, TokenMut, TokenIdent, TokenOpenCurly, TokenCloseCurly, TokenCloseParen, TokenOpenParen, TokenLineComment, TokenBlockComment};
use crate::error_codes::ErrorCode;
use crate::scope::BindingId;

mod expr;
mod precedence;
mod parse;

pub use expr::*;
pub use parse::{Parse, Spanned, Separated};
use crate::common::{PreInfo, SpecificType, FunctionType, Type, Value, FunctionImpl, Depth, StructType};
use indexmap::map::IndexMap;
use itertools::Itertools;

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
    /// arena to allocate expressions into
    arena: &'a Arena<Expr<'a, 'i>>,
    /// tokens to be consumed
    lexer: Lexer<'i>,
    diagnostics: &'i Diagnostics,
    /// finished bindings that aren't live anymore
    bindings: Vec<Binding<'i>>,
    /// pre-info to add first-pass definitions to
    pre_info: &'b mut PreInfo<'a, 'i>,
    /// already parsed expressions in the first-pass, to be consumed by the second pass
    pre_parsed: HashMap<(FileId, usize), &'a Expr<'a, 'i>>,
    /// stack of scopes with bindings that are still live
    scopes: Vec<Scope<'i>>,
    memoization: IndexMap<(FileId, usize), (&'a Expr<'a, 'i>, ParseUntil)>,
    binding_memoization: BTreeMap<Span, Binding<'i>>,
}

struct Scope<'i> {
    idents: IndexMap<Cow<'i, str>, Binding<'i>>
}

/// All expression parsing function consume whitespace and comments before tokens, but not after.
impl<'a, 'b, 'i> Parser<'a, 'b, 'i> {
    pub fn new(arena: &'a Arena<Expr<'a, 'i>>, lexer: Lexer<'i>, diagnostics: &'i Diagnostics, pre_info: &'b mut PreInfo<'a, 'i>) -> Self {
        let mut parser = Parser {
            arena,
            lexer,
            diagnostics,
            bindings: Vec::new(),
            pre_info,
            pre_parsed: HashMap::new(),
            scopes: vec![Scope { idents: IndexMap::new() }],
            memoization: IndexMap::new(),
            binding_memoization: BTreeMap::new(),
        };
        parser.first_pass();
        // make existing bindings known to parser
        for &binding in parser.pre_info.bindings.keys() {
            let name = parser.pre_info.rebo_associated_functions.get(&binding.id)
                .map(|(name, fun)| Cow::Owned(format!("{}::{}", name.ident, fun.binding.ident.ident)))
                .unwrap_or_else(|| Cow::Borrowed(binding.ident.ident));
            if let Some(old) = parser.scopes.last_mut().unwrap().idents.insert(name, binding) {
                let mut spans = [old.span(), binding.span()];
                spans.sort();
                parser.diagnostics.error(ErrorCode::DuplicateGlobal)
                    .with_info_label(spans[0], "first defined here")
                    .with_error_label(spans[1], "also defined here")
                    .emit();
            }
        }
        parser
    }

    /// Parse function, struct and enum definitions
    fn first_pass(&mut self) {
        trace!("first_pass");
        // create rogue scopes
        let old_scopes = ::std::mem::replace(&mut self.scopes, vec![Scope { idents: IndexMap::new() }]);
        let mark = self.lexer.mark();
        let functions: &[for<'x> fn(&'x mut Parser<'a, 'b, 'i>, _) -> Result<_, InternalError>] = &[
            |parser, depth: Depth| Ok(Expr::FunctionDefinition(ExprFunctionDefinition::parse_reset(parser, depth.next())?)),
            |parser, depth: Depth| Ok(Expr::StructDefinition(ExprStructDefinition::parse_reset(parser, depth.next())?)),
            |parser, depth: Depth| Ok(Expr::ImplBlock(ExprImplBlock::parse_reset(parser, depth.next())?)),
        ];
        while self.peek_token(0).is_ok() && !matches!(self.peek_token(0).unwrap(), Token::Eof(_)) {
            let depth = Depth::start();
            trace!("{} first_pass", depth);
            for function in functions {
                let expr = match function(&mut *self, depth.next()) {
                    Ok(expr) => &*self.arena.alloc(expr),
                    Err(_) => continue,
                };
                match expr {
                    Expr::FunctionDefinition(fun) => {
                        let typ = SpecificType::Function(Box::new(FunctionType {
                            args: fun.args.iter().map(|pattern| Type::Specific(SpecificType::from(&pattern.typ))).collect(),
                            ret: Type::Specific(fun.ret_type.as_ref().map(|(_, typ)| SpecificType::from(typ)).unwrap_or(SpecificType::Unit)),
                        }));
                        trace!("{} found {}", Depth::start(), fun);
                        self.pre_info.bindings.insert(fun.binding, typ);
                        self.pre_info.rebo_functions.insert(fun.binding.id, fun);
                        let arg_binding_ids = fun.args.iter().map(|ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }| binding.id).collect();
                        self.pre_info.root_scope.create(fun.binding.id, Value::Function(FunctionImpl::Rebo(fun.binding.id, arg_binding_ids)));
                    }
                    Expr::StructDefinition(struct_def) => {
                        let typ = StructType {
                            name: struct_def.name.ident.to_string(),
                            fields: struct_def.fields.iter()
                                .map(|(name, _, typ)| (name.ident.to_string(), SpecificType::from(typ)))
                                .collect(),
                        };
                        trace!("{} found {} ({:?}, {})", Depth::start(), struct_def, struct_def.span().file, struct_def.span().start);
                        if let Some((_old_typ, old_span)) = self.pre_info.structs.insert(struct_def.name.ident, (typ, struct_def.name.span())) {
                            let mut spans = [old_span, struct_def.name.span()];
                            spans.sort();
                            self.diagnostics.error(ErrorCode::DuplicateGlobal)
                                .with_info_label(spans[0], "first defined here")
                                .with_error_label(spans[1], "also defined here")
                                .emit();
                        }
                    }
                    Expr::ImplBlock(impl_block) => {
                        for fun in &impl_block.functions {
                            let typ = SpecificType::Function(Box::new(FunctionType {
                                args: fun.args.iter().map(|pattern| Type::Specific(SpecificType::from(&pattern.typ))).collect(),
                                ret: Type::Specific(fun.ret_type.as_ref().map(|(_, typ)| SpecificType::from(typ)).unwrap_or(SpecificType::Unit)),
                            }));
                            trace!("{} found {}::{}", Depth::start(), impl_block.name.ident, fun);
                            self.pre_info.bindings.insert(fun.binding, typ);
                            self.pre_info.rebo_associated_functions.insert(fun.binding.id, (&impl_block.name, fun));
                            let arg_binding_ids = fun.args.iter().map(|ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }| binding.id).collect();
                            self.pre_info.root_scope.create(fun.binding.id, Value::Function(FunctionImpl::Rebo(fun.binding.id, arg_binding_ids)));
                        }
                    }
                    _ => unreachable!("we just parsed you"),
                }
                self.pre_parsed.insert((expr.span().file, expr.span().start), expr);
                // consume tokens except last one as that's consumed after the for loop
                while self.peek_token(0).unwrap().span().end < expr.span().end {
                    drop(self.next_token());
                }
                break;
            }
            drop(self.next_token())
        }
        // reset token lookahead
        drop(mark);
        self.scopes = old_scopes;
    }

    pub fn parse_ast(mut self) -> Result<Ast<'a, 'i>, Error> {
        trace!("parse_ast");
        // file scope
        let body: BlockBody = match self.parse(Depth::start()) {
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


    fn add_binding(&mut self, ident: TokenIdent<'i>, mutable: Option<TokenMut>) -> Binding<'i> {
        self.add_binding_internal(ident, mutable, false)
    }
    fn add_rogue_binding(&mut self, ident: TokenIdent<'i>, mutable: Option<TokenMut>) -> Binding<'i> {
        self.add_binding_internal(ident, mutable, true)
    }
    fn add_binding_internal(&mut self, ident: TokenIdent<'i>, mutable: Option<TokenMut>, rogue: bool) -> Binding<'i> {
        let (name, binding) = match self.binding_memoization.entry(ident.span()) {
            Entry::Vacant(vacant) => {
                let id = BindingId::unique();
                let name = ident.ident;
                let binding = Binding { id, ident, mutable, rogue };
                vacant.insert(binding);
                (name, binding)
            }
            Entry::Occupied(occupied) => (occupied.get().ident.ident, *occupied.get())
        };
        self.scopes.last_mut().unwrap().idents.insert(Cow::Borrowed(name), binding);
        binding
    }
    fn get_binding(&mut self, ident: &str) -> Option<Binding<'i>> {
        self.scopes.iter().rev()
            .filter_map(|scope| scope.idents.get(ident))
            .cloned()
            .next()
    }
    fn push_scope(&mut self) -> &mut Scope<'i> {
        self.scopes.push(Scope { idents: IndexMap::new() });
        self.scopes.last_mut().unwrap()
    }
    fn pop_scope(&mut self) {
        let scope = self.scopes.pop().unwrap();
        for (_, binding) in scope.idents.into_iter() {
            self.bindings.push(binding);
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

    fn similar_ident(&self, ident: &'i str) -> Option<&str> {
        crate::util::similar_name(ident, self.scopes.iter().flat_map(|scope| scope.idents.keys()).map(|cow| cow.as_ref()))
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

    /// Consume until excluding the next instance of one of the passed tokens
    fn consume_until(&mut self, until: &[TokenType]) -> Consumed<'i> {
        let start_span = match self.peek_token(0) {
            Ok(token) => token.span(),
            Err(_) => return Consumed::InstantError,
        };
        let mut paren_depth = 0;
        let mut curly_depth = 0;
        let mut last_end = start_span.end;
        loop {
            match self.peek_token(0) {
                Ok(token) if until.contains(&token.typ()) && paren_depth == 0 && curly_depth == 0 => {
                    return Consumed::Found(Span::new(start_span.file, start_span.start, token.span().end), token)
                },
                Ok(Token::OpenParen(TokenOpenParen { span })) => {
                    last_end = span.end;
                    paren_depth += 1
                },
                Ok(Token::CloseParen(TokenCloseParen { span })) => {
                    last_end = span.end;
                    paren_depth -= 1
                },
                Ok(Token::OpenCurly(TokenOpenCurly { span })) => {
                    last_end = span.end;
                    curly_depth += 1
                },
                Ok(Token::CloseCurly(TokenCloseCurly { span })) => {
                    last_end = span.end;
                    curly_depth -= 1
                },
                Err(_) | Ok(Token::Eof(_)) => return Consumed::Eof(Span::new(start_span.file, start_span.start, last_end)),
                Ok(token) => last_end = token.span().end,
            }
            drop(self.next_token());
        }
    }
}


enum Consumed<'i> {
    InstantError,
    Found(Span, Token<'i>),
    Eof(Span),
}
