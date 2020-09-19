use std::fmt;
use std::borrow::Cow;
use std::collections::HashMap;

use typed_arena::Arena;

use crate::lexer::{Tokens, Token, TokenType};
use crate::diagnostics::{Span, Diagnostics, ErrorCode, DiagnosticBuilder};
use crate::scope::BindingId;

mod expr;

pub use expr::{Binding, Expr, ExprType};

#[derive(Debug)]
pub enum Error {
    /// Parsing encountered an unrecoverable error and a diagnostic was emitted. Abort.
    Abort,
}
#[derive(Debug)]
enum InternalError {
    Error(Error),
    /// This function doesn't handle the tokens in the token stream. Everything was backtracked,
    /// the next function should be checked.
    Backtrack(Cow<'static, [Expected]>),
}
impl From<Error> for InternalError {
    fn from(e: Error) -> Self {
        InternalError::Error(e)
    }
}
#[derive(Debug, Clone, Copy)]
enum Expected {
    DqString,
    Ident,
    Expression,
    Let,
    /// =
    Assign,
    /// Int / Float
    Immediate,
    /// + - * /
    MathOp,
    OpenParen,
    CloseParen,
    Argument,
    Comma,
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

pub struct Parser<'a, 'i, 'r> {
    /// arena to allocate expressions into
    arena: &'a Arena<Expr<'a, 'i>>,
    /// tokens to be consumed
    tokens: Tokens<'i>,
    diagnostics: &'i Diagnostics<'i>,
    /// mapping of the identifiers of the root scope to their respective BindingId
    root_scope_binding_ids: &'r HashMap<&'static str, BindingId>,
    /// finished bindings that aren't live anymore
    bindings: Vec<Binding<'i>>,
    /// stack of scopes with bindings that are still live
    scopes: Vec<Scope<'i>>,
}

struct Scope<'i> {
    idents: HashMap<&'i str, Binding<'i>>
}

/// used for the binding parser when calling the assign parser
enum CreateBinding {
    /// mutable
    Yes(bool),
    No,
}

impl<'a, 'i, 'r> Parser<'a, 'i, 'r> {
    pub fn new(arena: &'a Arena<Expr<'a, 'i>>, tokens: Tokens<'i>, diagnostics: &'i Diagnostics<'i>, root_scope_binding_ids: &'r HashMap<&'static str, BindingId>) -> Self {
        let mut parser = Parser {
            arena,
            tokens,
            diagnostics,
            root_scope_binding_ids,
            bindings: Vec::new(),
            scopes: vec![Scope { idents: HashMap::new() }],
        };
        // make root scope bindings known to parser
        for (&ident, &binding_id) in parser.root_scope_binding_ids {
            parser.scopes.last_mut().unwrap().idents.insert(ident, Binding {
                id: binding_id,
                ident,
                mutable: false,
                span: Span::external(),
                rogue: false,
            });
        }
        parser
    }

    fn add_binding(&mut self, ident: &'i str, mutable: bool, span: Span) -> Binding<'i> {
        self.add_binding_internal(ident, mutable, span, false)
    }
    fn add_rogue_binding(&mut self, ident: &'i str, mutable: bool, span: Span) -> Binding<'i> {
        self.add_binding_internal(ident, mutable, span, true)
    }
    fn add_binding_internal(&mut self, ident: &'i str, mutable: bool, span: Span, rogue: bool) -> Binding<'i> {
        let id = BindingId::new();
        let binding = Binding { id, ident, mutable, span, rogue };
        self.scopes.last_mut().unwrap().idents.insert(ident, binding.clone());
        binding
    }
    fn get_binding(&mut self, ident: &'i str) -> Option<Binding<'i>> {
        self.scopes.iter().rev()
            .filter_map(|scope| scope.idents.get(ident))
            .cloned()
            .next()
    }
    fn push_scope(&mut self) {
        self.scopes.push(Scope { idents: HashMap::new() });
    }
    fn pop_scope(&mut self) {
        let scope = self.scopes.pop().unwrap();
        for (_, binding) in scope.idents.into_iter() {
            self.bindings.push(binding);
        }
    }

    pub fn parse(mut self) -> Result<Ast<'a, 'i>, Error> {
        trace!("parse");
        enum Last {
            Terminated,
            Unterminated(Span),
        }

        // file scope
        self.push_scope();
        let mut exprs = Vec::new();
        let mut last = Last::Terminated;
        let mut last_span = self.tokens.peek(0).unwrap().span;
        while !self.tokens.is_empty() && self.tokens.peek(0).unwrap().typ != TokenType::Eof {
            let span = self.tokens.peek(0).unwrap().span;
            let expr = self.parse_expr_or_stmt(last_span)?;

            // handle missing semicolon
            match last {
                Last::Terminated => (),
                Last::Unterminated(span) => self.diagnostics.error(ErrorCode::MissingSemicolon)
                    .with_info_label(Span::new(span.file, span.end, span.end), "try adding a semicolon here")
                    .emit(),
            }
            match expr.typ {
                ExprType::Statement(_) => last = Last::Terminated,
                _ => last = Last::Unterminated(expr.span),
            }

            exprs.push(expr);
            last_span = span;
        }
        // file scope
        self.pop_scope();
        Ok(Ast {
            exprs,
            bindings: self.bindings,
        })
    }

    fn parse_expr_or_stmt(&mut self, last_span: Span) -> Result<&'a Expr<'a, 'i>, Error> {
        trace!("parse_expr_or_stmt");
        let expr = self.parse_expr(last_span)?;
        match self.tokens.peek(0) {
            Some(Token { span, typ: TokenType::Semicolon }) => {
                drop(self.tokens.next());
                Ok(self.arena.alloc(Expr::new(Span::new(span.file, expr.span.start, span.end), ExprType::Statement(expr))))
            }
            _ => Ok(expr)
        }
    }

    fn parse_expr(&mut self, last_span: Span) -> Result<&'a Expr<'a, 'i>, Error> {
        trace!("parse_expr");
        match self.try_parse_expr() {
            Ok(expr) => Ok(expr),
            Err(InternalError::Backtrack(expected)) => {
                self.diagnostic_expected(ErrorCode::InvalidExpression, Span::new(last_span.file, last_span.end, last_span.end), &expected);
                Err(Error::Abort)
            },
            Err(InternalError::Error(e)) => Err(e),
        }
    }

    fn try_parse_expr(&mut self) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("try_parse_expr");
        let mut expected = Vec::new();
        let fns = &[
            Self::try_parse_string,
            Self::try_parse_bind,
            |this: &mut Parser<'a, 'i, 'r>| Self::try_parse_assign(this, CreateBinding::No),
            Self::try_parse_math,
            Self::try_parse_immediate,
            Self::try_parse_fn_call,
            Self::try_parse_variable,
        ];
        for f in fns {
            match f(self) {
                Ok(expr) => return Ok(expr),
                Err(InternalError::Backtrack(expect)) => expected.extend(expect.into_iter().copied()),
                e @ Err(InternalError::Error(_)) => return e,
            }
        }
        Err(InternalError::Backtrack(expected.into()))
    }

    fn try_parse_string(&mut self) -> Result<&'a Expr<'a, 'i>, InternalError> {
        match self.tokens.peek(0) {
            Some(Token { span, typ: TokenType::DqString(s) }) => {
                drop(self.tokens.next());
                Ok(self.arena.alloc(Expr::new(span, ExprType::String(s))))
            }
            _ => Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::DqString]))),
        }
    }

    fn try_parse_variable(&mut self) -> Result<&'a Expr<'a, 'i>, InternalError> {
        match self.tokens.peek(0) {
            Some(Token { span, typ: TokenType::Ident(ident) }) => {
                drop(self.tokens.next());
                let binding = match self.get_binding(ident) {
                    Some(binding) => binding,
                    None => self.diagnostic_unknown_identifier(span, ident, |d| d),
                };
                Ok(self.arena.alloc(Expr::new(span, ExprType::Variable((binding, span)))))
            },
            _ => Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Ident]))),
        }
    }

    fn try_parse_bind(&mut self) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("try_parse_bind");
        let mark = self.tokens.mark();
        let let_span = match self.tokens.next() {
            Some(Token { span, typ: TokenType::Let }) => span,
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Let]))),
        };
        let is_mut = match self.tokens.peek(0) {
            Some(Token { typ: TokenType::Mut, .. }) => {
                drop(self.tokens.next());
                true
            }
            _ => false
        };
        let res = match self.try_parse_assign(CreateBinding::Yes(is_mut)) {
            Ok(Expr { span, typ: ExprType::Assign((binding, _ident_span), expr) }) => {
                let span = Span::new(span.file, let_span.start, span.end);
                &*self.arena.alloc(Expr::new(span, ExprType::Bind(binding.clone(), expr)))
            }
            Ok(_) => unreachable!("try_parse_assign should only return ExprType::Assign"),
            Err(InternalError::Backtrack(expected)) => {
                match self.consume_until_next_end_token() {
                    Some(span) => self.diagnostic_expected(ErrorCode::InvalidLetBinding, Span::new(let_span.file, let_span.start, span.end), &expected),
                    None => self.diagnostic_expected(ErrorCode::IncompleteLetBinding, let_span, &expected),
                }
                // recover with new rogue binding
                let rogue_binding = self.add_rogue_binding("rogue_binding", is_mut, let_span);
                self.arena.alloc(Expr::new(let_span, ExprType::Bind(rogue_binding, self.arena.alloc(Expr::new(let_span, ExprType::Unit)))))
            },
            e @ Err(InternalError::Error(_)) => return e,
        };
        mark.apply();
        Ok(res)
    }

    fn try_parse_assign(&mut self, create_binding: CreateBinding) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("try_parse_assign");
        let mark = self.tokens.mark();
        let (ident, ident_span) = match self.tokens.next() {
            Some(Token { span, typ: TokenType::Ident(ident)}) => (ident, span),
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Ident]))),
        };
        // TODO: Type
        match self.tokens.next() {
            Some(Token { span: _, typ: TokenType::Assign }) => (),
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Assign]))),
        };
        let expr = self.try_parse_expr()?;
        mark.apply();

        let binding = match create_binding {
            CreateBinding::Yes(mutable) => self.add_binding(ident, mutable, ident_span),
            CreateBinding::No => {
                let binding = match self.get_binding(ident) {
                    Some(binding) => binding,
                    None => self.diagnostic_unknown_identifier(ident_span, ident, |d| d.with_info_label(ident_span, format!("use `let {} = ...` to create a new binding", ident))),
                };

                // check mutability
                if !binding.mutable {
                    self.diagnostics.error(ErrorCode::ImmutableAssign)
                        .with_error_label(ident_span, format!("variable `{}` is assigned to even though it's not declared as mutable", ident))
                        .with_info_label(binding.span, format!("`{}` previously defined here", binding.ident))
                        .with_info_label(binding.span, format!("help: try using `let mut {} = ...` here", binding.ident))
                        .emit();
                }
                binding
            }
        };

        Ok(self.arena.alloc(Expr::new(Span::new(expr.span.file, ident_span.start, expr.span.end), ExprType::Assign((binding, ident_span), expr))))
    }

    fn try_parse_immediate(&mut self) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("try_parse_immediate");
        match self.tokens.peek(0) {
            Some(Token { span, typ: TokenType::Integer(i, _radix) }) => {
                drop(self.tokens.next());
                Ok(self.arena.alloc(Expr::new(span, ExprType::Integer(i))))
            }
            Some(Token { span, typ: TokenType::Float(f, _radix) }) => {
                drop(self.tokens.next());
                Ok(self.arena.alloc(Expr::new(span, ExprType::Float(f))))
            }
            _ => Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Immediate]))),
        }
    }

    fn try_parse_math(&mut self) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("try_parse_math");
        match self.tokens.peek(0) {
            Some(Token { span, typ: TokenType::Plus })
            | Some(Token { span, typ: TokenType::Minus })
            | Some(Token { span, typ: TokenType::Star })
            | Some(Token { span, typ: TokenType::Slash }) => {
                // consume the operator token
                let op = self.tokens.next().unwrap();
                let a = self.parse_expr(span)?;
                let b = self.parse_expr(span)?;
                match op.typ {
                    TokenType::Plus => Ok(self.arena.alloc(Expr::new(span, ExprType::Add(a, b)))),
                    TokenType::Minus => Ok(self.arena.alloc(Expr::new(span, ExprType::Sub(a, b)))),
                    TokenType::Star => Ok(self.arena.alloc(Expr::new(span, ExprType::Mul(a, b)))),
                    TokenType::Slash => Ok(self.arena.alloc(Expr::new(span, ExprType::Div(a, b)))),
                    _ => unreachable!(),
                }
            },
            _ => Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::MathOp]))),
        }
    }

    fn try_parse_fn_call(&mut self) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("try_parse_fn_call");
        let mark = self.tokens.mark();

        let (ident, ident_span) = match self.tokens.next() {
            Some(Token { span, typ: TokenType::Ident(ident) }) => (ident, span),
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Ident]))),
        };
        match self.tokens.next() {
            Some(Token { typ: TokenType::OpenParen, .. }) => (),
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::OpenParen]))),
        }
        let binding = match self.get_binding(ident) {
            Some(binding) => binding,
            None => self.diagnostic_unknown_identifier(ident_span, ident, |d| d),
        };
        let mut args = Vec::new();
        let mut last_span = ident_span;
        let res = loop {
            match self.tokens.peek(0) {
                Some(Token { span, typ: TokenType::CloseParen }) => {
                    drop(self.tokens.next());
                    break self.arena.alloc(Expr::new(Span::new(span.file, ident_span.start, span.end), ExprType::FunctionCall((binding, ident_span), args)));
                },
                None => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Argument, Expected::CloseParen]))),
                _ => (),
            }
            let arg = self.parse_expr(last_span)?;
            args.push(arg);
            match self.tokens.next() {
                Some(Token { typ: TokenType::Comma, span }) => last_span = span,
                Some(Token { span, typ: TokenType::CloseParen }) => {
                    break self.arena.alloc(Expr::new(Span::new(span.file, ident_span.start, span.end), ExprType::FunctionCall((binding, ident_span), args)));
                },
                _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Comma, Expected::CloseParen]))),
            }
        };
        mark.apply();
        Ok(&*res)
    }

    fn similar_ident(&self, ident: &'i str) -> Option<&'i str> {
        self.scopes.iter()
            .flat_map(|scope| scope.idents.keys())
            .map(|s| (strsim::levenshtein(ident, s), s))
            // .filter(|&(dist, _)| dist <= 3)
            .min_by_key(|&(dist, _)| dist)
            .map(|(_, &s)| s)
    }

    fn diagnostic_unknown_identifier(&mut self, span: Span, ident: &'i str, f: impl for<'d> FnOnce(DiagnosticBuilder<'i, 'd>) -> DiagnosticBuilder<'i, 'd>) -> Binding<'i> {
        let mut d = self.diagnostics.error(ErrorCode::UnknownIdentifier)
            .with_error_label(span, format!("variable `{}` doesn't exist", ident));
        d = f(d);
        if let Some(similar) = self.similar_ident(ident) {
            d = d.with_info_label(span, format!("did you mean the similarly named variable `{}`", similar));
        }
        d.emit();
        // introduce rogue variable
        self.add_rogue_binding(ident, true, span)
    }

    fn diagnostic_expected(&self, code: ErrorCode, span: Span, expected: &[Expected]) {
        let expected: Vec<_> = expected.into_iter()
            .map(|&expected| {
                use Expected::*;
                match expected {
                    DqString => "double-quoted string",
                    Ident => "identifier",
                    Expression => "expression",
                    Let => "`let`",
                    Assign => "`=`",
                    Immediate => "integer or float",
                    MathOp => "`+`, `-`, `*`, `/`",
                    OpenParen => "`(`",
                    CloseParen => "`)`",
                    Argument => "function argument",
                    Comma => "`,`",
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

    /// Consume until the next end token (`;`, `}`) in the hope that that allows us to recover.
    fn consume_until_next_end_token(&mut self) -> Option<Span> {
        let start_span = self.tokens.peek(0)?.span;
        let mut last_end = start_span.end;
        loop {
            match self.tokens.next() {
                Some(Token { span, typ: TokenType::Semicolon }) => return Some(Span::new(start_span.file, start_span.start, span.end)),
                Some(Token { span, .. }) => last_end = span.end,
                None => return Some(Span::new(start_span.file, start_span.start, last_end)),
            }
        }
    }
}
