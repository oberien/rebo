use std::fmt;
use std::borrow::Cow;
use std::collections::HashMap;

use typed_arena::Arena;
use diagnostic::{Span, Diagnostics, DiagnosticBuilder, FileId};

use crate::lexer::{Tokens, Token, TokenType};
use crate::error_codes::ErrorCode;
use crate::scope::BindingId;

mod expr;
mod precedence;

pub use expr::{Binding, Expr, ExprType};
use crate::parser::precedence::{Math, BooleanExpr};
use crate::common::{PreTypeInfo, SpecificType, FunctionType, Type, Value, FunctionImpl};

#[derive(Debug)]
pub enum Error {
    /// Parsing encountered an unrecoverable error and a diagnostic was emitted. Abort.
    Abort,
}
#[derive(Debug)]
enum InternalError {
    /// We can't recover, return error to caller
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
    Let,
    Fn,
    Type,
    /// =
    Assign,
    /// ===
    Equals,
    /// Int / Float
    Immediate,
    /// + - * /
    MathOp,
    /// || &&
    BooleanExprOp,
    OpenParen,
    CloseParen,
    OpenCurly,
    Argument,
    Comma,
    Colon,
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
    tokens: Tokens<'i>,
    diagnostics: &'i Diagnostics,
    /// finished bindings that aren't live anymore
    bindings: Vec<Binding<'i>>,
    /// pre-info to add first-pass definitions to
    pre_info: &'b mut PreTypeInfo<'a, 'i>,
    /// already parsed expressions in the first-pass, to be consumed by the second pass
    pre_parsed: HashMap<(FileId, usize), &'a Expr<'a, 'i>>,
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

#[derive(Debug, PartialOrd, PartialEq)]
enum ParseUntil {
    None,
    Math,
    Compare,
    BooleanExpr,
    All,
}

/// All expression parsing function consume whitespace and comments before tokens, but not after.
impl<'a, 'b, 'i> Parser<'a, 'b, 'i> {
    pub fn new(arena: &'a Arena<Expr<'a, 'i>>, tokens: Tokens<'i>, diagnostics: &'i Diagnostics, pre_info: &'b mut PreTypeInfo<'a, 'i>) -> Self {
        let mut parser = Parser {
            arena,
            tokens,
            diagnostics,
            bindings: Vec::new(),
            pre_info,
            pre_parsed: HashMap::new(),
            scopes: vec![Scope { idents: HashMap::new() }],
        };
        parser.first_pass();
        // make existing bindings known to parser
        for &binding in parser.pre_info.bindings.keys() {
            if let Some(old) = parser.scopes.last_mut().unwrap().idents.insert(binding.ident, binding) {
                let mut spans = [old.span, binding.span];
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
        let mark = self.tokens.mark();
        while self.tokens.peek(0).is_some() {
            match self.try_parse_fn_def(0) {
                Ok(expr) => match expr {
                    Expr { span, typ: ExprType::FunctionDefinition(fn_binding, args, ret_type, body) } => {
                        let typ = SpecificType::Function(Box::new(FunctionType {
                            args: args.iter().map(|(_binding, typ)| Type::Specific(typ.clone())).collect(),
                            ret: Type::Specific(ret_type.clone()),
                        }));
                        self.pre_info.bindings.insert(*fn_binding, typ);
                        self.pre_info.rebo_functions.insert(fn_binding.id, body);
                        let arg_binding_ids = args.iter().map(|(binding, _typ)| binding.id).collect();
                        self.pre_info.root_scope.create(fn_binding.id, Value::Function(FunctionImpl::Rebo(fn_binding.id, arg_binding_ids)));
                        self.pre_parsed.insert((span.file, span.start), expr);
                    }
                    _ => unreachable!("try_parse_fn_def didn't return FunctionDefinition but {:?}", expr),
                }
                _ => drop(self.tokens.next()),
            }
        }
        // reset token lookahead
        drop(mark);
    }

    fn add_binding(&mut self, ident: &'i str, mutable: bool, span: Span) -> Binding<'i> {
        self.add_binding_internal(ident, mutable, span, false)
    }
    fn add_rogue_binding(&mut self, ident: &'i str, mutable: bool, span: Span) -> Binding<'i> {
        self.add_binding_internal(ident, mutable, span, true)
    }
    fn add_binding_internal(&mut self, ident: &'i str, mutable: bool, span: Span, rogue: bool) -> Binding<'i> {
        let id = BindingId::unique();
        let binding = Binding { id, ident, mutable, span, rogue };
        self.scopes.last_mut().unwrap().idents.insert(ident, binding);
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

    fn next_token(&mut self) -> Option<Token<'i>> {
        self.consume_comments();
        self.tokens.next()
    }
    fn peek_token(&mut self, index: usize) -> Option<Token<'i>> {
        let mut non_comments = 0;
        for i in 0.. {
            match self.tokens.peek(i)?.typ {
                TokenType::BlockComment(_) | TokenType::LineComment(_) => (),
                _ => {
                    non_comments += 1;
                    if non_comments - 1 == index {
                        return self.tokens.peek(i);
                    }
                }
            }
        }
        unreachable!()
    }

    pub fn parse(mut self) -> Result<Ast<'a, 'i>, Error> {
        trace!("parse");
        // file scope
        let body = self.parse_block_body(0);
        // make sure everything parsed during first-pass was consumed and used by the second pass
        assert!(self.pre_parsed.is_empty());
        Ok(Ast {
            exprs: body,
            bindings: self.bindings,
        })
    }

    fn parse_expr_or_stmt(&mut self, last_span: Span, depth: usize) -> Result<&'a Expr<'a, 'i>, Error> {
        trace!("{}parse_expr_or_stmt: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let expr = self.parse_expr(last_span, depth+1)?;
        match self.peek_token(0) {
            Some(Token { span, typ: TokenType::Semicolon }) => {
                drop(self.next_token());
                Ok(self.arena.alloc(Expr::new(Span::new(span.file, expr.span.start, span.end), ExprType::Statement(expr))))
            }
            _ => Ok(expr)
        }
    }

    fn parse_expr(&mut self, last_span: Span, depth: usize) -> Result<&'a Expr<'a, 'i>, Error> {
        trace!("{}parse_expr: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let span = self.peek_token(0).map(|t| t.span).unwrap_or_else(|| Span::new(last_span.file, last_span.end, last_span.end));
        match self.try_parse_until_including(ParseUntil::All, depth+1) {
            Ok(expr) => Ok(expr),
            Err(InternalError::Backtrack(expected)) => {
                self.diagnostic_expected(ErrorCode::InvalidExpression, Span::new(span.file, span.start, span.end), &expected);
                Err(Error::Abort)
            },
            Err(InternalError::Error(e)) => Err(e),
        }
    }
    fn try_parse_until_including(&mut self, until: ParseUntil, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_until_including {:?}: {}", "|".repeat(depth), until, self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        self.try_parse_until(until, PartialOrd::ge, depth)
    }
    fn try_parse_until_excluding(&mut self, until: ParseUntil, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_until_excluding {:?}: {}", "|".repeat(depth), until, self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        self.try_parse_until(until, PartialOrd::gt, depth)
    }

    fn try_parse_until(&mut self, until: ParseUntil, cmp: fn(&ParseUntil, &ParseUntil) -> bool, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        if let Some(Token { span, typ: _ }) = self.peek_token(0) {
            if let Some(expr) = self.pre_parsed.remove(&(span.file, span.start)) {
                // consume tokens already parsed in first-pass
                while let Some(Token { span, typ: _ }) = self.peek_token(0) {
                    if span.start < expr.span.end {
                        self.next_token();
                    } else {
                        break;
                    }
                }
                return Ok(expr);
            }
        }

        type ParseFn<'a, 'b, 'i> = fn(&mut Parser<'a, 'b, 'i>, usize) -> Result<&'a Expr<'a, 'i>, InternalError>;
        let mut fns: Vec<ParseFn<'a, 'b, 'i>> = Vec::new();
        // add in reverse order
        if cmp(&until, &ParseUntil::BooleanExpr) {
            fns.push(Self::try_parse_precedence::<BooleanExpr>);
        }
        if cmp(&until, &ParseUntil::Compare) {
            fns.push(Self::try_parse_compare);
        }
        if cmp(&until, &ParseUntil::Math) {
            fns.push(Self::try_parse_precedence::<Math>);
        }
        fns.extend(&[
            Self::try_parse_parens,
            Self::try_parse_block_expr,
            Self::try_parse_not,
            Self::try_parse_bind,
            |this: &mut Parser<'a, 'b, 'i>, depth: usize| Self::try_parse_assign(this, CreateBinding::No, depth),
            Self::try_parse_fn_call,
            Self::try_parse_string,
            Self::try_parse_immediate,
            Self::try_parse_variable,
        ]);

        let mut expected = Vec::new();
        for f in fns {
            match f(self, depth+1) {
                Ok(expr) => return Ok(expr),
                Err(InternalError::Backtrack(expect)) => expected.extend(expect.iter().copied()),
                e @ Err(InternalError::Error(_)) => return e,
            }
        }
        Err(InternalError::Backtrack(expected.into()))
    }

    fn try_parse_parens(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_parens: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let start_span = match self.peek_token(0) {
            Some(Token { span, typ: TokenType::OpenParen }) => span,
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::OpenParen]))),
        };
        drop(self.next_token());

        // try parse unit
        if let Some(Token { span, typ: TokenType::CloseParen }) = self.peek_token(0) {
            drop(self.next_token());
            return Ok(self.arena.alloc(Expr::new(Span::new(start_span.file, start_span.start, span.end), ExprType::Unit)));
        }

        // try parse parenthesized
        let expr = self.try_parse_until_including(ParseUntil::All, depth+1)?;
        let span = match self.next_token() {
            Some(Token { span, typ: TokenType::CloseParen }) => span,
            _ => {
                let span = Span::new(start_span.file, start_span.start, expr.span.end);
                self.diagnostics.error(ErrorCode::UnclosedParen)
                    .with_error_label(span, "unclosed parenthesis")
                    .with_info_label(Span::new(expr.span.file, expr.span.end, expr.span.end), "try inserting a `)` here")
                    .emit();
                span
            },
        };
        trace!("{} parens: ({})", "|".repeat(depth), expr);
        Ok(self.arena.alloc(Expr::new(Span::new(start_span.file, start_span.start, span.end), ExprType::Parenthezised(expr))))
    }

    fn try_parse_block_expr(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        let (body, span) = self.try_parse_block(depth)?;
        Ok(self.arena.alloc(Expr::new(span, ExprType::Block(body))))
    }

    fn try_parse_block(&mut self, depth: usize) -> Result<(Vec<&'a Expr<'a, 'i>>, Span), InternalError> {
        trace!("{}try_parse_block: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let start_span = match self.peek_token(0) {
            Some(Token { span, typ: TokenType::OpenCurly }) => span,
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::OpenCurly])))
        };
        drop(self.next_token());
        let body = self.parse_block_body(depth + 1);
        let span = match self.next_token() {
            Some(Token { span, typ: TokenType::CloseCurly }) => span,
            _ => {
                let span = Span::new(start_span.file, start_span.start, body.last().map(|e| e.span.end).unwrap_or(start_span.start));
                self.diagnostics.error(ErrorCode::UnclosedBlock)
                    .with_error_label(span, "unclosed block")
                    .emit();
                span
            },
        };
        trace!("{} block: {{ {} }}", "|".repeat(depth), body.iter().fold(String::new(), |s, e| s + &e.to_string()));
        Ok((body, Span::new(start_span.file, start_span.start, span.end)))
    }

    fn parse_block_body(&mut self, depth: usize) -> Vec<&'a Expr<'a, 'i>> {
        trace!("{}parse_block_body: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        enum Last {
            Terminated,
            Unterminated(Span),
        }

        self.push_scope();
        let mut exprs = Vec::new();
        let mut last = Last::Terminated;
        let mut last_span = self.peek_token(0).unwrap().span;

        while !self.tokens.is_empty() && self.peek_token(0).unwrap().typ != TokenType::Eof && self.peek_token(0).unwrap().typ != TokenType::CloseCurly {
            let span = self.peek_token(0).unwrap().span;
            let expr = match self.parse_expr_or_stmt(last_span, depth+1) {
                Ok(expr) => expr,
                Err(_) => {
                    trace!("{}    got error, recovering with {} exprs", "|".repeat(depth), exprs.len());
                    // recover
                    self.consume_until_next_end_token();
                    return exprs;
                }
            };
            trace!("{} block got expression {}", "|".repeat(depth), expr);

            // handle missing semicolon
            match last {
                Last::Terminated => (),
                Last::Unterminated(span) => self.diagnostics.error(ErrorCode::MissingSemicolon)
                    .with_info_label(Span::new(span.file, span.end, span.end), "try adding a semicolon here")
                    .emit(),
            }
            match expr.typ {
                ExprType::Statement(_) => last = Last::Terminated,
                ExprType::FunctionDefinition(..) => last = Last::Terminated,
                _ => last = Last::Unterminated(expr.span),
            }

            exprs.push(expr);
            last_span = span;
        }
        self.pop_scope();
        exprs
    }

    fn try_parse_not(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}trace_parse_not: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        match self.peek_token(0) {
            Some(Token { span, typ: TokenType::Exclamation }) => {
                drop(self.next_token());
                let expr = self.try_parse_until_including(ParseUntil::None, depth+1)?;
                trace!("{} not: !{}", "|".repeat(depth), expr);
                Ok(self.arena.alloc(Expr::new(span, ExprType::BoolNot(expr))))
            }
            _ => Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::DqString]))),
        }
    }

    fn try_parse_string(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}trace_parse_string: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        match self.peek_token(0) {
            Some(Token { span, typ: TokenType::DqString(s) }) => {
                drop(self.next_token());
                trace!("{} string: {:?}", "|".repeat(depth), s);
                Ok(self.arena.alloc(Expr::new(span, ExprType::String(s))))
            }
            _ => Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::DqString]))),
        }
    }

    fn try_parse_variable(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_variable: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        match self.peek_token(0) {
            Some(Token { span, typ: TokenType::Ident(ident) }) => {
                drop(self.next_token());
                let binding = match self.get_binding(ident) {
                    Some(binding) => binding,
                    None => self.diagnostic_unknown_identifier(span, ident, |d| d),
                };
                trace!("{} var: {}", "|".repeat(depth), binding.ident);
                Ok(self.arena.alloc(Expr::new(span, ExprType::Variable(binding))))
            },
            _ => Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Ident]))),
        }
    }

    fn try_parse_bind(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_bind: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let mark = self.tokens.mark();
        let let_span = match self.next_token() {
            Some(Token { span, typ: TokenType::Let }) => span,
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Let]))),
        };
        let is_mut = match self.peek_token(0) {
            Some(Token { typ: TokenType::Mut, .. }) => {
                drop(self.next_token());
                true
            }
            _ => false
        };
        let res = match self.try_parse_assign(CreateBinding::Yes(is_mut), depth+1) {
            Ok(Expr { span, typ: ExprType::Assign((binding, _ident_span), expr) }) => {
                let span = Span::new(span.file, let_span.start, span.end);
                &*self.arena.alloc(Expr::new(span, ExprType::Bind(*binding, expr)))
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
        trace!("{} bind: {}", "|".repeat(depth), res);
        Ok(res)
    }

    fn try_parse_assign(&mut self, create_binding: CreateBinding, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_assign: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let mark = self.tokens.mark();
        let (ident, ident_span) = match self.next_token() {
            Some(Token { span, typ: TokenType::Ident(ident)}) => (ident, span),
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Ident]))),
        };
        // TODO: Type
        match self.next_token() {
            Some(Token { span: _, typ: TokenType::Assign }) => (),
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Assign]))),
        };
        let expr = self.try_parse_until_including(ParseUntil::All, depth+1)?;
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

        trace!("{} assign: {} = {}", "|".repeat(depth), ident, expr);
        Ok(self.arena.alloc(Expr::new(Span::new(expr.span.file, ident_span.start, expr.span.end), ExprType::Assign((binding, ident_span), expr))))
    }

    fn try_parse_immediate(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_immediate: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        match self.peek_token(0) {
            Some(Token { span, typ: TokenType::Integer(i, _radix) }) => {
                drop(self.next_token());
                Ok(self.arena.alloc(Expr::new(span, ExprType::Integer(i))))
            }
            Some(Token { span, typ: TokenType::Float(f, _radix) }) => {
                drop(self.next_token());
                Ok(self.arena.alloc(Expr::new(span, ExprType::Float(f))))
            }
            Some(Token { span, typ: TokenType::Bool(b) }) => {
                drop(self.next_token());
                Ok(self.arena.alloc(Expr::new(span, ExprType::Bool(b))))
            }
            _ => Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Immediate]))),
        }
    }

    fn try_parse_fn_call(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_fn_call: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let mark = self.tokens.mark();

        let (ident, ident_span) = match self.next_token() {
            Some(Token { span, typ: TokenType::Ident(ident) }) => (ident, span),
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Ident]))),
        };
        trace!("{} name: {}", "|".repeat(depth), ident);
        match self.next_token() {
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
            match self.peek_token(0) {
                Some(Token { span, typ: TokenType::CloseParen }) => {
                    drop(self.next_token());
                    break self.arena.alloc(Expr::new(Span::new(span.file, ident_span.start, span.end), ExprType::FunctionCall((binding, ident_span), args)));
                },
                None => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Argument, Expected::CloseParen]))),
                _ => (),
            }
            let arg = self.parse_expr(last_span, depth+1)?;
            trace!("{} arg{}: {}", "|".repeat(depth), args.len(), arg);
            args.push(arg);
            match self.next_token() {
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

    fn try_parse_fn_def(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_fn_def: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let mark = self.tokens.mark();
        let (fn_binding, args, ret_type, signature_span) = self.try_parse_fn_header(depth+1)?;
        self.push_scope();
        for (arg, _typ) in &args {
            self.scopes.last_mut().unwrap().idents.insert(arg.ident, *arg);
        }
        let (stmts, span) = self.try_parse_block(depth+1)?;
        self.pop_scope();
        mark.apply();
        let span = Span::new(span.file, signature_span.start, span.end);
        Ok(self.arena.alloc(Expr::new(span, ExprType::FunctionDefinition(fn_binding, args, ret_type, stmts))))
    }

    /// parse the function signature `fn name(args...) -> ret` without the function body
    fn try_parse_fn_header(&mut self, depth: usize) -> Result<(Binding<'i>, Vec<(Binding<'i>, SpecificType)>, SpecificType, Span), InternalError> {
        let mark = self.tokens.mark();
        let fn_span = match self.next_token() {
            Some(Token { span, typ: TokenType::Fn }) => span,
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Fn])))
        };
        let (ident, span) = match self.next_token() {
            Some(Token { span, typ: TokenType::Ident(ident) }) => (ident, span),
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Ident]))),
        };
        let _open_paren_span = match self.next_token() {
            Some(Token { span, typ: TokenType::OpenParen }) => span,
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::OpenParen]))),
        };
        let mut args = Vec::new();
        let close_paren_span = loop {
            if let Some(Token { span, typ: TokenType::CloseParen }) = self.peek_token(0) {
                drop(self.next_token());
                break span;
            }
            let mutable = match self.peek_token(0) {
                Some(Token { span, typ: TokenType::Mut }) => {
                    drop(self.next_token());
                    Some(span)
                },
                _ => None,
            };
            let (ident, ident_span) = match self.next_token() {
                Some(Token { span, typ: TokenType::Ident(ident) }) => (ident, span),
                _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Ident]))),
            };
            let _colon_span = match self.next_token() {
                Some(Token { span, typ: TokenType::Colon }) => span,
                _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Colon])))
            };
            let (typ, _typ_span) = self.try_parse_type(depth+1)?;
            let binding = Binding {
                id: BindingId::unique(),
                ident,
                mutable: mutable.is_some(),
                span: ident_span,
                rogue: false,
            };
            args.push((binding, typ));
            match self.next_token() {
                Some(Token { span: _, typ: TokenType::Comma }) => (),
                Some(Token { span, typ: TokenType::CloseParen }) => break span,
                _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Comma, Expected::CloseParen]))),
            }
        };
        let (ret_type, ret_type_span) = match self.peek_token(0) {
            Some(Token { span: _, typ: TokenType::Arrow }) => {
                drop(self.next_token());
                match self.try_parse_type(depth+1) {
                    Ok((typ, span)) => (typ, span),
                    Err(InternalError::Backtrack(_)) => (SpecificType::Unit, Span::new(close_paren_span.file, close_paren_span.end, close_paren_span.end)),
                    Err(InternalError::Error(e)) => return Err(InternalError::Error(e)),
                }
            }
            _ => (SpecificType::Unit, Span::new(close_paren_span.file, close_paren_span.end, close_paren_span.end)),
        };
        mark.apply();
        let binding = Binding {
            id: BindingId::unique(),
            ident,
            mutable: false,
            span,
            rogue: false,
        };
        Ok((binding, args, ret_type, Span::new(fn_span.file, fn_span.start, ret_type_span.end)))
    }

    fn try_parse_type(&mut self, depth: usize) -> Result<(SpecificType, Span), InternalError> {
        trace!("{}try_parse_type: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        match self.next_token() {
            Some(Token { span: open_span, typ: TokenType::OpenParen }) => match self.next_token() {
                Some(Token { span: close_span, typ: TokenType::CloseParen }) => return Ok((SpecificType::Unit, Span::new(open_span.file, open_span.start, close_span.end))),
                _ => ()
            }
            Some(Token { span, typ: TokenType::StringType }) => return Ok((SpecificType::String, span)),
            Some(Token { span, typ: TokenType::IntType }) => return Ok((SpecificType::Integer, span)),
            Some(Token { span, typ: TokenType::FloatType }) => return Ok((SpecificType::Float, span)),
            Some(Token { span, typ: TokenType::BoolType }) => return Ok((SpecificType::Bool, span)),
            _ => (),
        }
        Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Type])))
    }

    fn try_parse_compare(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_compare: {}", "|".repeat(depth), self.peek_token(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let mark = self.tokens.mark();
        let left = self.try_parse_until_excluding(ParseUntil::Compare, depth+1)?;
        let op = match self.next_token() {
            Some(Token { typ: TokenType::LessThan, .. }) => ExprType::LessThan,
            Some(Token { typ: TokenType::LessEquals, .. }) => ExprType::LessEquals,
            Some(Token { typ: TokenType::Equals, .. }) => ExprType::Equals,
            Some(Token { typ: TokenType::NotEquals, .. }) => ExprType::NotEquals,
            Some(Token { typ: TokenType::FloatEquals, .. }) => ExprType::FloatEquals,
            Some(Token { typ: TokenType::FloatNotEquals, .. }) => ExprType::FloatNotEquals,
            Some(Token { typ: TokenType::GreaterEquals, .. }) => ExprType::GreaterEquals,
            Some(Token { typ: TokenType::GreaterThan, .. }) => ExprType::GreaterThan,
            _ => return Err(InternalError::Backtrack(Cow::Borrowed(&[Expected::Equals]))),
        };
        let right = self.try_parse_until_including(ParseUntil::Compare, depth+1)?;
        mark.apply();
        let res = self.arena.alloc(Expr::new(Span::new(left.span.file, left.span.start, right.span.end), op(left, right)));
        trace!("{} compare: {} ", "|".repeat(depth), res);
        Ok(res)
    }

    fn similar_ident(&self, ident: &'i str) -> Option<&'i str> {
        self.scopes.iter()
            .flat_map(|scope| scope.idents.keys())
            .map(|s| (strsim::levenshtein(ident, s), s))
            // .filter(|&(dist, _)| dist <= 3)
            .min_by_key(|&(dist, _)| dist)
            .map(|(_, &s)| s)
    }

    fn diagnostic_unknown_identifier(&mut self, span: Span, ident: &'i str, f: impl for<'d> FnOnce(DiagnosticBuilder<'d, ErrorCode>) -> DiagnosticBuilder<'d, ErrorCode>) -> Binding<'i> {
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
        let expected: Vec<_> = expected.iter()
            .map(|&expected| {
                use Expected::*;
                match expected {
                    DqString => "double-quoted string",
                    Ident => "identifier",
                    Let => "`let`",
                    Fn => "`fn`",
                    Type => "type",
                    Assign => "`=`",
                    Equals => "`==`",
                    Immediate => "integer or float",
                    MathOp => "`+`, `-`, `*`, `/`",
                    BooleanExprOp => "`&&`, `||`",
                    OpenParen => "`(`",
                    CloseParen => "`)`",
                    OpenCurly => "`{`",
                    Argument => "function argument",
                    Comma => "`,`",
                    Colon => "`:`",
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
        match self.tokens.peek(0) {
            Some(Token { typ: TokenType::LineComment(c), .. })
            | Some(Token { typ: TokenType::BlockComment(c), .. }) => {
                drop(self.tokens.next().unwrap());
                trace!("dropped comment {}", c);
            },
            _ => (),
        }
    }

    /// Consume until the next end token (`;`, `}`) in the hope that that allows us to recover.
    fn consume_until_next_end_token(&mut self) -> Option<Span> {
        let start_span = self.peek_token(0)?.span;
        let mut last_end = start_span.end;
        loop {
            match self.peek_token(0) {
                Some(Token { span, typ: TokenType::Semicolon })
                | Some(Token { span, typ: TokenType::CloseCurly }) => {
                    return Some(Span::new(start_span.file, start_span.start, span.end))
                },
                Some(Token { span, .. }) => {
                    drop(self.next_token());
                    last_end = span.end
                },
                None => return Some(Span::new(start_span.file, start_span.start, last_end)),
            }
        }
    }
}
