use std::fmt;
use std::borrow::Cow;
use std::collections::HashMap;

use typed_arena::Arena;
use diagnostic::{Span, Diagnostics, DiagnosticBuilder, FileId};

use crate::lexer::{Tokens, Token, TokenType, TokenMut, TokenIdent, TokenOpenCurly, TokenCloseCurly, TokenCloseParen, TokenOpenParen, TokenLineComment, TokenBlockComment};
use crate::error_codes::ErrorCode;
use crate::scope::BindingId;

mod expr;
mod precedence;
mod parse;

pub use expr::*;
pub use parse::{Parse, Spanned, Separated};
use crate::common::{PreTypeInfo, SpecificType, FunctionType, Type, Value, FunctionImpl};
use indexmap::map::IndexMap;
use itertools::Itertools;

#[derive(Debug)]
pub enum Error {
    /// Parsing encountered an unrecoverable error and a diagnostic was emitted. Abort.
    Abort,
}
#[derive(Debug)]
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
        Expected::Token(TokenType::FuzzyEquals),
        Expected::Token(TokenType::FuzzyNotEquals),
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
    idents: IndexMap<&'i str, Binding<'i>>
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
            scopes: vec![Scope { idents: IndexMap::new() }],
        };
        parser.first_pass();
        // make existing bindings known to parser
        for &binding in parser.pre_info.bindings.keys() {
            if let Some(old) = parser.scopes.last_mut().unwrap().idents.insert(binding.ident.ident, binding) {
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
        let mark = self.tokens.mark();
        while self.tokens.peek(0).is_some() {
            match ExprFunctionDefinition::parse(self) {
                Ok(fun) => {
                    let fun_expr = &*self.arena.alloc(Expr::FunctionDefinition(fun));
                    let fun = match fun_expr {
                        Expr::FunctionDefinition(fun) => fun,
                        _ => unreachable!("we just inserted you"),
                    };
                    let typ = SpecificType::Function(Box::new(FunctionType {
                        args: fun.args.iter().map(|pattern| Type::Specific(SpecificType::from(&pattern.typ))).collect(),
                        ret: Type::Specific(fun.ret_type.as_ref().map(|(_, typ)| SpecificType::from(typ)).unwrap_or(SpecificType::Unit)),
                    }));
                    self.pre_info.bindings.insert(fun.binding, typ);
                    self.pre_info.rebo_functions.insert(fun.binding.id, &fun.body);
                    let arg_binding_ids = fun.args.iter().map(|ExprPatternTyped { pattern: ExprPatternUntyped { binding }, .. }| binding.id).collect();
                    self.pre_info.root_scope.create(fun.binding.id, Value::Function(FunctionImpl::Rebo(fun.binding.id, arg_binding_ids)));
                    self.pre_parsed.insert((fun.fn_token.span.file, fun.fn_token.span.start), fun_expr);
                }
                _ => drop(self.tokens.next()),
            }
        }
        // reset token lookahead
        drop(mark);
        self.scopes = old_scopes;
    }

    pub fn parse_ast(mut self) -> Result<Ast<'a, 'i>, Error> {
        trace!("parse_ast");
        // file scope
        let body: BlockBody = match self.parse() {
            Ok(body) => body,
            Err(InternalError::Backtrack(span, expected)) => {
                self.diagnostic_expected(ErrorCode::InvalidExpression, span, &expected);
                return Err(Error::Abort)
            },
            Err(InternalError::Error(e)) => return Err(e),
        };
        // make sure everything parsed during first-pass was consumed and used by the second pass
        assert!(self.pre_parsed.is_empty(), "not everything from first-pass was consumed: {:?}", self.pre_parsed);
        assert!(self.tokens.peek(0).is_none() || matches!(self.tokens.peek(0), Some(Token::Eof(_))), "not all tokens were consumed: {}", self.tokens.iter().map(|t| format!("    {:?}", t)).join("\n"));
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
        let id = BindingId::unique();
        let name = ident.ident;
        let binding = Binding { id, ident, mutable, rogue };
        self.scopes.last_mut().unwrap().idents.insert(name, binding);
        binding
    }
    fn get_binding(&mut self, ident: &'i str) -> Option<Binding<'i>> {
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

    fn next_token(&mut self) -> Option<Token<'i>> {
        self.consume_comments();
        self.tokens.next()
    }
    fn peek_token(&mut self, index: usize) -> Option<Token<'i>> {
        let mut non_comments = 0;
        for i in 0.. {
            match self.tokens.peek(i)? {
                Token::BlockComment(_) | Token::LineComment(_) => (),
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

    pub(in crate::parser) fn parse<T: Parse<'a, 'i>>(&mut self) -> Result<T, InternalError> {
        T::parse(self)
    }

    fn similar_ident(&self, ident: &'i str) -> Option<&'i str> {
        self.scopes.iter()
            .flat_map(|scope| scope.idents.keys())
            .map(|s| (strsim::levenshtein(ident, s), s))
            // .filter(|&(dist, _)| dist <= 3)
            .min_by_key(|&(dist, _)| dist)
            .map(|(_, &s)| s)
    }

    fn diagnostic_unknown_identifier(&mut self, ident: TokenIdent<'i>, f: impl for<'d> FnOnce(DiagnosticBuilder<'d, ErrorCode>) -> DiagnosticBuilder<'d, ErrorCode>) -> Binding<'i> {
        let mut d = self.diagnostics.error(ErrorCode::UnknownIdentifier)
            .with_error_label(ident.span, format!("variable `{}` doesn't exist", ident.ident));
        d = f(d);
        if let Some(similar) = self.similar_ident(ident.ident) {
            d = d.with_info_label(ident.span, format!("did you mean the similarly named variable `{}`", similar));
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
        match self.tokens.peek(0) {
            Some(Token::LineComment(TokenLineComment { comment, .. }))
            | Some(Token::BlockComment(TokenBlockComment { comment, .. })) => {
                drop(self.tokens.next().unwrap());
                trace!("dropped comment {}", comment);
            },
            _ => (),
        }
    }

    /// Consume until excluding the next instance of one of the passed tokens
    fn consume_until(&mut self, until: &[TokenType]) -> Consumed<'i> {
        let start_span = match self.peek_token(0) {
            Some(token) => token.span(),
            None => return Consumed::InstantEof,
        };
        let mut paren_depth = 0;
        let mut curly_depth = 0;
        let mut last_end = start_span.end;
        loop {
            match self.peek_token(0) {
                Some(token) if until.contains(&token.typ()) && paren_depth == 0 && curly_depth == 0 => {
                    return Consumed::Found(Span::new(start_span.file, start_span.start, token.span().end), token)
                },
                Some(Token::OpenParen(TokenOpenParen { span })) => {
                    last_end = span.end;
                    paren_depth += 1
                },
                Some(Token::CloseParen(TokenCloseParen { span })) => {
                    last_end = span.end;
                    paren_depth -= 1
                },
                Some(Token::OpenCurly(TokenOpenCurly { span })) => {
                    last_end = span.end;
                    curly_depth += 1
                },
                Some(Token::CloseCurly(TokenCloseCurly { span })) => {
                    last_end = span.end;
                    curly_depth -= 1
                },
                Some(token) => last_end = token.span().end,
                None => return Consumed::Eof(Span::new(start_span.file, start_span.start, last_end)),
            }
            drop(self.next_token());
        }
    }
}


enum Consumed<'i> {
    InstantEof,
    Found(Span, Token<'i>),
    Eof(Span),
}
