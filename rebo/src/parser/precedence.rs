use std::borrow::Cow;

use typed_arena::Arena;

use crate::lexer::{Token, Tokens, TokenType};
use super::{Expr, InternalError, Parser};
use crate::parser::{Expected, ExprType};
use crate::diagnostics::Span;

pub(super) trait Precedence: Sized + Copy {
    fn try_from_token(token: Token<'_>) -> Result<Self, InternalError>;
    fn precedence(self) -> u8;
    fn expr_type_constructor<'a, 'i>(self) -> fn(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>) -> ExprType<'a, 'i>;
    fn expected() -> Cow<'static, [Expected]>;
    fn primitive_parse_fn<'a, 'i, 'r>() -> fn(&mut Parser<'a, 'i, 'r>, usize) -> Result<&'a Expr<'a, 'i>, InternalError>;
}

#[derive(Debug, Clone, Copy)]
pub enum Math {
    Add,
    Sub,
    Mul,
    Div,
}

impl Precedence for Math {
    fn try_from_token(token: Token<'_>) -> Result<Self, InternalError> {
        match token.typ {
            TokenType::Plus => Ok(Math::Add),
            TokenType::Minus => Ok(Math::Sub),
            TokenType::Star => Ok(Math::Mul),
            TokenType::Slash => Ok(Math::Div),
            _ => Err(InternalError::Backtrack(Self::expected())),
        }
    }

    fn precedence(self) -> u8 {
        match self {
            Math::Add | Math::Sub => 0,
            Math::Mul | Math::Div => 1,
        }
    }

    fn expr_type_constructor<'a, 'i>(self) -> fn(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>) -> ExprType<'a, 'i> {
        match self {
            Math::Add => ExprType::Add,
            Math::Sub => ExprType::Sub,
            Math::Mul => ExprType::Mul,
            Math::Div => ExprType::Div,
        }
    }

    fn expected() -> Cow<'static, [Expected]> {
        Cow::Borrowed(&[Expected::MathOp])
    }

    fn primitive_parse_fn<'a, 'i, 'r>() -> fn(&mut Parser<'a, 'i, 'r>, usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        Parser::try_parse_non_math
    }
}

impl<'a, 'i, 'r> Parser<'a, 'i, 'r> {
    pub(super) fn try_parse_precedence<P: Precedence>(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_precedence", "|".repeat(depth));
        let mark = self.tokens.mark();
        let lhs = P::primitive_parse_fn()(self, depth+1)?;
        let res = self.try_parse_precedence_inner::<P>(lhs, depth + 1)?;
        mark.apply();
        Ok(res)
    }

    fn try_parse_precedence_inner<P: Precedence>(&mut self, lhs: &'a Expr<'a, 'i>, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_precedence_inner", "|".repeat(depth));
        let next = match self.tokens.next() {
            Some(token) => token,
            None => return Err(InternalError::Backtrack(P::expected())),
        };
        let op = P::try_from_token(next)?;
        let mut rhs = P::primitive_parse_fn()(self, depth+1)?;
        loop {
            let next = match self.tokens.peek(0) {
                Some(token) => token,
                None => return Ok(self.arena.alloc(Expr::new(Span::new(lhs.span.file, lhs.span.start, rhs.span.end), op.expr_type_constructor()(lhs, rhs)))),
            };
            let op2 = match P::try_from_token(next) {
                Ok(op) => op,
                Err(_) => return Ok(self.arena.alloc(Expr::new(Span::new(lhs.span.file, lhs.span.start, rhs.span.end), op.expr_type_constructor()(lhs, rhs)))),
            };
            if op2.precedence() <= op.precedence() {
                return Ok(self.arena.alloc(Expr::new(Span::new(lhs.span.file, lhs.span.start, rhs.span.end), op.expr_type_constructor()(lhs, rhs))));
            }
            rhs = self.try_parse_precedence_inner::<P>(rhs, depth+1)?;
        }
    }
}
