use std::borrow::Cow;

use typed_arena::Arena;

use crate::lexer::{Token, Tokens, TokenType};
use super::{Expr, InternalError, Parser};
use crate::parser::{Expected, ExprType};
use crate::diagnostics::Span;

// make trace! here log as if this still was the parser module
macro_rules! module_path {
    () => {{
        let path = std::module_path!();
        let end = path.rfind("::").unwrap();
        &path[..end]
    }}
}

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

#[derive(Debug, Clone, Copy)]
pub enum BooleanExpr {
    And,
    Or,
}

impl Precedence for BooleanExpr {
    fn try_from_token(token: Token<'_>) -> Result<Self, InternalError> {
        match token.typ {
            TokenType::DoubleAmp => Ok(BooleanExpr::And),
            TokenType::DoublePipe => Ok(BooleanExpr::Or),
            _ => Err(InternalError::Backtrack(Self::expected())),
        }
    }

    fn precedence(self) -> u8 {
        match self {
            BooleanExpr::Or => 0,
            BooleanExpr::And => 1,
        }
    }

    fn expr_type_constructor<'a, 'i>(self) -> fn(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>) -> ExprType<'a, 'i> {
        match self {
            BooleanExpr::And => ExprType::BoolAnd,
            BooleanExpr::Or => ExprType::BoolOr,
        }
    }

    fn expected() -> Cow<'static, [Expected]> {
        Cow::Borrowed(&[Expected::BooleanExprOp])
    }

    fn primitive_parse_fn<'a, 'i, 'r>() -> fn(&mut Parser<'a, 'i, 'r>, usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        Parser::try_parse_non_boolean_expr
    }
}

impl<'a, 'i, 'r> Parser<'a, 'i, 'r> {
    pub(super) fn try_parse_precedence<P: Precedence>(&mut self, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_precedence: {}", "|".repeat(depth), self.tokens.peek(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let mark = self.tokens.mark();
        let mut lhs = P::primitive_parse_fn()(self, depth+1)?;
        lhs = self.try_parse_precedence_inner::<P>(lhs, depth + 1)?;
        loop {
            trace!("{} lhs = {}", "|".repeat(depth), lhs);
            lhs = match self.try_parse_precedence_inner::<P>(lhs, depth + 1) {
                Ok(expr) => expr,
                Err(_) => {
                    mark.apply();
                    return Ok(lhs)
                }
            }
        }
    }

    fn try_parse_precedence_inner<P: Precedence>(&mut self, lhs: &'a Expr<'a, 'i>, depth: usize) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{}try_parse_precedence_inner: {}", "|".repeat(depth), self.tokens.peek(0).map(|t| t.to_string()).unwrap_or_else(|| "".to_string()));
        let next = match self.tokens.peek(0) {
            Some(token) => token,
            None => return Err(InternalError::Backtrack(P::expected())),
        };
        let op = P::try_from_token(next)?;
        drop(self.tokens.next());
        let mut rhs = P::primitive_parse_fn()(self, depth+1)?;
        loop {
            trace!("{} rhs = {}", "|".repeat(depth), rhs);
            let next = match self.tokens.peek(0) {
                Some(token) => token,
                None => return Ok(self.arena.alloc(Expr::new(Span::new(lhs.span.file, lhs.span.start, rhs.span.end), op.expr_type_constructor()(lhs, rhs)))),
            };
            let op2 = match P::try_from_token(next) {
                Ok(op) => op,
                Err(_) => return Ok(self.arena.alloc(Expr::new(Span::new(lhs.span.file, lhs.span.start, rhs.span.end), op.expr_type_constructor()(lhs, rhs)))),
            };
            if op2.precedence() < op.precedence() {
                return Ok(self.arena.alloc(Expr::new(Span::new(lhs.span.file, lhs.span.start, rhs.span.end), op.expr_type_constructor()(lhs, rhs))));
            }
            rhs = match self.try_parse_precedence_inner::<P>(rhs, depth+1) {
                Ok(rhs) => rhs,
                Err(_) => return Ok(self.arena.alloc(Expr::new(Span::new(lhs.span.file, lhs.span.start, rhs.span.end), op.expr_type_constructor()(lhs, rhs)))),
            }
        }
    }
}
