use std::borrow::Cow;

use crate::lexer::Token;
use super::{Expr, InternalError, Parser};
use crate::parser::{Expected};
use crate::parser::expr::{ExprBoolAnd, ExprBoolOr, ExprAdd, ExprSub, ExprMul, ExprDiv, ParseUntil};

// make trace! here log as if this still was the parser module
#[allow(unused_macros)]
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
    fn expr_type_constructor<'a, 'i>(self) -> fn(&'a Expr<'a, 'i>, Token<'i>, &'a Expr<'a, 'i>) -> Expr<'a, 'i>;
    fn expected() -> Cow<'static, [Expected]>;
    fn primitive_parse_fn<'a, 'b, 'i>() -> fn(&mut Parser<'a, 'b, 'i>) -> Result<&'a Expr<'a, 'i>, InternalError>;
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
        match token {
            Token::Plus(_) => Ok(Math::Add),
            Token::Minus(_) => Ok(Math::Sub),
            Token::Star(_) => Ok(Math::Mul),
            Token::Slash(_) => Ok(Math::Div),
            _ => Err(InternalError::Backtrack(token.span(), Self::expected())),
        }
    }

    fn precedence(self) -> u8 {
        match self {
            Math::Add | Math::Sub => 0,
            Math::Mul | Math::Div => 1,
        }
    }

    fn expr_type_constructor<'a, 'i>(self) -> fn(&'a Expr<'a, 'i>, Token<'i>, &'a Expr<'a, 'i>) -> Expr<'a, 'i> {
        match self {
            Math::Add => ExprAdd::new_as_expr,
            Math::Sub => ExprSub::new_as_expr,
            Math::Mul => ExprMul::new_as_expr,
            Math::Div => ExprDiv::new_as_expr,
        }
    }

    fn expected() -> Cow<'static, [Expected]> {
        Cow::Borrowed(Expected::COMPARE_OP)
    }

    fn primitive_parse_fn<'a, 'b, 'i>() -> fn(&mut Parser<'a, 'b, 'i>) -> Result<&'a Expr<'a, 'i>, InternalError> {
        |parser| Expr::try_parse_until_excluding(parser, ParseUntil::Math)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BooleanExpr {
    And,
    Or,
}

impl Precedence for BooleanExpr {
    fn try_from_token(token: Token<'_>) -> Result<Self, InternalError> {
        match token {
            Token::DoubleAmp(_) => Ok(BooleanExpr::And),
            Token::DoublePipe(_) => Ok(BooleanExpr::Or),
            _ => Err(InternalError::Backtrack(token.span(), Self::expected())),
        }
    }

    fn precedence(self) -> u8 {
        match self {
            BooleanExpr::Or => 0,
            BooleanExpr::And => 1,
        }
    }

    fn expr_type_constructor<'a, 'i>(self) -> fn(&'a Expr<'a, 'i>, Token<'i>, &'a Expr<'a, 'i>) -> Expr<'a, 'i> {
        match self {
            BooleanExpr::And => ExprBoolAnd::new_as_expr,
            BooleanExpr::Or => ExprBoolOr::new_as_expr,
        }
    }

    fn expected() -> Cow<'static, [Expected]> {
        Cow::Borrowed(Expected::MATH_OP)
    }

    fn primitive_parse_fn<'a, 'b, 'i>() -> fn(&mut Parser<'a, 'b, 'i>) -> Result<&'a Expr<'a, 'i>, InternalError> {
        |parser| Expr::try_parse_until_excluding(parser, ParseUntil::BooleanExpr)
    }
}

impl<'a, 'i> Expr<'a, 'i> {
    pub(super) fn try_parse_precedence<P: Precedence>(parser: &mut Parser<'a, '_, 'i>) -> Result<&'a Expr<'a, 'i>, InternalError> {
        let mark = parser.tokens.mark();
        let mut lhs = P::primitive_parse_fn()(parser)?;
        lhs = Expr::try_parse_precedence_inner::<P>(parser, lhs)?;
        loop {
            lhs = match Self::try_parse_precedence_inner::<P>(parser, lhs) {
                Ok(expr) => expr,
                Err(_) => {
                    mark.apply();
                    return Ok(lhs)
                }
            }
        }
    }

    fn try_parse_precedence_inner<P: Precedence>(parser: &mut Parser<'a, '_, 'i>, lhs: &'a Expr<'a, 'i>) -> Result<&'a Expr<'a, 'i>, InternalError> {
        let op_token = match parser.tokens.peek(0) {
            Some(token) => token,
            None => return Err(InternalError::Backtrack(parser.tokens.last_span(), P::expected())),
        };
        let op = P::try_from_token(op_token.clone())?;
        drop(parser.tokens.next());
        let mut rhs = P::primitive_parse_fn()(parser)?;
        loop {
            let next = match parser.tokens.peek(0) {
                Some(token) => token,
                None => return Ok(parser.arena.alloc(op.expr_type_constructor()(lhs, op_token, rhs))),
            };
            let op2 = match P::try_from_token(next) {
                Ok(op) => op,
                Err(_) => return Ok(parser.arena.alloc(op.expr_type_constructor()(lhs, op_token, rhs))),
            };
            if op2.precedence() < op.precedence() {
                return Ok(parser.arena.alloc(op.expr_type_constructor()(lhs, op_token, rhs)));
            }
            rhs = match Self::try_parse_precedence_inner::<P>(parser, rhs) {
                Ok(rhs) => rhs,
                Err(_) => return Ok(parser.arena.alloc(op.expr_type_constructor()(lhs, op_token, rhs))),
            }
        }
    }
}
