use std::borrow::Cow;

use crate::lexer::Token;
use super::{Expr, InternalError, Parser, Backtrack};
use crate::parser::{Expected};
use crate::parser::expr::{ExprBoolAnd, ExprBoolOr, ExprAdd, ExprSub, ExprMul, ExprDiv, ExprMod, ExprXor, ParseUntil};
use crate::common::{Depth, Spanned};

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
    fn expr_type_constructor<'a, 'i>(self) -> fn(&'a Expr<'a, 'i>, Token<'i>, &'a Expr<'a, 'i>) -> Expr<'a, 'i>;
    fn expected() -> Cow<'static, [Expected]>;
    fn primitive_parse_fn<'a, 'b, 'i>() -> fn(&mut Parser<'a, 'b, 'i>, Depth) -> Result<&'a Expr<'a, 'i>, InternalError>;
}

#[derive(Debug, Clone, Copy)]
pub enum Math {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Xor,
}

impl Precedence for Math {
    fn try_from_token(token: Token<'_>) -> Result<Self, InternalError> {
        match token {
            Token::Plus(_) => Ok(Math::Add),
            Token::Minus(_) => Ok(Math::Sub),
            Token::Star(_) => Ok(Math::Mul),
            Token::Slash(_) => Ok(Math::Div),
            Token::Percent(_) => Ok(Math::Mod),
            Token::Circumflex(_) => Ok(Math::Xor),
            _ => Err(InternalError::Backtrack(Backtrack { span: token.span_(), expected: Self::expected() })),
        }
    }

    fn precedence(self) -> u8 {
        match self {
            Math::Add | Math::Sub => 0,
            Math::Mul | Math::Div | Math::Mod => 1,
            Math::Xor => 2,
        }
    }

    fn expr_type_constructor<'a, 'i>(self) -> fn(&'a Expr<'a, 'i>, Token<'i>, &'a Expr<'a, 'i>) -> Expr<'a, 'i> {
        match self {
            Math::Add => ExprAdd::new_as_expr,
            Math::Sub => ExprSub::new_as_expr,
            Math::Mul => ExprMul::new_as_expr,
            Math::Div => ExprDiv::new_as_expr,
            Math::Mod => ExprMod::new_as_expr,
            Math::Xor => ExprXor::new_as_expr,
        }
    }

    fn expected() -> Cow<'static, [Expected]> {
        Cow::Borrowed(Expected::MATH_OP)
    }

    fn primitive_parse_fn<'a, 'b, 'i>() -> fn(&mut Parser<'a, 'b, 'i>, Depth) -> Result<&'a Expr<'a, 'i>, InternalError> {
        |parser, depth| Expr::try_parse_until_excluding(parser, ParseUntil::Math, depth)
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
            _ => Err(InternalError::Backtrack(Backtrack { span: token.span_(), expected: Self::expected() })),
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
        Cow::Borrowed(Expected::BOOL_OP)
    }

    fn primitive_parse_fn<'a, 'b, 'i>() -> fn(&mut Parser<'a, 'b, 'i>, Depth) -> Result<&'a Expr<'a, 'i>, InternalError> {
        |parser, depth| Expr::try_parse_until_excluding(parser, ParseUntil::BooleanExpr, depth)
    }
}

impl<'a, 'i> Expr<'a, 'i> {
    pub(super) fn try_parse_precedence<P: Precedence>(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{} Expr::try_parse_precedence {}        ({:?})", depth, ::std::any::type_name::<P>(), parser.peek_token(0));
        let mark = parser.lexer.mark();
        let mut lhs = P::primitive_parse_fn()(parser, depth.next())?;
        lhs = Expr::try_parse_precedence_inner::<P>(parser, lhs, depth.next())?;
        loop {
            trace!("{}    lhs: {}", depth, lhs);
            lhs = match Self::try_parse_precedence_inner::<P>(parser, lhs, depth.next()) {
                Ok(expr) => expr,
                Err(_) => {
                    mark.apply();
                    trace!("{}    returning {}", depth, lhs);
                    return Ok(lhs)
                }
            }
        }
    }

    fn try_parse_precedence_inner<P: Precedence>(parser: &mut Parser<'a, '_, 'i>, lhs: &'a Expr<'a, 'i>, depth: Depth) -> Result<&'a Expr<'a, 'i>, InternalError> {
        let op_token = parser.peek_token(0)?;
        let op = P::try_from_token(op_token.clone())?;
        drop(parser.next_token());
        let mut rhs = P::primitive_parse_fn()(parser, depth.next())?;
        loop {
            trace!("{}    rhs: {}", depth, rhs);
            let next = parser.peek_token(0)?;
            let op2 = match P::try_from_token(next) {
                Ok(op) => op,
                Err(_) => return Ok(parser.arena.alloc(op.expr_type_constructor()(lhs, op_token, rhs))),
            };
            if op2.precedence() <= op.precedence() {
                return Ok(parser.arena.alloc(op.expr_type_constructor()(lhs, op_token, rhs)));
            }
            rhs = match Self::try_parse_precedence_inner::<P>(parser, rhs, depth.next()) {
                Ok(rhs) => rhs,
                Err(_) => return Ok(parser.arena.alloc(op.expr_type_constructor()(lhs, op_token, rhs))),
            }
        }
    }
}
