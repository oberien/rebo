use std::fmt;
use std::collections::HashMap;

use typed_arena::Arena;

use crate::lexer::{Tokens, Token, TokenType};
use crate::diagnostics::Span;
use crate::scope::BindingId;

#[derive(Debug)]
pub struct Expr<'a, 'i> {
    pub span: Span,
    pub typ: ExprType<'a, 'i>,
}

#[derive(Debug, Clone)]
pub struct Binding<'i> {
    pub id: BindingId,
    pub ident: &'i str,
    pub mutable: bool,
    /// Span of the original declaration binding
    pub span: Span,
    /// If this is a rogue binding that was created by the parser when an error occurred.
    /// If there is a further error involving this binding, it shouldn't be emitted.
    pub rogue: bool,
}


#[derive(Debug)]
pub enum ExprType<'a, 'i> {
    Unit,
    Variable((Binding<'i>, Span)),
    Integer(i64),
    Float(f64),
    String(String),
    // let foo = bar
    Bind(Binding<'i>, &'a Expr<'a, 'i>),
    // foo = bar
    Assign((Binding<'i>, Span), &'a Expr<'a, 'i>),
    Add(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    Sub(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    Mul(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    Div(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    Statement(&'a Expr<'a, 'i>),
    FunctionCall((Binding<'i>, Span), Vec<&'a Expr<'a, 'i>>),
}

impl<'a, 'i> fmt::Display for Expr<'a, 'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl<'a, 'i> fmt::Display for ExprType<'a, 'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::Unit => write!(f, "() ", ),
            ExprType::Variable((binding, _)) => write!(f, "{} ", binding.ident),
            ExprType::Integer(i) => write!(f, "{} ", i),
            ExprType::Float(fl) => write!(f, "{:2.1} ", fl),
            ExprType::String(s) => write!(f, "{:?} ", s),
            ExprType::Bind(binding, expr) => {
                write!(f, "let ")?;
                if binding.mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "{} = {}", binding.ident, expr)
            }
            ExprType::Assign((binding, _), expr) => write!(f, "{} = {}", binding.ident, expr),
            ExprType::Add(a, b) => write!(f, "+ {}{}", a, b),
            ExprType::Sub(a, b) => write!(f, "- {}{}", a, b),
            ExprType::Mul(a, b) => write!(f, "* {}{}", a, b),
            ExprType::Div(a, b) => write!(f, "/ {}{}", a, b),
            ExprType::Statement(expr) => write!(f, "{}; ", expr),
            ExprType::FunctionCall((binding, _), exprs) => {
                write!(f, "{}(", binding.ident)?;
                for expr in exprs {
                    write!(f, "{}, ", expr)?;
                }
                write!(f, ") ")
            }
        }
    }
}

impl<'a, 'i> Expr<'a, 'i> {
    pub fn new(span: Span, typ: ExprType<'a, 'i>) -> Expr<'a, 'i> {
        Expr { span, typ }
    }
}
