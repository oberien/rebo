use std::fmt;

use typed_arena::Arena;

use crate::lexer::{Tokens, Token, TokenType};
use crate::span::Span;

#[derive(Debug)]
pub struct Ast<'a, 'i> {
    pub exprs: Vec<&'a Expr<'a, 'i>>,
}

impl<'a, 'i> fmt::Display for Ast<'a, 'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for expr in &self.exprs {
            writeln!(f, "{}", expr)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Expr<'a, 'i> {
    pub span: Span,
    pub typ: ExprType<'a, 'i>,
}

#[derive(Debug)]
pub enum ExprType<'a, 'i> {
    Ident(&'i str),
    Integer(i64),
    Float(f64),
    String(String),
    // let foo = bar
    Bind((&'i str, Span), bool, &'a Expr<'a, 'i>),
    // foo = bar
    Assign((&'i str, Span), &'a Expr<'a, 'i>),
    Add(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    Sub(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    Mul(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    Div(&'a Expr<'a, 'i>, &'a Expr<'a, 'i>),
    Statement(&'a Expr<'a, 'i>),
    FunctionCall((&'i str, Span), Vec<&'a Expr<'a, 'i>>),
}

impl<'a, 'i> fmt::Display for Expr<'a, 'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl<'a, 'i> fmt::Display for ExprType<'a, 'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::Ident(ident) => write!(f, "{} ", ident),
            ExprType::Integer(i) => write!(f, "{} ", i),
            ExprType::Float(fl) => write!(f, "{:2.1} ", fl),
            ExprType::String(s) => write!(f, "{:?} ", s),
            &ExprType::Bind((ident, _), is_mut, expr) => {
                write!(f, "let ")?;
                if is_mut {
                    write!(f, "mut ")?;
                }
                write!(f, "{} = {}", ident, expr)
            }
            ExprType::Assign((ident, _), expr) => write!(f, "{} = {}", ident, expr),
            ExprType::Add(a, b) => write!(f, "+ {}{}", a, b),
            ExprType::Sub(a, b) => write!(f, "- {}{}", a, b),
            ExprType::Mul(a, b) => write!(f, "* {}{}", a, b),
            ExprType::Div(a, b) => write!(f, "/ {}{}", a, b),
            ExprType::Statement(expr) => write!(f, "{}; ", expr),
            ExprType::FunctionCall((ident, _), exprs) => {
                write!(f, "{}(", ident)?;
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

pub fn parse<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, mut tokens: Tokens<'i>) -> Ast<'a, 'i> {
    let mut exprs = Vec::new();
    while !tokens.is_empty() && tokens.peek(0).unwrap().typ != TokenType::Eof {
        exprs.push(parse_expr_or_stmt(arena, &mut tokens));
    }
    Ast { exprs }
}

fn parse_expr_or_stmt<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> &'a Expr<'a, 'i> {
    let expr = parse_expr(arena, tokens);
    match tokens.peek(0) {
        Some(Token { span, typ: TokenType::Semicolon }) => {
            drop(tokens.next());
            arena.alloc(Expr::new(Span::new(expr.span.start, span.end), ExprType::Statement(expr)))
        }
        _ => expr
    }
}

fn parse_expr<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> &'a Expr<'a, 'i> {
    let res = match try_parse_expr(arena, tokens) {
        Some(expr) => expr,
        None => todo!("error handling"),
    };
    trace!("parsed expr {:?}", res);
    res
}
fn try_parse_expr<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> Option<&'a Expr<'a, 'i>> {
    trace!("parse_expr");
    match try_parse_string(arena, tokens) {
        Some(expr) => return Some(expr),
        None => (),
    }
    match try_parse_bind(arena, tokens) {
        Some(expr) => return Some(expr),
        None => (),
    }
    match try_parse_assign(arena, tokens) {
        Some(expr) => return Some(expr),
        None => (),
    }
    match try_parse_math(arena, tokens) {
        Some(expr) => return Some(expr),
        None => (),
    }
    match try_parse_immediate(arena, tokens) {
        Some(expr) => return Some(expr),
        None => (),
    }
    match try_parse_fn_call(arena, tokens) {
        Some(expr) => return Some(expr),
        None => (),
    }
    match try_parse_ident(arena, tokens) {
        Some(expr) => return Some(expr),
        None => (),
    }
    None
}

fn try_parse_string<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> Option<&'a Expr<'a, 'i>> {
    match tokens.peek(0) {
        Some(Token { span, typ: TokenType::DqString(s) }) => {
            drop(tokens.next());
            Some(arena.alloc(Expr::new(span, ExprType::String(s))))
        }
        _ => None,
    }
}

fn try_parse_ident<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> Option<&'a Expr<'a, 'i>> {
    match tokens.peek(0) {
        Some(Token { span, typ: TokenType::Ident(ident) }) => {
            drop(tokens.next());
            Some(arena.alloc(Expr::new(span, ExprType::Ident(ident))))
        },
        _ => None,
    }
}

fn try_parse_bind<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> Option<&'a Expr<'a, 'i>> {
    trace!("try_parse_bind");
    let mark = tokens.mark();
    let let_span = match tokens.next() {
        Some(Token { span, typ: TokenType::Let }) => span,
        _ => return None,
    };
    let is_mut = match tokens.peek(0) {
        Some(Token { typ: TokenType::Mut, .. }) => {
            drop(tokens.next());
            true
        }
        _ => false
    };
    let res = match try_parse_assign(arena, tokens) {
        Some(&Expr { span, typ: ExprType::Assign((ident, ident_span), expr) }) => {
            Some(&*arena.alloc(Expr::new(Span::new(let_span.start, span.end), ExprType::Bind((ident, ident_span), is_mut, expr))))
        }
        _ => return None,
    };
    mark.apply();
    res
}

fn try_parse_assign<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> Option<&'a Expr<'a, 'i>> {
    trace!("try_parse_assign");
    let mark = tokens.mark();
    let (ident, ident_span) = match tokens.next() {
        Some(Token { span, typ: TokenType::Ident(ident)}) => (ident, span),
        _ => return None,
    };
    // TODO: Type
    let assign_span = match tokens.next() {
        Some(Token { span, typ: TokenType::Assign }) => span,
        _ => return None,
    };
    let expr = try_parse_expr(arena, tokens)?;
    mark.apply();
    Some(arena.alloc(Expr::new(Span::new(ident_span.start, expr.span.end), ExprType::Assign((ident, ident_span), expr))))
}

fn try_parse_immediate<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> Option<&'a Expr<'a, 'i>> {
    trace!("try_parse_immediate");
    match tokens.peek(0)? {
        Token { span, typ: TokenType::Integer(i, _radix) } => {
            drop(tokens.next());
            Some(arena.alloc(Expr::new(span, ExprType::Integer(i))))
        }
        Token { span, typ: TokenType::Float(f) } => {
            drop(tokens.next());
            Some(arena.alloc(Expr::new(span, ExprType::Float(f))))
        }
        _ => None,
    }
}

fn try_parse_math<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> Option<&'a Expr<'a, 'i>> {
    trace!("try_parse_math");
    match tokens.peek(0)? {
        Token { span, typ: TokenType::Plus }
        | Token { span, typ: TokenType::Minus }
        | Token { span, typ: TokenType::Star }
        | Token { span, typ: TokenType::Slash } => {
            // consume the operator token
            let op = tokens.next().unwrap();
            let a = parse_expr(arena, tokens);
            let b = parse_expr(arena, tokens);
            match op.typ {
                TokenType::Plus => Some(arena.alloc(Expr::new(span, ExprType::Add(a, b)))),
                TokenType::Minus => Some(arena.alloc(Expr::new(span, ExprType::Sub(a, b)))),
                TokenType::Star => Some(arena.alloc(Expr::new(span, ExprType::Mul(a, b)))),
                TokenType::Slash => Some(arena.alloc(Expr::new(span, ExprType::Div(a, b)))),
                _ => unreachable!(),
            }
        },
        _ => None,
    }
}

fn try_parse_fn_call<'a, 'i>(arena: &'a Arena<Expr<'a, 'i>>, tokens: &mut Tokens<'i>) -> Option<&'a Expr<'a, 'i>> {
    trace!("try_parse_fn_call");
    match tokens.peek(0)? {
        Token { typ: TokenType::Ident(_), .. } => (),
        _ => return None,
    }
    match tokens.peek(1)? {
        Token { typ: TokenType::OpenParen, ..} => (),
        _ => return None,
    }

    // actually consume
    let ident = match tokens.next().unwrap() {
        Token { span, typ: TokenType::Ident(ident) } => (ident, span),
        _ => unreachable!(),
    };
    drop(tokens.next());
    let mut args = Vec::new();
    loop {
        match tokens.peek(0) {
            Some(Token { span, typ: TokenType::CloseParen }) => {
                drop(tokens.next());
                return Some(arena.alloc(Expr::new(Span::new(ident.1.start, span.end), ExprType::FunctionCall(ident, args))));
            },
            None => todo!("error handling"),
            _ => (),
        }
        args.push(parse_expr(arena, tokens));
        match tokens.peek(0) {
            Some(Token { span, typ: TokenType::Comma }) | Some(Token { span, typ: TokenType::CloseParen }) => {
                drop(tokens.next());
                return Some(arena.alloc(Expr::new(Span::new(ident.1.start, span.end), ExprType::FunctionCall(ident, args))));
            },
            _ => todo!("error handling"),
        }
    }
}
