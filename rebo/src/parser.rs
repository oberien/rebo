use std::fmt;
use std::collections::HashMap;

use typed_arena::Arena;

use crate::lexer::{Tokens, Token, TokenType};
use crate::diagnostics::Span;
use crate::scope::BindingId;

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

#[derive(Debug, Clone)]
pub struct Binding<'i> {
    pub id: BindingId,
    pub ident: &'i str,
    pub mutable: bool,
    /// Span of the original declaration binding
    pub span: Span,
}

#[derive(Debug)]
pub struct Expr<'a, 'i> {
    pub span: Span,
    pub typ: ExprType<'a, 'i>,
}

#[derive(Debug)]
pub enum ExprType<'a, 'i> {
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

pub struct Parser<'a, 'i, 'r> {
    /// arena to allocate expressions into
    arena: &'a Arena<Expr<'a, 'i>>,
    /// tokens to be consumed
    tokens: Tokens<'i>,
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
    pub fn new(arena: &'a Arena<Expr<'a, 'i>>, tokens: Tokens<'i>, root_scope_binding_ids: &'r HashMap<&'static str, BindingId>) -> Self {
        let mut parser = Parser {
            arena,
            tokens,
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
            });
        }
        parser
    }

    fn add_binding(&mut self, ident: &'i str, mutable: bool, span: Span) -> Binding<'i> {
        let id = BindingId::new();
        let binding = Binding { id, ident, mutable, span };
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

    pub fn parse(mut self) -> Ast<'a, 'i> {
        trace!("parse");
        // file scope
        self.push_scope();
        let mut exprs = Vec::new();
        let mut last_expr = false;
        while !self.tokens.is_empty() && self.tokens.peek(0).unwrap().typ != TokenType::Eof {
            let expr = self.parse_expr_or_stmt();
            match expr.typ {
                ExprType::Statement(_) => (),
                _ => {
                    if last_expr {
                        todo!("error handling: semicolon required for all except the last expression");
                    }
                    last_expr = true;
                },
            }
            exprs.push(expr);
        }
        // file scope
        self.pop_scope();
        Ast {
            exprs,
            bindings: self.bindings,
        }
    }

    fn parse_expr_or_stmt(&mut self) -> &'a Expr<'a, 'i> {
        trace!("parse_expr_or_stmt");
        let expr = self.parse_expr();
        match self.tokens.peek(0) {
            Some(Token { span, typ: TokenType::Semicolon }) => {
                drop(self.tokens.next());
                self.arena.alloc(Expr::new(Span::new(span.file, expr.span.start, span.end), ExprType::Statement(expr)))
            }
            _ => expr
        }
    }

    fn parse_expr(&mut self) -> &'a Expr<'a, 'i> {
        trace!("parse_expr");
        let res = match self.try_parse_expr() {
            Some(expr) => expr,
            None => todo!("error handling"),
        };
        trace!("parsed expr {:?}", res);
        res
    }

    fn try_parse_expr(&mut self) -> Option<&'a Expr<'a, 'i>> {
        trace!("try_parse_expr");
        match self.try_parse_string() {
            Some(expr) => return Some(expr),
            None => (),
        }
        match self.try_parse_bind() {
            Some(expr) => return Some(expr),
            None => (),
        }
        match self.try_parse_assign(CreateBinding::No) {
            Some(expr) => return Some(expr),
            None => (),
        }
        match self.try_parse_math() {
            Some(expr) => return Some(expr),
            None => (),
        }
        match self.try_parse_immediate() {
            Some(expr) => return Some(expr),
            None => (),
        }
        match self.try_parse_fn_call() {
            Some(expr) => return Some(expr),
            None => (),
        }
        match self.try_parse_ident() {
            Some(expr) => return Some(expr),
            None => (),
        }
        None
    }

    fn try_parse_string(&mut self) -> Option<&'a Expr<'a, 'i>> {
        match self.tokens.peek(0) {
            Some(Token { span, typ: TokenType::DqString(s) }) => {
                drop(self.tokens.next());
                Some(self.arena.alloc(Expr::new(span, ExprType::String(s))))
            }
            _ => None,
        }
    }

    fn try_parse_ident(&mut self) -> Option<&'a Expr<'a, 'i>> {
        match self.tokens.peek(0) {
            Some(Token { span, typ: TokenType::Ident(ident) }) => {
                drop(self.tokens.next());
                let binding = match self.get_binding(ident) {
                    Some(binding) => binding,
                    None => todo!("error handling"),
                };
                Some(self.arena.alloc(Expr::new(span, ExprType::Variable((binding, span)))))
            },
            _ => None,
        }
    }

    fn try_parse_bind(&mut self) -> Option<&'a Expr<'a, 'i>> {
        trace!("try_parse_bind");
        let mark = self.tokens.mark();
        let let_span = match self.tokens.next() {
            Some(Token { span, typ: TokenType::Let }) => span,
            _ => return None,
        };
        let is_mut = match self.tokens.peek(0) {
            Some(Token { typ: TokenType::Mut, .. }) => {
                drop(self.tokens.next());
                true
            }
            _ => false
        };
        let res = match self.try_parse_assign(CreateBinding::Yes(is_mut)) {
            Some(Expr { span, typ: ExprType::Assign((binding, _ident_span), expr) }) => {
                let span = Span::new(span.file, let_span.start, span.end);
                Some(&*self.arena.alloc(Expr::new(span, ExprType::Bind(binding.clone(), expr))))
            }
            _ => return None,
        };
        mark.apply();
        res
    }

    fn try_parse_assign(&mut self, create_binding: CreateBinding) -> Option<&'a Expr<'a, 'i>> {
        trace!("try_parse_assign");
        let mark = self.tokens.mark();
        let (ident, ident_span) = match self.tokens.next() {
            Some(Token { span, typ: TokenType::Ident(ident)}) => (ident, span),
            _ => return None,
        };
        // TODO: Type
        match self.tokens.next() {
            Some(Token { span: _, typ: TokenType::Assign }) => (),
            _ => return None,
        };
        let expr = self.try_parse_expr()?;
        mark.apply();

        let binding = match create_binding {
            CreateBinding::Yes(mutable) => self.add_binding(ident, mutable, ident_span),
            CreateBinding::No => match self.get_binding(ident) {
                Some(binding) => binding,
                None => todo!("error handling"),
            }
        };

        Some(self.arena.alloc(Expr::new(Span::new(expr.span.file, ident_span.start, expr.span.end), ExprType::Assign((binding, ident_span), expr))))
    }

    fn try_parse_immediate(&mut self) -> Option<&'a Expr<'a, 'i>> {
        trace!("try_parse_immediate");
        match self.tokens.peek(0)? {
            Token { span, typ: TokenType::Integer(i, _radix) } => {
                drop(self.tokens.next());
                Some(self.arena.alloc(Expr::new(span, ExprType::Integer(i))))
            }
            Token { span, typ: TokenType::Float(f, _radix) } => {
                drop(self.tokens.next());
                Some(self.arena.alloc(Expr::new(span, ExprType::Float(f))))
            }
            _ => None,
        }
    }

    fn try_parse_math(&mut self) -> Option<&'a Expr<'a, 'i>> {
        trace!("try_parse_math");
        match self.tokens.peek(0)? {
            Token { span, typ: TokenType::Plus }
            | Token { span, typ: TokenType::Minus }
            | Token { span, typ: TokenType::Star }
            | Token { span, typ: TokenType::Slash } => {
                // consume the operator token
                let op = self.tokens.next().unwrap();
                let a = self.parse_expr();
                let b = self.parse_expr();
                match op.typ {
                    TokenType::Plus => Some(self.arena.alloc(Expr::new(span, ExprType::Add(a, b)))),
                    TokenType::Minus => Some(self.arena.alloc(Expr::new(span, ExprType::Sub(a, b)))),
                    TokenType::Star => Some(self.arena.alloc(Expr::new(span, ExprType::Mul(a, b)))),
                    TokenType::Slash => Some(self.arena.alloc(Expr::new(span, ExprType::Div(a, b)))),
                    _ => unreachable!(),
                }
            },
            _ => None,
        }
    }

    fn try_parse_fn_call(&mut self) -> Option<&'a Expr<'a, 'i>> {
        trace!("try_parse_fn_call");
        match self.tokens.peek(0)? {
            Token { typ: TokenType::Ident(_), .. } => (),
            _ => return None,
        }
        match self.tokens.peek(1)? {
            Token { typ: TokenType::OpenParen, ..} => (),
            _ => return None,
        }

        // actually consume
        let (ident, ident_span) = match self.tokens.next().unwrap() {
            Token { span, typ: TokenType::Ident(ident) } => (ident, span),
            _ => unreachable!(),
        };
        let binding = match self.get_binding(ident) {
            Some(binding) => binding,
            None => todo!("error handling"),
        };
        drop(self.tokens.next());
        let mut args = Vec::new();
        loop {
            match self.tokens.peek(0) {
                Some(Token { span, typ: TokenType::CloseParen }) => {
                    drop(self.tokens.next());
                    return Some(self.arena.alloc(Expr::new(Span::new(span.file, ident_span.start, span.end), ExprType::FunctionCall((binding, ident_span), args))));
                },
                None => todo!("error handling"),
                _ => (),
            }
            args.push(self.parse_expr());
            match self.tokens.peek(0) {
                Some(Token { span, typ: TokenType::Comma }) | Some(Token { span, typ: TokenType::CloseParen }) => {
                    drop(self.tokens.next());
                    return Some(self.arena.alloc(Expr::new(Span::new(span.file, ident_span.start, span.end), ExprType::FunctionCall((binding, ident_span), args))));
                },
                _ => todo!("error handling"),
            }
        }
    }
}
