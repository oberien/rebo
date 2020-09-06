use std::fmt;
use std::collections::VecDeque;
use std::cell::RefCell;
use std::rc::Rc;

use crate::diagnostics::Span;

#[derive(Debug)]
pub struct Tokens<'i> {
    inner: Rc<RefCell<TokensInner<'i>>>,
}

#[derive(Debug)]
pub struct TokensInner<'i> {
    tokens: VecDeque<Token<'i>>,
    lookahead: Vec<Lookahead>,
}

#[derive(Debug, Default, Clone, Copy)]
struct Lookahead {
    /// How much we have peeked ahead already.
    peek: usize,
    /// How much must be consumed. This value is >= peek. It is increased when a mark deeper in
    /// the stack should clean up, to mark those elements as possibly consumed if the mark isn't
    /// backtracked.
    consume: usize,
}

#[derive(Debug, Clone)]
pub struct Token<'i> {
    pub span: Span,
    pub typ: TokenType<'i>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType<'i> {
    Ident(&'i str),
    DqString(String),
    Integer(i64, Radix),
    Float(f64, Radix),
    Let,
    Mut,
    // =
    Assign,
    // ==
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    OpenParen,
    CloseParen,
    Comma,
    Semicolon,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Radix {
    Bin,
    Dec,
    Hex,
}

impl Radix {
    pub fn to_u8(self) -> u8 {
        match self {
            Radix::Bin => 2,
            Radix::Dec => 10,
            Radix::Hex => 16,
        }
    }
}

impl<'i> Tokens<'i> {
    pub(super) fn new(tokens: VecDeque<Token<'i>>) -> Tokens<'i> {
        Tokens {
            inner: Rc::new(RefCell::new(TokensInner {
                tokens,
                lookahead: Vec::new(),
            }))
        }
    }

    /// Marks the current position for automatic backtracking on drop. Call Mark::apply to prevent backtracking.
    pub fn mark(&self) -> Mark<'i> {
        let lookahead = self.inner.borrow().lookahead.last().cloned().unwrap_or_default();
        trace!("marking at {:?}", lookahead);
        self.inner.borrow_mut().lookahead.push(lookahead);
        Mark {
            tokens: Rc::clone(&self.inner),
            done: false,
        }
    }

    pub fn next(&mut self) -> Option<Token<'i>> {
        let res = self.inner.borrow_mut().next();
        trace!("taking token {:?}", res);
        res
    }

    pub fn peek(&self, index: usize) -> Option<Token<'i>> {
        self.inner.borrow().peek(index)
    }

    pub fn is_empty(&self) -> bool {
        self.inner.borrow().is_empty()
    }
}

impl<'i> TokensInner<'i> {
    fn next(&mut self) -> Option<Token<'i>> {
        match self.lookahead.last_mut() {
            Some(lookahead) => {
                let res = self.tokens.get(lookahead.peek).cloned();
                lookahead.peek += 1;
                lookahead.consume = lookahead.consume.max(lookahead.peek);
                res
            }
            None => self.tokens.pop_front(),
        }
    }

    fn peek(&self, index: usize) -> Option<Token<'i>> {
        match self.lookahead.last() {
            Some(lookahead) => self.tokens.get(lookahead.peek + index).cloned(),
            None => self.tokens.get(index).cloned()
        }
    }

    fn is_empty(&self) -> bool {
        match self.lookahead.last() {
            Some(lookahead) => self.tokens.len() >= lookahead.peek,
            None => self.tokens.is_empty()
        }
    }
}

pub struct Mark<'i> {
    tokens: Rc<RefCell<TokensInner<'i>>>,
    done: bool,
}

impl Mark<'_> {
    pub fn apply(mut self) {
        let mut tokens = self.tokens.borrow_mut();
        let mut lookahead = tokens.lookahead.pop().unwrap();
        match tokens.lookahead.last_mut() {
            Some(l) => {
                trace!("updating consume from {} to {}", l.consume, lookahead.consume);
                l.consume = lookahead.consume;
            },
            None => for _ in 0..lookahead.consume {
                let token = tokens.tokens.pop_front();
                trace!("consuming {:?}", token);
            }
        }
        self.done = true;
    }
}

impl Drop for Mark<'_> {
    fn drop(&mut self) {
        if !self.done {
            self.tokens.borrow_mut().lookahead.pop();
        }
    }
}

impl<'i> Token<'i> {
    pub fn new(span: Span, typ: TokenType<'i>) -> Token<'i> {
        Token { span, typ }
    }
}

impl<'i> fmt::Display for Tokens<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for token in &self.inner.borrow().tokens {
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

impl<'i> fmt::Display for Token<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl<'i> fmt::Display for TokenType<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Ident(name) => write!(f, "{} ", name),
            TokenType::DqString(s) => write!(f, "{:?} ", s),
            &TokenType::Integer(i, radix) => write!(f, "{} ", lexical::to_string_radix(i, radix.to_u8())),
            &TokenType::Float(fl, radix) => write!(f, "{} ", lexical::to_string_radix(fl, radix.to_u8())),
            TokenType::Let => write!(f, "let "),
            TokenType::Mut => write!(f, "mut "),
            TokenType::Assign => write!(f, "= "),
            TokenType::Equals => write!(f, "== "),
            TokenType::Plus => write!(f, "+ "),
            TokenType::Minus => write!(f, "- "),
            TokenType::Star => write!(f, "* "),
            TokenType::Slash => write!(f, "/ "),
            TokenType::OpenParen => write!(f, "( "),
            TokenType::CloseParen => write!(f, ") "),
            TokenType::Comma => write!(f, ", "),
            TokenType::Semicolon => writeln!(f, ";"),
            TokenType::Eof => Ok(()),
        }
    }
}
