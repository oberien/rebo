use std::collections::VecDeque;

use diagnostic::{Span, FileId, Diagnostics};

mod token;
mod lex_fns;

pub use token::*;
use std::rc::Rc;
use std::cell::RefCell;
use crate::ErrorCode;

#[derive(Debug)]
pub enum Error {
    /// The current token is handled by the current function, but it was incorrect in an
    /// unrecoverable way and a diagnostic was emitted. Abort lexing.
    Abort,
    /// An unexpected Eof was encountered. No diagnostic was emitted yet.
    UnexpectedEof(Span),
}

/// Pull-Lexer, lexing the next token only once it's requested
#[derive(Debug)]
pub struct Lexer<'i> {
    inner: Rc<RefCell<LexerInner<'i>>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LexerMode {
    // if lexing fails due to an unexpected character, print a diagnostic  and continue
    UnexpectedCharacterDiagnostic,
    // if lexing fails due to an unexpected character return EOF
    UnexpectedCharacterEof,
}

#[derive(Debug)]
pub struct LexerInner<'i> {
    mode: LexerMode,
    diagnostics: &'i Diagnostics<ErrorCode>,
    source: &'i str,
    file: FileId,
    /// index to continue lexing from
    lex_index: usize,
    /// already lexed and buffered tokens
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

impl<'i> Lexer<'i> {
    pub fn new(diagnostics: &'i Diagnostics<ErrorCode>, file: FileId) -> Lexer<'i> {
        let source = diagnostics.get_file(file);
        Self::new_in(diagnostics, file, 0, source.len(), LexerMode::UnexpectedCharacterDiagnostic)
    }
    pub fn new_in(diagnostics: &'i Diagnostics<ErrorCode>, file: FileId, from: usize, to: usize, mode: LexerMode) -> Lexer<'i> {
        Lexer {
            inner: Rc::new(RefCell::new(LexerInner {
                mode,
                diagnostics,
                file,
                source: &diagnostics.get_file(file)[..to],
                lex_index: from,
                tokens: VecDeque::new(),
                lookahead: Vec::new(),
            })),
        }
    }

    /// Marks the current position for automatic backtracking on drop. Call Mark::apply to prevent backtracking.
    pub fn mark(&self) -> Mark<'i> {
        let lookahead = self.inner.borrow().lookahead.last().cloned().unwrap_or_default();
        self.inner.borrow_mut().lookahead.push(lookahead);
        Mark {
            tokens: Rc::clone(&self.inner),
            done: false,
        }
    }

    pub fn next(&self) -> Result<Token<'i>, Error> {
        self.inner.borrow_mut().next()
    }
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.into_iter()
    }

    pub fn next_span(&self) -> Span {
        self.next_span_from(0)
    }
    pub fn next_span_from(&self, from: usize) -> Span {
        match self.inner.borrow_mut().peek(from) {
            Ok(token) => token.span(),
            Err(_) => {
                let file = self.inner.borrow().file;
                let index = self.inner.borrow().lex_index;
                Span::new(file, index, index)
            },
        }
    }
    pub fn last_span(&self) -> Span {
        let file = self.inner.borrow().file;
        let end = self.inner.borrow().source.len();
        Span::new(file, end, end)
    }

    pub fn peek(&self, index: usize) -> Result<Token<'i>, Error> {
        self.inner.borrow_mut().peek(index)
    }

    pub fn is_empty(&self) -> bool {
        match self.peek(0) {
            Ok(Token::Eof(_)) => true,
            Ok(_) => false,
            Err(_) => true,
        }
    }
}
impl<'i> LexerInner<'i> {
    fn get(&mut self, i: usize) -> Result<Token<'i>, Error> {
        while self.tokens.len() <= i {
            match lex_fns::lex_next(self.diagnostics, self.file, self.source, self.lex_index, self.mode)? {
                token @ Token::Eof(_) => {
                    self.tokens.push_back(token);
                }
                token => {
                    self.lex_index = token.span().end;
                    self.tokens.push_back(token);
                }
            }
        }
        Ok(self.tokens.get(i).unwrap().clone())
    }
    fn next(&mut self) -> Result<Token<'i>, Error> {
        match self.lookahead.last() {
            Some(lookahead) => {
                let idx = lookahead.peek;
                let res = self.get(idx)?;
                let lookahead = self.lookahead.last_mut().unwrap();
                lookahead.peek += 1;
                lookahead.consume = lookahead.consume.max(lookahead.peek);
                Ok(res)
            }
            None => {
                self.get(0)?;
                Ok(self.tokens.pop_front().unwrap())
            },
        }
    }

    fn peek(&mut self, index: usize) -> Result<Token<'i>, Error> {
        match self.lookahead.last() {
            Some(lookahead) => {
                let idx = lookahead.peek + index;
                self.get(idx)
            },
            None => self.get(index)
        }
    }
}

#[must_use]
pub struct Mark<'i> {
    tokens: Rc<RefCell<LexerInner<'i>>>,
    done: bool,
}

impl Mark<'_> {
    pub fn apply(mut self) {
        let mut tokens = self.tokens.borrow_mut();
        let lookahead = tokens.lookahead.pop().unwrap();
        match tokens.lookahead.last_mut() {
            Some(l) => {
                l.peek = lookahead.peek;
                l.consume = lookahead.consume;
            },
            None => for _ in 0..lookahead.consume {
                let _token = tokens.tokens.pop_front();
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

pub struct TokenIter<'b, 'i> {
    lexer: &'b Lexer<'i>,
    index: usize,
    eof: bool,
}
impl<'b, 'i> IntoIterator for &'b Lexer<'i> {
    type Item = Token<'i>;
    type IntoIter = TokenIter<'b, 'i>;

    fn into_iter(self) -> Self::IntoIter {
        TokenIter { lexer: self, index: 0, eof: false }
    }
}
impl<'b, 'i> Iterator for TokenIter<'b, 'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            return None;
        }
        let res = match self.lexer.peek(self.index) {
            Ok(token @ Token::Eof(_)) => {
                self.eof = true;
                token
            }
            Ok(token) => token,
            Err(_) => return None,
        };
        self.index += 1;
        Some(res)
    }
}
