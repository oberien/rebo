use std::fmt;
use std::collections::VecDeque;
use std::cell::RefCell;
use std::rc::Rc;

use diagnostic::{Span, FileId};
use derive_more::Display;

macro_rules! gen_tokens {
    ($(
        $name:ident$(<$lt:lifetime>)?, $tokenname:ident, $repr:literal, {$($field:ident : $field_ty:ty,)*}, (fmt = $fmt:literal $(, $fmtarg:expr)*) $(, ($($copy:ident),+))? $(, #[$comment:meta])?;
    )+) => {
        #[derive(Debug, Clone, PartialEq, Display, rebo_derive::Functions)]
        pub enum Token<'i> {
            $(
                $(#[$comment])?
                #[doc = $repr]
                $name($tokenname $(<$lt>)?),
            )+
        }
        impl<'i> Token<'i> {
            pub fn typ(&self) -> TokenType {
                match self {
                    $(
                        Token::$name($tokenname { .. }) => TokenType::$name,
                    )+
                }
            }
            pub fn span(&self) -> Span {
                match self {
                    $(
                        Token::$name($tokenname { span, .. }) => *span,
                    )+
                }
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
        pub enum TokenType {
            $(
                $(#[$comment])? #[doc = $repr] $name,
            )+
        }
        impl TokenType {
            pub fn as_str(self) -> &'static str {
                match self {
                    $(
                        TokenType::$name => $repr,
                    )+
                }
            }
        }
        impl ::std::fmt::Display for TokenType {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "{}", self.as_str())
            }
        }

        $(
            #[derive(Debug, Clone, $($($copy,)+)? PartialEq, Display)]
            #[display(fmt = $fmt $(, $fmtarg)*)]
            $(#[$comment])?
            #[doc = $repr]
            pub struct $tokenname $(<$lt>)? {
                pub span: Span,
                $(
                    pub $field: $field_ty,
                )*
            }
        )+
    }
}

gen_tokens! {
    // primitives
    Ident<'i>, TokenIdent, "ident", {ident: &'i str,}, (fmt = "{} ", ident), (Copy, Eq, Hash);
    DqString, TokenDqString, "double-quoted string", {string: String,}, (fmt = "{:?} ", string), (Eq, Hash);
    Integer, TokenInteger, "integer value", {value: i64, radix: Radix,}, (fmt = "{} ", "lexical::to_string_radix(*value, radix.to_u8())"), (Copy, Eq, Hash);
    Float, TokenFloat, "float value", {value: f64, radix: Radix,}, (fmt = "{} ", "lexical::to_string_radix(*value, radix.to_u8())"), (Copy);
    Bool, TokenBool, "bool value", {value: bool,}, (fmt = "{} ", value), (Copy, Eq, Hash);
    // keywords
    Let, TokenLet, "let", {}, (fmt = "let "), (Copy, Eq, Hash);
    Mut, TokenMut, "mut", {}, (fmt = "mut "), (Copy, Eq, Hash);
    Fn, TokenFn, "fn", {}, (fmt = "fn "), (Copy, Eq, Hash);
    // types
    StringType, TokenStringType, "string", {}, (fmt = "string "), (Copy, Eq, Hash);
    IntType, TokenIntType, "int", {}, (fmt = "int "), (Copy, Eq, Hash);
    FloatType, TokenFloatType, "float", {}, (fmt = "float "), (Copy, Eq, Hash);
    BoolType, TokenBoolType, "bool", {}, (fmt = "bool "), (Copy, Eq, Hash);
    // symbols
    Assign, TokenAssign, "=", {}, (fmt = "= "), (Copy, Eq, Hash);
    LessThan, TokenLessThan, "<", {}, (fmt = "< "), (Copy, Eq, Hash);
    LessEquals, TokenLessEquals, "<=", {}, (fmt = "<= "), (Copy, Eq, Hash);
    Equals, TokenEquals, "==", {}, (fmt = "== "), (Copy, Eq, Hash);
    NotEquals, TokenNotEquals, "!=", {}, (fmt = "!= "), (Copy, Eq, Hash);
    FuzzyEquals, TokenFuzzyEquals, "~~", {}, (fmt = "~~ "), (Copy, Eq, Hash);
    FuzzyNotEquals, TokenFuzzyNotEquals, "!~", {}, (fmt = "!~ "), (Copy, Eq, Hash);
    GreaterEquals, TokenGreaterEquals, ">=", {}, (fmt = ">= "), (Copy, Eq, Hash);
    GreaterThan, TokenGreaterThan, ">", {}, (fmt = "> "), (Copy, Eq, Hash);
    Plus, TokenPlus, "+", {}, (fmt = "+ "), (Copy, Eq, Hash);
    Minus, TokenMinus, "-", {}, (fmt = "- "), (Copy, Eq, Hash);
    Star, TokenStar, "*", {}, (fmt = "* "), (Copy, Eq, Hash);
    Slash, TokenSlash, "/", {}, (fmt = "/ "), (Copy, Eq, Hash);
    Bang, TokenBang, "!", {}, (fmt = "! "), (Copy, Eq, Hash);
    Amp, TokenAmp, "&", {}, (fmt = "& "), (Copy, Eq, Hash);
    DoubleAmp, TokenDoubleAmp, "&&", {}, (fmt = "&& "), (Copy, Eq, Hash);
    Pipe, TokenPipe, "|", {}, (fmt = "| "), (Copy, Eq, Hash);
    DoublePipe, TokenDoublePipe, "||", {}, (fmt = "|| "), (Copy, Eq, Hash);
    OpenParen, TokenOpenParen, "(", {}, (fmt = "( "), (Copy, Eq, Hash);
    CloseParen, TokenCloseParen, ")", {}, (fmt = ") "), (Copy, Eq, Hash);
    OpenCurly, TokenOpenCurly, "{", {}, (fmt = "{{\n"), (Copy, Eq, Hash);
    CloseCurly, TokenCloseCurly, "}", {}, (fmt = "}}\n"), (Copy, Eq, Hash);
    Comma, TokenComma, ",", {}, (fmt = ", "), (Copy, Eq, Hash);
    Semicolon, TokenSemicolon, ";", {}, (fmt = ";\n"), (Copy, Eq, Hash);
    Colon, TokenColon, ":", {}, (fmt = ": "), (Copy, Eq, Hash);
    Arrow, TokenArrow, "->", {}, (fmt = "-> "), (Copy, Eq, Hash);
    LineComment<'i>, TokenLineComment, "//", {comment: &'i str,}, (fmt = "{}", comment), (Copy, Eq, Hash), #[doc = "Line Comment including starting `//` and ending newline"];
    BlockComment<'i>, TokenBlockComment, "/* */", {comment: &'i str,}, (fmt = "{}", comment), (Copy, Eq, Hash), #[doc = "/// Block Comment including starting `/*` and ending `*/`"];
    Eof, TokenEof, "EOF", {}, (fmt = "EOF");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug)]
pub struct Tokens<'i> {
    file: FileId,
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

impl<'i> Tokens<'i> {
    pub(super) fn new(file: FileId, tokens: VecDeque<Token<'i>>) -> Tokens<'i> {
        Tokens {
            file,
            inner: Rc::new(RefCell::new(TokensInner {
                tokens,
                lookahead: Vec::new(),
            })),
        }
    }

    /// Marks the current position for automatic backtracking on drop. Call Mark::apply to prevent backtracking.
    pub fn mark(&self) -> Mark<'i> {
        let lookahead = self.inner.borrow().lookahead.last().cloned().unwrap_or_default();
        // trace!("marking at {:?}", lookahead);
        self.inner.borrow_mut().lookahead.push(lookahead);
        Mark {
            tokens: Rc::clone(&self.inner),
            done: false,
        }
    }

    pub fn next(&mut self) -> Option<Token<'i>> {
        let res = self.inner.borrow_mut().next();
        // trace!("taking token {:?}", res);
        res
    }
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        (&self).into_iter()
    }

    pub fn next_span(&self) -> Span {
        self.next_span_from(0)
    }
    pub fn next_span_from(&self, from: usize) -> Span {
        match self.inner.borrow().peek(from) {
            Some(token) => token.span(),
            None => self.last_span(),
        }
    }
    pub fn last_span(&self) -> Span {
        self.inner.borrow().tokens.back()
            .map(|t| t.span())
            .unwrap_or_else(|| Span::new(self.file, 0, 0))
    }

    pub fn peek(&self, index: usize) -> Option<Token<'i>> {
        self.inner.borrow().peek(index)
    }

    pub fn is_empty(&self) -> bool {
        self.inner.borrow().is_empty()
    }
}
pub struct TokenIter<'b, 'i> {
    tokens: &'b Tokens<'i>,
    index: usize,
}
impl<'b, 'i> IntoIterator for &'b Tokens<'i> {
    type Item = Token<'i>;
    type IntoIter = TokenIter<'b, 'i>;

    fn into_iter(self) -> Self::IntoIter {
        TokenIter { tokens: self, index: 0 }
    }
}
impl<'b, 'i> Iterator for TokenIter<'b, 'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.tokens.peek(self.index);
        self.index += 1;
        res
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
            Some(lookahead) => self.tokens.len() <= lookahead.peek,
            None => self.tokens.is_empty()
        }
    }
}

#[must_use]
pub struct Mark<'i> {
    tokens: Rc<RefCell<TokensInner<'i>>>,
    done: bool,
}

impl Mark<'_> {
    pub fn apply(mut self) {
        let mut tokens = self.tokens.borrow_mut();
        let lookahead = tokens.lookahead.pop().unwrap();
        match tokens.lookahead.last_mut() {
            Some(l) => {
                // trace!("updating peek from {} to {}", l.peek, lookahead.peek);
                l.peek = lookahead.peek;
                // trace!("updating consume from {} to {}", l.consume, lookahead.consume);
                l.consume = lookahead.consume;
            },
            None => for _ in 0..lookahead.consume {
                let _token = tokens.tokens.pop_front();
                // trace!("consuming {:?}", token);
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

impl<'i> fmt::Display for Tokens<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for token in &self.inner.borrow().tokens {
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}
