use crate::parser::{Expr, InternalError, Parser};
use std::fmt::{self, Display, Formatter};
use std::iter::FromIterator;
use crate::lexer::*;
use diagnostic::Span;
use std::marker::PhantomData;
use std::sync::LazyLock;
use crate::common::Depth;
use regex::Regex;
use crate::common::Spanned;
use crate::parser::scope::ScopeType;

pub trait Parse<'i>: Sized {
    fn parse(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        static REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"\w*::[a-zA-Z0-9_:]*::"#).unwrap());
        let _guard = tracing::info_span!(
            "parse",
            type = %REGEX.replace_all(::std::any::type_name::<Self>(), ""),
            token = ?parser.peek_token(0),
        ).entered();
        let mark = parser.lexer.mark();
        let res = Self::parse_marked(parser, depth)?;
        mark.apply();
        Ok(res)
    }
    /// Parse the element, resetting the tokens to the previous state even on success
    fn parse_reset(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        let mark = parser.lexer.mark();
        let res = Self::parse(parser, depth);
        drop(mark);
        res
    }
    fn parse_scoped(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        let scope_guard = parser.push_scope(ScopeType::Synthetic);
        let res = Self::parse(parser, depth);
        drop(scope_guard);
        res
    }
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError>;
}
impl<'i, T: Parse<'i>> Parse<'i> for Option<T> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        match parser.parse(depth.last()) {
            Ok(t) => Ok(Some(t)),
            Err(_) => Ok(None),
        }
    }
}
impl<'i, T: Parse<'i>> Parse<'i> for Vec<T> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        let mut vec = Vec::new();
        while let Ok(t) = parser.parse(depth.next()) {
            vec.push(t);
        }
        Ok(vec)
    }
}
impl<'i, T: Parse<'i>> Parse<'i> for Box<T> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(Box::new(parser.parse(depth)?))
    }
}

#[derive(Debug, Clone)]
pub struct Scoped<T>(pub T);
impl<'i, T: Parse<'i>> Parse<'i> for Scoped<T> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(Scoped(parser.parse_scoped(depth)?))
    }
}


#[derive(Debug, Clone)]
pub struct Separated<'i, T: 'i, D: 'i> {
    inner: Vec<(T, D)>,
    last: Option<T>,
    marker: PhantomData<&'i Expr<'i>>,
}

impl<'i, T, D> Default for Separated<'i, T, D> {
    fn default() -> Self {
        Separated {
            inner: Vec::new(),
            last: None,
            marker: PhantomData,
        }
    }
}

impl<'i, T: Parse<'i>, D: Parse<'i>> Parse<'i> for Separated<'i, T, D> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(Separated {
            inner: parser.parse(depth.next())?,
            last: parser.parse(depth.last())?,
            marker: PhantomData,
        })
    }
}
impl<'i, T, D> Separated<'i, T, D> {
    pub fn from<U>(other: Separated<'i, U, D>) -> Self where T: From<U> {
        Separated {
            inner: other.inner.into_iter().map(|(u, d)| (T::from(u), d)).collect(),
            last: other.last.map(T::from),
            marker: other.marker,
        }
    }
}
impl<'i: 'b, 'b, T: 'i, D: 'i> Separated<'i, T, D> {
    pub fn len(&self) -> usize {
        self.inner.len() + (self.last.is_some() as usize)
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn iter(&'b self) -> <&'b Self as IntoIterator>::IntoIter {
        self.into_iter()
    }
    pub fn iter_mut(&'b mut self) -> <&'b mut Self as IntoIterator>::IntoIter {
        self.into_iter()
    }
    pub fn iter_with_delimiters(&'b self) -> Box<dyn Iterator<Item = (&'b T, Option<&'b D>)> + 'b> {
        Box::new(self.inner.iter().map(|(t, d)| (t, Some(d))).chain(self.last.as_ref().map(|t| (t, None))))
    }
    pub fn last_unterminated(&'b self) -> Option<&'b T> {
        self.last.as_ref()
    }
    pub fn last_terminated(&'b self) -> Option<&'b T> {
        self.inner.last().map(|(t, _d)| t)
    }
    pub fn last(&'b self) -> Option<&'b T> {
        self.last_unterminated().or_else(|| self.last_terminated())
    }
    pub fn is_terminated(&self) -> bool {
        self.last.is_none()
    }
    /// Prepend an element to the separated list. If it's the only element in the list, the delimiter
    /// can be None, otherwise it panics if delimiter is None.
    pub fn push_front(&mut self, element: T, delimiter: Option<D>) {
        match delimiter {
            Some(delimiter) => self.inner.insert(0, (element, delimiter)),
            None => {
                assert!(self.inner.is_empty() && self.last.is_none());
                self.last = Some(element);
            }
        }
    }
    /// Builder pattern for push_front
    pub fn prepend(mut self, element: T, delimiter: Option<D>) -> Self {
        self.push_front(element, delimiter);
        self
    }
    /// Append an element to the separated list. If it's the only element in the list, the delimiter
    /// can be None, otherwise it panics if delimiter is None.
    pub fn push_back(&mut self, delimiter: Option<D>, element: T) {
        if let Some(last) = self.last.take() {
            self.inner.push((last, delimiter.unwrap()));
        }
        self.last = Some(element);
    }
    /// Builder pattern for push_back
    pub fn append(mut self, delimiter: Option<D>, element: T) -> Self {
        self.push_back(delimiter, element);
        self
    }
}
impl<'i, T: Spanned + 'i, D: 'i> Separated<'i, T, D> {
    pub fn diagnostics_span(&self) -> Option<Span> {
        let first = self.iter().next();
        let last = self.last.as_ref().or_else(|| self.inner.iter().last().map(|(t, _d)| t));
        match (first, last) {
            (Some(first), None) => Some(first.diagnostics_span()),
            (None, Some(last)) => Some(last.diagnostics_span()),
            (Some(first), Some(last)) => Some((first.span_with_id() | last.span_with_id()).diagnostics_span()),
            (None, None) => None,
        }
    }
}
impl<'i, T: 'i, D: 'i> IntoIterator for Separated<'i, T, D> {
    type Item = T;
    type IntoIter = Box<dyn Iterator<Item = T> + 'i>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.inner.into_iter().map(|(t, _)| t).chain(self.last))
    }
}
impl<'b, 'i: 'b, T: 'i, D: 'i> IntoIterator for &'b Separated<'i, T, D> {
    type Item = &'b T;
    type IntoIter = Box<dyn Iterator<Item = &'b T> + 'b>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.inner.iter().map(|(t, _)| t).chain(&self.last))
    }
}
impl<'b, 'i: 'b, T: 'i, D: 'i> IntoIterator for &'b mut Separated<'i, T, D> {
    type Item = &'b mut T;
    type IntoIter = Box<dyn Iterator<Item = &'b mut T> + 'b>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.inner.iter_mut().map(|(t, _)| t).chain(&mut self.last))
    }
}
impl<'i, T: 'i, D: 'i> Extend<(D, T)> for Separated<'i, T, D> {
    fn extend<I: IntoIterator<Item=(D, T)>>(&mut self, iter: I) {
        for (delim, element) in iter {
            self.push_back(Some(delim), element)
        }
    }
}
impl<'i, T: 'i, D: 'i> FromIterator<(D, T)> for Separated<'i, T, D> {
    fn from_iter<I: IntoIterator<Item=(D, T)>>(iter: I) -> Self {
        let mut sep = Separated::default();
        sep.extend(iter);
        sep
    }
}
impl<'i, T: Display, D: Display> Display for Separated<'i, T, D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut joined: String = self.inner.iter()
            .map(|(t, d)| format!("{}{}", t, d))
            .chain(self.last.iter().map(|t| t.to_string()))
            .collect();
        if joined.ends_with(' ') {
            joined.pop();
        }
        write!(f, "{}", joined)
    }
}

macro_rules! impl_parse_for_tuples {
    ($last:ident $(,$name:ident)*) => {
        impl<'i, $($name: Parse<'i>,)* $last: Parse<'i>> Parse<'i> for ($($name,)* $last,) {
            fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
                Ok(($($name::parse(parser, depth.next())?,)* $last::parse(parser, depth.last())?,))
            }
        }
    }
}
impl_parse_for_tuples!(A);
impl_parse_for_tuples!(A, B);
impl_parse_for_tuples!(A, B, C);
impl_parse_for_tuples!(A, B, C, D);
impl_parse_for_tuples!(A, B, C, D, E);
impl_parse_for_tuples!(A, B, C, D, E, F);


macro_rules! impl_parse_for_tokens {
    ($($name:ident$(<$lt:lifetime>)?, $tokenname:ident;)+) => {
        $(
            impl<'i> crate::parser::Parse<'i> for $tokenname $(<$lt>)? {
                fn parse_marked(parser: &mut crate::parser::Parser<'i, '_>, _depth: Depth) -> Result<Self, crate::parser::InternalError> {
                    match parser.peek_token(0)? {
                        crate::lexer::Token::$name(t) => {
                            drop(parser.next_token());
                            Ok(t)
                        },
                        _ => Err(crate::parser::InternalError::Backtrack(crate::parser::Backtrack {
                            span: parser.lexer.next_span(),
                            expected: ::std::borrow::Cow::Borrowed(&[crate::parser::Expected::Token(crate::lexer::TokenType::$name)]),
                        })),
                    }
                }
            }
        )+
    }
}

impl_parse_for_tokens! {
    // primitives
    Ident<'i>, TokenIdent;
    DqString, TokenDqString;
    FormatString<'i>, TokenFormatString;
    Integer, TokenInteger;
    Float, TokenFloat;
    Bool, TokenBool;
    // keywords
    Let, TokenLet;
    Mut, TokenMut;
    Struct, TokenStruct;
    Enum, TokenEnum;
    Impl, TokenImpl;
    Fn, TokenFn;
    Gen, TokenGen;
    Match, TokenMatch;
    If, TokenIf;
    Else, TokenElse;
    While, TokenWhile;
    For, TokenFor;
    Loop, TokenLoop;
    Break, TokenBreak;
    Continue, TokenContinue;
    Return, TokenReturn;
    Yield, TokenYield;
    In, TokenIn;
    Static, TokenStatic;
    Include, TokenInclude;
    // types
    StringType, TokenStringType;
    IntType, TokenIntType;
    FloatType, TokenFloatType;
    BoolType, TokenBoolType;
    // symbols
    Assign, TokenAssign;
    LessThan, TokenLessThan;
    LessEquals, TokenLessEquals;
    Equals, TokenEquals;
    NotEquals, TokenNotEquals;
    GreaterEquals, TokenGreaterEquals;
    GreaterThan, TokenGreaterThan;
    Plus, TokenPlus;
    Minus, TokenMinus;
    Star, TokenStar;
    Slash, TokenSlash;
    Percent, TokenPercent;
    Circumflex, TokenCircumflex;
    Bang, TokenBang;
    Amp, TokenAmp;
    DoubleAmp, TokenDoubleAmp;
    Pipe, TokenPipe;
    DoublePipe, TokenDoublePipe;
    OpenParen, TokenOpenParen;
    CloseParen, TokenCloseParen;
    OpenCurly, TokenOpenCurly;
    CloseCurly, TokenCloseCurly;
    Comma, TokenComma;
    Semicolon, TokenSemicolon;
    Colon, TokenColon;
    DoubleColon, TokenDoubleColon;
    Arrow, TokenArrow;
    FatArrow, TokenFatArrow;
    Dot, TokenDot;
    DotDotDot, TokenDotDotDot;
    Underscore, TokenUnderscore;
    Apostrophe, TokenApostrophe;
    LineComment<'i>, TokenLineComment;
    BlockComment<'i>, TokenBlockComment;
    Eof, TokenEof;
}
