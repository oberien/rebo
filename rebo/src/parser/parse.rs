use crate::parser::{InternalError, Parser, Expr};
use std::fmt::{self, Display, Formatter};
use crate::lexer::*;
use diagnostic::Span;
use std::marker::PhantomData;
use crate::common::Depth;
use regex::Regex;

// make trace! here log as if this still was the parser module
macro_rules! module_path {
    () => {{
        let path = std::module_path!();
        let end = path.rfind("::").unwrap();
        &path[..end]
    }}
}

pub trait Parse<'a, 'i>: Sized {
    fn parse(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        lazy_static::lazy_static! {
            static ref REGEX: Regex = Regex::new(r#"\w*::[a-zA-Z0-9_:]*::"#).unwrap();
        }
        trace!("{} {}::parse        ({:?})", depth, REGEX.replace_all(::std::any::type_name::<Self>(), ""), parser.peek_token(0));
        let mark = parser.tokens.mark();
        let res = Self::parse_marked(parser, depth)?;
        mark.apply();
        Ok(res)
    }
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError>;
}
impl<'a, 'i, T: Parse<'a, 'i>> Parse<'a, 'i> for Option<T> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        match parser.parse(depth.last()) {
            Ok(t) => Ok(Some(t)),
            Err(_) => Ok(None),
        }
    }
}
impl<'a, 'i, T: Parse<'a, 'i>> Parse<'a, 'i> for Vec<T> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let mut vec = Vec::new();
        while let Ok(t) = parser.parse(depth.next()) {
            vec.push(t);
        }
        Ok(vec)
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}
impl<'a, T: Spanned> Spanned for &'a T {
    fn span(&self) -> Span {
        <T as Spanned>::span(self)
    }
}

#[derive(Debug, Clone)]
pub struct Separated<'a, 'i, T: 'a, D: 'a> {
    inner: Vec<(T, D)>,
    last: Option<T>,
    marker: PhantomData<&'a Expr<'a, 'i>>,
}

impl<'a, 'i, T: Parse<'a, 'i>, D: Parse<'a, 'i>> Parse<'a, 'i> for Separated<'a, 'i, T, D> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(Separated {
            inner: parser.parse(depth.next())?,
            last: parser.parse(depth.last())?,
            marker: PhantomData,
        })
    }
}
impl<'b, 'a: 'b, 'i: 'b, T: 'a, D: 'a> Separated<'a, 'i, T, D> {
    pub fn len(&self) -> usize {
        self.inner.len() + (self.last.is_some() as usize)
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn iter(&'b self) -> <&'b Self as IntoIterator>::IntoIter {
        (&self).into_iter()
    }
    pub fn last_unterminated(&'b self) -> Option<&T> {
        self.last.as_ref()
    }
    pub fn last_terminated(&'b self) -> Option<&T> {
        self.inner.last().map(|(t, _d)| t)
    }
}
impl<'a, 'i, T: Spanned + 'a, D: 'a> Separated<'a, 'i, T, D> {
    pub fn span(&self) -> Option<Span> {
        let first = self.iter().next();
        let last = self.last.as_ref().or(self.inner.iter().next().map(|(t, _d)| t));
        match (first, last) {
            (Some(first), None) => Some(first.span()),
            (None, Some(last)) => Some(last.span()),
            (Some(first), Some(last)) => Some(Span::new(first.span().file, first.span().start, last.span().start)),
            (None, None) => None,
        }
    }
}
impl<'a, 'i, T: 'a, D: 'a> IntoIterator for Separated<'a, 'i, T, D> {
    type Item = T;
    type IntoIter = Box<dyn Iterator<Item = T> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.inner.into_iter().map(|(t, _)| t).chain(self.last))
    }
}
impl<'b, 'a: 'b, 'i: 'b, T: 'a, D: 'a> IntoIterator for &'b Separated<'a, 'i, T, D> {
    type Item = &'b T;
    type IntoIter = Box<dyn Iterator<Item = &'b T> + 'b>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.inner.iter().map(|(t, _)| t).chain(&self.last))
    }
}
impl<'a, 'i, T: Display, D: Display> Display for Separated<'a, 'i, T, D> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut joined: String = self.inner.iter()
            .map(|(t, d)| format!("{}{}", t, d))
            .chain(self.last.iter().map(|t| t.to_string()))
            .collect();
        if joined.ends_with(" ") {
            joined.pop();
        }
        write!(f, "{}", joined)
    }
}

macro_rules! impl_for_tuples {
    ($last:ident $(,$name:ident)*) => {
        impl<'a, 'i, $($name: Parse<'a, 'i>,)* $last: Parse<'a, 'i>> Parse<'a, 'i> for ($($name,)* $last,) {
            fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
                Ok(($($name::parse(parser, depth.next())?,)* $last::parse(parser, depth.last())?,))
            }
        }
    }
}
impl_for_tuples!(A);
impl_for_tuples!(A, B);
impl_for_tuples!(A, B, C);
impl_for_tuples!(A, B, C, D);
impl_for_tuples!(A, B, C, D, E);
impl_for_tuples!(A, B, C, D, E, F);


macro_rules! impl_for_tokens {
    ($($name:ident$(<$lt:lifetime>)?, $tokenname:ident;)+) => {
        $(
            impl<'a, 'i> crate::parser::Parse<'a, 'i> for $tokenname $(<$lt>)? {
                fn parse_marked(parser: &mut crate::parser::Parser<'a, '_, 'i>, _depth: Depth) -> Result<Self, crate::parser::InternalError> {
                    match parser.peek_token(0) {
                        Some(crate::lexer::Token::$name(t)) => {
                            drop(parser.next_token());
                            Ok(t)
                        },
                        _ => Err(crate::parser::InternalError::Backtrack(
                            parser.tokens.next_span(),
                            ::std::borrow::Cow::Borrowed(&[crate::parser::Expected::Token(crate::lexer::TokenType::$name)]),
                        )),
                    }
                }
            }
            impl $(<$lt>)? Spanned for $tokenname $(<$lt>)? {
                fn span(&self) -> Span {
                    self.span
                }
            }
        )+
    }
}

impl_for_tokens! {
    // primitives
    Ident<'i>, TokenIdent;
    DqString, TokenDqString;
    Integer, TokenInteger;
    Float, TokenFloat;
    Bool, TokenBool;
    // keywords
    Let, TokenLet;
    Mut, TokenMut;
    Fn, TokenFn;
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
    FuzzyEquals, TokenFuzzyEquals;
    FuzzyNotEquals, TokenFuzzyNotEquals;
    GreaterEquals, TokenGreaterEquals;
    GreaterThan, TokenGreaterThan;
    Plus, TokenPlus;
    Minus, TokenMinus;
    Star, TokenStar;
    Slash, TokenSlash;
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
    Arrow, TokenArrow;
    LineComment<'i>, TokenLineComment;
    BlockComment<'i>, TokenBlockComment;
    Eof, TokenEof;
}
