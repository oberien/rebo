use crate::parser::{Binding, expr::NewBinding, ExprLiteral, ExprType, InternalError, Parse, Parser, Separated};
use crate::common::{Depth, Spanned, SpanWithId};
use std::fmt::{self, Display, Formatter};
use crate::lexer::{TokenCloseParen, TokenColon, TokenComma, TokenDoubleColon, TokenIdent, TokenOpenParen, TokenUnderscore};
use super::helper;
use itertools::Itertools;

// make trace! here log as if this still was the parser module
#[allow(unused)]
macro_rules! module_path {
    () => {{
        let path = std::module_path!();
        let end = path.rfind("::").unwrap();
        let end = end.rfind("::").unwrap();
        &path[..end]
    }}
}


#[derive(Debug, Clone, derive_more::Display)]
pub enum ExprPattern<'a, 'i> {
    Typed(ExprPatternTyped<'a, 'i>),
    Untyped(ExprPatternUntyped<'i>),
}
impl<'a, 'i> Parse<'a, 'i> for ExprPattern<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let err1 = match ExprPatternTyped::parse(parser, depth.next()) {
            Ok(typed) => return Ok(ExprPattern::Typed(typed)),
            Err(e) => e,
        };
        let err2 = match ExprPatternUntyped::parse(parser, depth.last()) {
            Ok(untyped) => return Ok(ExprPattern::Untyped(untyped)),
            Err(e) => e,
        };
        Err(helper::last_error(&[err1, err2]))
    }
}
impl<'a, 'i> Spanned for ExprPattern<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            ExprPattern::Typed(t) => t.span_with_id(),
            ExprPattern::Untyped(t) => t.span_with_id(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprPatternUntyped<'i> {
    pub binding: Binding<'i>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprPatternUntyped<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let binding = Binding::parse_new(parser, depth.last())?;
        Ok(ExprPatternUntyped { binding })
    }
}
impl<'i> Spanned for ExprPatternUntyped<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.binding.span_with_id()
    }
}
impl<'i> Display for ExprPatternUntyped<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.binding.mutable.is_some() {
            write!(f, "mut ")?;
        }
        write!(f, "{}", self.binding)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprPatternTyped<'a, 'i> {
    pub pattern: ExprPatternUntyped<'i>,
    pub colon_token: TokenColon,
    pub typ: ExprType<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprPatternTyped<'a, 'i> {
    pub fn new(pattern: ExprPatternUntyped<'i>, colon_token: TokenColon, typ: ExprType<'a, 'i>) -> Self {
        let span = pattern.span_with_id() | typ.span_with_id();
        ExprPatternTyped { pattern, colon_token, typ, span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprPatternTyped<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let pattern = parser.parse(depth.next())?;
        let colon_token = parser.parse(depth.next())?;
        let typ = parser.parse(depth.last())?;
        Ok(ExprPatternTyped::new(pattern, colon_token, typ))
    }
}
impl<'a, 'i> Spanned for ExprPatternTyped<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprPatternTyped<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.pattern, self.typ)
    }
}

#[derive(Debug, Clone, derive_more::Display)]
#[allow(clippy::large_enum_variant)]
pub enum ExprMatchPattern<'a, 'i> {
    Literal(ExprLiteral),
    Variant(ExprMatchPatternVariant<'a, 'i>),
    Binding(Binding<'i>),
    Wildcard(TokenUnderscore),
}
impl<'a, 'i> Parse<'a, 'i> for ExprMatchPattern<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let err1 = match ExprMatchPatternVariant::parse(parser, depth.next()) {
            Ok(variant) => return Ok(ExprMatchPattern::Variant(variant)),
            Err(e) => e,
        };
        let err2 = match ExprLiteral::parse(parser, depth.next()) {
            Ok(literal) => return Ok(ExprMatchPattern::Literal(literal)),
            Err(e) => e,
        };
        let err3 = match Binding::parse_new(parser, depth.next()) {
            Ok(binding) => return Ok(ExprMatchPattern::Binding(binding)),
            Err(e) => e,
        };
        let err4 = match TokenUnderscore::parse(parser, depth.last()) {
            Ok(token) => return Ok(ExprMatchPattern::Wildcard(token)),
            Err(e) => e,
        };
        Err(helper::last_error(&[err1, err2, err3, err4]))
    }
}
impl<'a, 'i> Spanned for ExprMatchPattern<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            ExprMatchPattern::Literal(lit) => lit.span_with_id(),
            ExprMatchPattern::Variant(variant) => variant.span_with_id(),
            ExprMatchPattern::Binding(binding) => binding.span_with_id(),
            ExprMatchPattern::Wildcard(wildcard) => wildcard.span_with_id(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprMatchPatternVariant<'a, 'i> {
    pub enum_name: TokenIdent<'i>,
    pub double_colon: TokenDoubleColon,
    pub variant_name: TokenIdent<'i>,
    pub fields: Option<(TokenOpenParen, Separated<'a, 'i, Binding<'i>, TokenComma>, TokenCloseParen)>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprMatchPatternVariant<'a, 'i> {
    pub fn new(enum_name: TokenIdent<'i>, double_colon: TokenDoubleColon, variant_name: TokenIdent<'i>, fields: Option<(TokenOpenParen, Separated<'a, 'i, Binding<'i>, TokenComma>, TokenCloseParen)>) -> Self {
        let span = enum_name.span | variant_name.span | fields.as_ref().map(|(.., close)| close.span_with_id());
        ExprMatchPatternVariant { enum_name, double_colon, variant_name, fields, span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprMatchPatternVariant<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprMatchPatternVariant::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse::<Option<(TokenOpenParen, Separated<'a, 'i, NewBinding<'i>, TokenComma>, TokenCloseParen)>>(depth.next())?
                .map(|(open, sep, close)| (open, Separated::from(sep), close)),
        ))
    }
}
impl<'a, 'i> Spanned for ExprMatchPatternVariant<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprMatchPatternVariant<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.enum_name.ident, self.variant_name.ident)?;
        if let Some((_, sep, _)) = &self.fields {
            let joined = sep.iter().map(|pat| pat.to_string()).join(", ");
            write!(f, "({})", joined)?;
        }
        Ok(())
    }
}
