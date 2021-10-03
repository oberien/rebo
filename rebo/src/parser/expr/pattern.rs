use crate::parser::{Parse, Parser, InternalError, Spanned, Binding, ExprType, Separated, ExprLiteral};
use crate::common::Depth;
use diagnostic::Span;
use std::fmt::{self, Display, Formatter};
use crate::lexer::{TokenColon, TokenUnderscore, TokenIdent, TokenDoubleColon, TokenOpenParen, TokenComma, TokenCloseParen};
use super::helper;
use derive_more::Display;
use itertools::Itertools;

// make trace! here log as if this still was the parser module
macro_rules! module_path {
    () => {{
        let path = std::module_path!();
        let end = path.rfind("::").unwrap();
        let end = end.rfind("::").unwrap();
        &path[..end]
    }}
}


#[derive(Debug, Clone, Display)]
pub enum ExprPattern<'i> {
    Typed(ExprPatternTyped<'i>),
    Untyped(ExprPatternUntyped<'i>),
}
impl<'a, 'i> Parse<'a, 'i> for ExprPattern<'i> {
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
impl<'i> Spanned for ExprPattern<'i> {
    fn span(&self) -> Span {
        match self {
            ExprPattern::Typed(t) => t.span(),
            ExprPattern::Untyped(t) => t.span(),
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
    fn span(&self) -> Span {
        self.binding.span()
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
pub struct ExprPatternTyped<'i> {
    pub pattern: ExprPatternUntyped<'i>,
    pub colon_token: TokenColon,
    pub typ: ExprType<'i>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprPatternTyped<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprPatternTyped {
            pattern: parser.parse(depth.next())?,
            colon_token: parser.parse(depth.next())?,
            typ: parser.parse(depth.last())?,
        })
    }
}
impl<'i> Spanned for ExprPatternTyped<'i> {
    fn span(&self) -> Span {
        let first = self.pattern.span();
        let last = self.typ.span();
        Span::new(first.file, first.start, last.end)
    }
}
impl<'i> Display for ExprPatternTyped<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.pattern, self.typ)
    }
}

#[derive(Debug, Clone, Display)]
pub enum ExprMatchPattern<'i> {
    Literal(ExprLiteral),
    Binding(Binding<'i>),
    // Variant(ExprMatchPatternVariant<'a, 'i>),
    Wildcard(TokenUnderscore),
}
impl<'a, 'i> Parse<'a, 'i> for ExprMatchPattern<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // let err1 = match ExprMatchPatternVariant::parse(parser, depth.next()) {
        //     Ok(variant) => return Ok(ExprMatchPattern::Variant(variant)),
        //     Err(e) => e,
        // };
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
        Err(helper::last_error(&[err2, err3, err4]))
    }
}
impl<'i> Spanned for ExprMatchPattern<'i> {
    fn span(&self) -> Span {
        match self {
            ExprMatchPattern::Literal(lit) => lit.span(),
            ExprMatchPattern::Binding(binding) => binding.span(),
            ExprMatchPattern::Wildcard(wildcard) => wildcard.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprMatchPatternVariant<'a, 'i> {
    pub enum_name: TokenIdent<'i>,
    pub double_colon: TokenDoubleColon,
    pub variant_name: TokenIdent<'i>,
    pub fields: Option<(TokenOpenParen, Separated<'a, 'i, ExprMatchPattern<'i>, TokenComma>, TokenCloseParen)>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprMatchPatternVariant<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprMatchPatternVariant {
            enum_name: parser.parse(depth.next())?,
            double_colon: parser.parse(depth.next())?,
            variant_name: parser.parse(depth.next())?,
            fields: parser.parse(depth.last())?,
        })
    }
}
impl<'a, 'i> Spanned for ExprMatchPatternVariant<'a, 'i> {
    fn span(&self) -> Span {
        let end = self.fields.as_ref().map(|(.., close)| close.span.end)
            .unwrap_or(self.variant_name.span.end);
        Span::new(self.enum_name.span.file, self.enum_name.span.start, end)
    }
}
impl<'a, 'i> Display for ExprMatchPatternVariant<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.enum_name.ident, self.variant_name.ident)?;
        if let Some((_, sep, _)) = &self.fields {
            let joined = sep.iter().map(|pat| pat.to_string()).join(", ");
            write!(f, "{}", joined)?;
        }
        Ok(())
    }
}
