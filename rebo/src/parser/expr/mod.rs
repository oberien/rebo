mod pattern;
mod helper;
mod generator_transform;
#[cfg(test)]
mod generator_tests;
pub use pattern::{ExprMatchPattern, ExprPattern, ExprPatternTyped, ExprPatternUntyped};

use std::fmt::{self, Debug, Display, Formatter, Write};
use derive_more::Display;
use crate::parser::scope::{BindingId, ScopeType};
use crate::util::PadFmt;
use crate::lexer::{Lexer, LexerMode, Token, TokenAmp, TokenApostrophe, TokenArrow, TokenAssign, TokenBang, TokenBool, TokenBoolType, TokenBreak, TokenCircumflex, TokenCloseCurly, TokenCloseParen, TokenColon, TokenComma, TokenContinue, TokenDot, TokenDotDotDot, TokenDoubleAmp, TokenDoubleColon, TokenDoublePipe, TokenDqString, TokenElse, TokenEnum, TokenEquals, TokenFatArrow, TokenFloat, TokenFloatType, TokenFn, TokenFor, TokenFormatString, TokenFormatStringPart, TokenGen, TokenGreaterEquals, TokenGreaterThan, TokenIdent, TokenIf, TokenImpl, TokenIn, TokenInclude, TokenInteger, TokenIntType, TokenLessEquals, TokenLessThan, TokenLet, TokenLoop, TokenMatch, TokenMinus, TokenMut, TokenNotEquals, TokenOpenCurly, TokenOpenParen, TokenPercent, TokenPipe, TokenPlus, TokenReturn, TokenSlash, TokenStar, TokenStatic, TokenStringType, TokenStruct, TokenType, TokenWhile, TokenYield};
use crate::parser::{Backtrack, Expected, InternalError, Parse, Parser};
use crate::error_codes::ErrorCode;
use std::borrow::Cow;
use crate::parser::parse::{Scoped, Separated};
use crate::parser::precedence::{BooleanExpr, Math};
use diagnostic::Span;
use itertools::{Either, Itertools};
use crate::common::{Depth, Spanned, SpanWithId, UserType};
use indexmap::set::IndexSet;
use rt_format::{Format, Specifier};
use crate::util;

// make trace! here log as if this still was the parser module
macro_rules! module_path {
    () => {{
        let path = std::module_path!();
        let end = path.rfind("::").unwrap();
        &path[..end]
    }}
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Binding<'i> {
    pub id: BindingId,
    pub mutable: Option<TokenMut>,
    pub ident: TokenIdent<'i>,
    /// If this is a rogue binding that was created by the parser when an error occurred.
    /// If there is a further error involving this binding, it shouldn't be emitted.
    pub rogue: bool,
    pub span: SpanWithId,
}
impl<'i> Binding<'i> {
    /// Return the newly created binding
    fn parse_new<'a>(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Binding<'i>, InternalError> {
        trace!("{} Binding::parse_new        ({:?})", depth, parser.peek_token(0));
        Binding::parse_new_internal(parser, false, depth)
    }
    /// Return the newly created binding, allowing `self` as name
    fn parse_self<'a>(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Binding<'i>, InternalError> {
        trace!("{} Binding::parse_self        ({:?})", depth, parser.peek_token(0));
        Binding::parse_new_internal(parser, true, depth)
    }
    fn parse_new_internal<'a>(parser: &mut Parser<'a, '_, 'i>, require_self: bool, depth: Depth) -> Result<Binding<'i>, InternalError> {
        let mark = parser.lexer.mark();
        let mut_token: Option<TokenMut> = parser.parse(depth.next())?;
        let ident: TokenIdent = parser.parse(depth.last())?;
        if !require_self && ident.ident == "self" {
            parser.diagnostics.error(ErrorCode::SelfBinding)
                .with_error_label(ident.span.diagnostics_span(), "using `self` as variable is not allowed here")
                .with_note("note: `self` is only allowed as first parameter of methods")
                .emit();
        }
        if require_self && ident.ident != "self" {
            return Err(InternalError::Backtrack(Backtrack {
                span: ident.span.diagnostics_span(),
                expected: Cow::Borrowed( & [Expected::Token(TokenType::Ident)]),
            }));
        }
        mark.apply();
        let binding = parser.add_binding(ident, mut_token);
        trace!("{} got binding {}", depth, binding);
        Ok(binding)
    }
    /// Return the existing binding and the usage-span
    fn parse_existing<'a>(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<(Binding<'i>, SpanWithId), InternalError> {
        trace!("{} Binding::parse_existing        ({:?})", depth, parser.peek_token(0));
        let ident = match (parser.peek_token(0)?, parser.peek_token(1), parser.peek_token(2)) {
            (
                t @ Token::StringType(_) | t @ Token::IntType(_) | t @ Token::FloatType(_) | t @ Token::BoolType(_),
                Ok(Token::DoubleColon(_)),
                Ok(Token::Ident(_)),
            ) => {
                drop(parser.next_token().unwrap());
                TokenIdent {
                    ident: t.typ().as_str(),
                    span: t.span_with_id(),
                }
            },
            _ => parser.parse(depth.last())?,
        };
        // hack to allow functions as values for now until we have proper path resolution
        let rest: Option<(TokenDoubleColon, TokenIdent<'i>)> = parser.parse(depth.next())?;
        let ident = match rest {
            None => ident,
            Some((_colon, rest)) => {
                let span = ident.span | rest.span;
                TokenIdent {
                    span,
                    ident: parser.diagnostics.resolve_span(span.diagnostics_span()),
                }
            }
        };
        let binding = match parser.get_binding(ident.ident) {
            Some(binding) => binding,
            None if ident.ident.contains("::") => parser.add_rogue_binding(ident, Some(TokenMut { span: SpanWithId::from(ident.span.diagnostics_span()) })),
            None => parser.diagnostic_unknown_identifier(ident, |d| d.with_info_label(ident.span.diagnostics_span(), format!("use `let {} = ...` to create a new binding", ident))),
        };
        trace!("{} got binding {}", depth, binding);
        Ok((binding, ident.span))
    }
}
impl<'i> Spanned for Binding<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for Binding<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.ident.ident, self.id)
    }
}
struct NewBinding<'i> {
    b: Binding<'i>,
}
impl<'i> From<NewBinding<'i>> for Binding<'i> {
    fn from(n: NewBinding<'i>) -> Self {
        n.b
    }
}
impl<'a, 'i> Parse<'a, 'i> for NewBinding<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Binding::parse_new(parser, depth).map(|b| NewBinding { b })
    }
}
impl<'i> Spanned for NewBinding<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.b.span_with_id()
    }
}
impl<'i> Display for NewBinding<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.b, f)
    }
}
struct SelfBinding<'i> {
    b: Binding<'i>,
}
impl<'i> From<SelfBinding<'i>> for Binding<'i> {
    fn from(n: SelfBinding<'i>) -> Self {
        n.b
    }
}
impl<'a, 'i> Parse<'a, 'i> for SelfBinding<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Binding::parse_self(parser, depth).map(|b| SelfBinding { b })
    }
}
impl<'i> Spanned for SelfBinding<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.b.span_with_id()
    }
}
impl<'i> Display for SelfBinding<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.b, f)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Generic<'i> {
    /// identifier of the original defintion
    pub def_ident: TokenIdent<'i>,
    /// identifier of the usage
    pub ident: TokenIdent<'i>,
}
impl<'i> Generic<'i> {
    fn parse_new(parser: &mut Parser<'_, '_, 'i>, depth: Depth) -> Result<Generic<'i>, InternalError> {
        trace!("{} Generic::parse_new        ({:?})", depth, parser.peek_token(0));
        let ident = parser.parse(depth.last())?;
        let generic = Generic {
            def_ident: ident,
            ident,
        };
        parser.add_generic(generic);
        Ok(generic)
    }
}
impl<'i> Display for Generic<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.ident, f)
    }
}
impl<'i> Spanned for Generic<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.ident.span_with_id()
    }
}
struct NewGeneric<'i> {
    g: Generic<'i>,
}
impl<'i> From<NewGeneric<'i>> for Generic<'i> {
    fn from(n: NewGeneric<'i>) -> Self {
        n.g
    }
}
impl<'a, 'i> Parse<'a, 'i> for NewGeneric<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Generic::parse_new(parser, depth).map(|b| NewGeneric { g: b })
    }
}
impl<'i> Spanned for NewGeneric<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.g.span_with_id()
    }
}
impl<'i> Display for NewGeneric<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.g, f)
    }
}

#[derive(Debug, Clone, Display)]
pub enum ExprLiteral {
    /// ()
    Unit(ExprUnit),
    /// 0
    Integer(ExprInteger),
    /// 0.
    Float(ExprFloat),
    /// true
    Bool(ExprBool),
    /// "foo"
    String(ExprString),
}
impl<'a, 'i> Parse<'a, 'i> for ExprLiteral {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let err1 = match ExprUnit::parse(parser, depth.next()) {
            Ok(unit) => return Ok(ExprLiteral::Unit(unit)),
            Err(e) => e,
        };
        let err2 = match ExprInteger::parse(parser, depth.next()) {
            Ok(int) => return Ok(ExprLiteral::Integer(int)),
            Err(e) => e,
        };
        let err3 = match ExprFloat::parse(parser, depth.next()) {
            Ok(float) => return Ok(ExprLiteral::Float(float)),
            Err(e) => e,
        };
        let err4 = match ExprBool::parse(parser, depth.next()) {
            Ok(boolean) => return Ok(ExprLiteral::Bool(boolean)),
            Err(e) => e,
        };
        let err5 = match ExprString::parse(parser, depth.next()) {
            Ok(string) => return Ok(ExprLiteral::String(string)),
            Err(e) => e,
        };
        Err(helper::last_error(&[err1, err2, err3, err4, err5]))
    }
}
impl Spanned for ExprLiteral {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            ExprLiteral::Unit(e) => e.span_with_id(),
            ExprLiteral::Integer(e) => e.span_with_id(),
            ExprLiteral::Float(e) => e.span_with_id(),
            ExprLiteral::Bool(e) => e.span_with_id(),
            ExprLiteral::String(e) => e.span_with_id(),
        }
    }
}

#[derive(Debug, Display, rebo_derive::Functions, derive_more::From)]
#[function(fn span_with_id(&self) -> SpanWithId = expr => expr.span_with_id())]
#[allow(clippy::large_enum_variant)]
pub enum Expr<'a, 'i> {
    Literal(ExprLiteral),
    /// f"abc {expr} def"
    FormatString(ExprFormatString<'a, 'i>),
    /// let pattern = expr
    Bind(ExprBind<'a, 'i>),
    /// static pattern = expr
    Static(ExprStatic<'a, 'i>),
    /// ident = expr
    Assign(ExprAssign<'a, 'i>),
    // unops
    /// !expr
    BoolNot(ExprBoolNot<'a, 'i>),
    /// -expr
    Neg(ExprNeg<'a, 'i>),
    // binops
    /// expr + expr
    Add(ExprAdd<'a, 'i>),
    /// expr - expr
    Sub(ExprSub<'a, 'i>),
    /// expr * expr
    Mul(ExprMul<'a, 'i>),
    /// expr / expr
    Div(ExprDiv<'a, 'i>),
    /// expr % expr
    Mod(ExprMod<'a, 'i>),
    /// expr ^ expr
    Xor(ExprXor<'a, 'i>),
    /// expr && expr
    BoolAnd(ExprBoolAnd<'a, 'i>),
    /// expr || bar
    BoolOr(ExprBoolOr<'a, 'i>),
    // binop-assign
    /// lhs += expr
    AddAssign(ExprAddAssign<'a, 'i>),
    /// lhs -= expr
    SubAssign(ExprSubAssign<'a, 'i>),
    /// lhs *= expr
    MulAssign(ExprMulAssign<'a, 'i>),
    /// lhs /= expr
    DivAssign(ExprDivAssign<'a, 'i>),
    /// lhs %= expr
    ModAssign(ExprModAssign<'a, 'i>),
    /// lhs ^= expr
    XorAssign(ExprXorAssign<'a, 'i>),
    /// lhs &= expr
    BoolAndAssign(ExprBoolAndAssign<'a, 'i>),
    /// lhs |= bar
    BoolOrAssign(ExprBoolOrAssign<'a, 'i>),
    // comparison ops
    /// expr < expr
    LessThan(ExprLessThan<'a, 'i>),
    /// expr <= expr
    LessEquals(ExprLessEquals<'a, 'i>),
    /// expr == expr
    Equals(ExprEquals<'a, 'i>),
    /// expr != expr
    NotEquals(ExprNotEquals<'a, 'i>),
    /// expr >= expr
    GreaterEquals(ExprGreaterEquals<'a, 'i>),
    /// expr > expr
    GreaterThan(ExprGreaterThan<'a, 'i>),
    /// { expr;... }
    Block(ExprBlock<'a, 'i>),
    /// foo
    Variable(ExprVariable<'i>),
    /// foo.bar.baz().qux.quux().corge
    Access(ExprAccess<'a, 'i>),
    /// (expr)
    Parenthesized(ExprParenthesized<'a, 'i>),
    /// if expr {...} else if expr {...} else if expr {...} else {...}
    IfElse(ExprIfElse<'a, 'i>),
    /// match expr { pat => expr, pat => expr, ... }
    Match(ExprMatch<'a, 'i>),
    /// 'label: while expr {...}
    While(ExprWhile<'a, 'i>),
    /// 'label: for binding in expr {...}
    For(ExprFor<'a, 'i>),
    /// 'label: loop {...}
    Loop(ExprLoop<'a, 'i>),
    /// break 'label expr
    Break(ExprBreak<'a, 'i>),
    /// continue 'label
    Continue(ExprContinue<'i>),
    /// return expr
    Return(ExprReturn<'a, 'i>),
    /// yield expr
    Yield(ExprYield<'a, 'i>),
    /// (ident::)?ident(expr, expr, ...)
    FunctionCall(ExprFunctionCall<'a, 'i>),
    /// gen? fn ident(pat: typ, pat: typ, ...) -> typ { expr... }
    FunctionDefinition(ExprFunctionDefinition<'a, 'i>),
    /// struct ident { ident: typ, ident: typ, ... }
    StructDefinition(ExprStructDefinition<'a, 'i>),
    /// ident { ident: expr, ident: expr, ... }
    StructInitialization(ExprStructInitialization<'a, 'i>),
    /// enum ident { ident, ident(typ, typ, ...), ... }
    EnumDefinition(ExprEnumDefinition<'a, 'i>),
    // enum tuple-variant initialization is handled with an associated function
    /// C-Like enum variants: ident::ident
    EnumInitialization(ExprEnumInitialization<'i>),
    /// impl ident { fn foo(...) {...} fn bar(self, ...) {...} ... }
    ImplBlock(ExprImplBlock<'a, 'i>),
}
impl<'a, 'i> Spanned for Expr<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        Expr::span_with_id(self)
    }
}
#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub(in crate::parser) enum ParseUntil {
    Math,
    Compare,
    BooleanExpr,
    All,
}
impl<'a, 'i> Expr<'a, 'i> {
    pub fn is_item(&self) -> bool {
        match self {
            Expr::FunctionDefinition(ExprFunctionDefinition { sig: ExprFunctionSignature { name: Some(_), .. }, .. })
            | Expr::StructDefinition(_)
            | Expr::EnumDefinition(_)
            | Expr::ImplBlock(_)
            | Expr::IfElse(_)
            | Expr::Match(_)
            | Expr::Loop(_)
            | Expr::While(_)
            | Expr::For(_)
            | Expr::Block(_) => true,
            _ => false,
        }
    }
    fn try_parse_until_including(parser: &mut Parser<'a, '_, 'i>, until: ParseUntil, depth: Depth) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{} Expr::try_parse_until_including {:?}        ({:?})", depth, until, parser.peek_token(0));
        Expr::try_parse_until(parser, until, PartialOrd::ge, depth)
    }
    pub(in crate::parser) fn try_parse_until_excluding(parser: &mut Parser<'a, '_, 'i>, until: ParseUntil, depth: Depth) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{} Expr::try_parse_until_excluding {:?}        ({:?})", depth, until, parser.peek_token(0));
        Expr::try_parse_until(parser, until, PartialOrd::gt, depth)
    }
    fn try_parse_until(parser: &mut Parser<'a, '_, 'i>, until: ParseUntil, cmp: fn(&ParseUntil, &ParseUntil) -> bool, depth: Depth) -> Result<&'a Expr<'a, 'i>, InternalError> {
        type ParseFn<'a, 'b, 'i> = fn(&mut Parser<'a, 'b, 'i>, Depth) -> Result<&'a Expr<'a, 'i>, InternalError>;
        let mut fns: Vec<ParseFn<'a, '_, 'i>> = Vec::new();
        // add in reverse order
        if cmp(&until, &ParseUntil::BooleanExpr) {
            fns.push(Self::try_parse_precedence::<BooleanExpr>);
        }
        if cmp(&until, &ParseUntil::Compare) {
            fns.push(Self::try_parse_compare);
        }
        if cmp(&until, &ParseUntil::Math) {
            fns.push(Self::try_parse_precedence::<Math>);
        }
        let others: &[ParseFn<'a, '_, 'i>] = &[
            |parser: &mut Parser<'a, '_, 'i>, depth| {
                let expr = ExprFunctionDefinition::parse(parser, depth)?;
                let expr = &*parser.arena.alloc(Expr::FunctionDefinition(expr));
                let fun = match expr {
                    Expr::FunctionDefinition(fun) => fun,
                    _ => unreachable!("we just created you"),
                };
                parser.add_free_function_to_meta_info(fun);
                Ok(expr)
            },
            |parser: &mut Parser<'a, '_, 'i>, depth| {
                let expr = &*parser.arena.alloc(Expr::ImplBlock(ExprImplBlock::parse(parser, depth)?));
                let impl_block = match expr {
                    Expr::ImplBlock(impl_block) => impl_block,
                    _ => unreachable!("we just created you"),
                };
                parser.add_impl_block_functions_to_meta_info(impl_block);
                Ok(expr)
            },
            |parser: &mut Parser<'a, '_, 'i>, depth| {
                let static_expr = &*parser.arena.alloc(Expr::Static(ExprStatic::parse(parser, depth)?));
                let static_ = match static_expr {
                    Expr::Static(stati) => stati,
                    _ => unreachable!("we just created you"),
                };
                // add static to globals if it isn't there already (needed for included statics)
                parser.meta_info.add_static(parser.diagnostics, static_);
                Ok(static_expr)
            },
            |parser: &mut Parser<'a, '_, 'i>, depth| {
                let include = ExprInclude::parse(parser, depth)?;
                // We don't actually want to return an error, as that would mean that this function
                // is called several more times when other expressions try to parse expressions.
                // Instead, we return Unit to consume the tokens, as we did in fact parse the include correctly.
                let err = Ok(&*parser.arena.alloc(Expr::Literal(ExprLiteral::Unit(ExprUnit {
                    open: TokenOpenParen { span: include.include_token.span | include.file.span },
                    close: TokenCloseParen { span: include.file.span.end_span() },
                    span: include.include_token.span | include.file.span,
                }))));

                let file = match parser.meta_info.included_files.get(&include.span_with_id()) {
                    Some(&file) => file,
                    None => return err,
                };
                let lexer = Lexer::new(parser.diagnostics, file);
                let old_lexer = ::std::mem::replace(&mut parser.lexer, lexer);
                let body_res = parser.parse_file_content();
                parser.lexer = old_lexer;
                let body = match body_res {
                    Ok(body) => body,
                    Err(_) => return err,
                };
                let block = &*parser.arena.alloc(Expr::Block(ExprBlock {
                    open: TokenOpenCurly { span: include.include_token.span.start_span() },
                    body,
                    close: TokenCloseCurly { span: include.file.span.end_span() },
                    span: include.include_token.span | include.file.span,
                }));

                Ok(block)
            },
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::StructInitialization(ExprStructInitialization::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Break(ExprBreak::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Continue(ExprContinue::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Return(ExprReturn::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Yield(ExprYield::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::IfElse(ExprIfElse::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Match(ExprMatch::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::While(ExprWhile::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::For(ExprFor::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Loop(ExprLoop::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Parenthesized(ExprParenthesized::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Block(ExprBlock::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::BoolNot(ExprBoolNot::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Neg(ExprNeg::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Bind(ExprBind::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::EnumInitialization(ExprEnumInitialization::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::FunctionCall(ExprFunctionCall::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Literal(ExprLiteral::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::FormatString(ExprFormatString::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Assign(ExprAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::AddAssign(ExprAddAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::SubAssign(ExprSubAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::MulAssign(ExprMulAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::DivAssign(ExprDivAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::BoolAndAssign(ExprBoolAndAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::BoolOrAssign(ExprBoolOrAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Access(ExprAccess::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Variable(ExprVariable::parse(parser, depth)?))),
        ];
        fns.extend(others);

        let mut expected = Vec::new();
        for (i, f) in fns.iter().enumerate() {
            // check if we have memoization already
            let token = parser.peek_token(0)?;
            let memoized = if let Some(expr) = parser.pre_parsed.remove(&(token.file_id(), token.start())) {
                Some(expr)
            } else if let Some(&(expr, memuntil)) = parser.memoization.get(&(token.file_id(), token.start())) {
                if cmp(&until, &memuntil) {
                    Some(expr)
                } else {
                    None
                }
            } else {
                None
            };
            match memoized {
                None => (),
                Some(expr) => {
                    // consume tokens of known expression
                    while parser.peek_token(0).unwrap().start() < expr.end() {
                        drop(parser.next_token());
                    }
                    trace!("{}    reusing {:?}: {}", depth, expr.diagnostics_span(), expr);
                    return Ok(expr);
                }
            }

            let mark = parser.lexer.mark();
            let next_depth = if i == fns.len() - 1 { depth.last() } else { depth.next() };
            match f(parser, next_depth) {
                Ok(expr) => {
                    trace!("{}    memoize {:?}: {}", depth, expr.diagnostics_span(), expr);
                    parser.memoization.entry((expr.file_id(), expr.start()))
                        .and_modify(|mem| if until >= mem.1 {
                            *mem = (expr, until);
                        }).or_insert((expr, until));
                    mark.apply();
                    return Ok(expr)
                },
                Err(InternalError::Backtrack(backtrack)) => {
                    parser.backtracks.insert(backtrack.clone());
                    expected.push(backtrack)
                },
                e @ Err(InternalError::Error(_)) => return e,
            }
        }
        let max_span = expected.iter().map(|Backtrack { span, expected: _ }| span).max().copied().unwrap();
        let expected: IndexSet<_> = expected.into_iter()
            .filter(|Backtrack { span, expected: _ }| *span == max_span)
            .flat_map(|backtrack| backtrack.expected.into_owned())
            .collect();
        let mut expected: Vec<_> = expected.into_iter().collect();
        expected.sort();
        Err(InternalError::Backtrack(Backtrack { span: max_span, expected: Cow::Owned(expected) }))
    }

    fn try_parse_compare(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<&'a Expr<'a, 'i>, InternalError> {
        trace!("{} Expr::try_parse_compare        ({:?})", depth, parser.peek_token(0));
        let mark = parser.lexer.mark();
        let left = Expr::try_parse_until_excluding(parser, ParseUntil::Compare, depth.next())?;
        trace!("{}    left: {}", depth, left);
        let (constructor, token): (fn(&'a Expr<'a, 'i>, Token<'i>, &'a Expr<'a, 'i>) -> Expr<'a, 'i>, _) = match parser.next_token()? {
            op @ Token::LessThan(_) => (ExprLessThan::new_as_expr, op),
            op @ Token::LessEquals(_) => (ExprLessEquals::new_as_expr, op),
            op @ Token::Equals(_) => (ExprEquals::new_as_expr, op),
            op @ Token::NotEquals(_) => (ExprNotEquals::new_as_expr, op),
            op @ Token::GreaterEquals(_) => (ExprGreaterEquals::new_as_expr, op),
            op @ Token::GreaterThan(_) => (ExprGreaterThan::new_as_expr, op),
            token => return Err(InternalError::Backtrack(Backtrack {
                span: token.diagnostics_span(),
                expected: Cow::Borrowed(Expected::COMPARE_OP),
            })),
        };
        let right = Expr::try_parse_until_including(parser, ParseUntil::Compare, depth.last())?;
        trace!("{}    right: {}", depth, right);
        mark.apply();
        let expr = constructor(left, token, right);
        let res = parser.arena.alloc(expr);
        Ok(res)
    }
}
impl<'a, 'i> Parse<'a, 'i> for &'a Expr<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Expr::try_parse_until_including(parser, ParseUntil::All, depth.last())
    }
}

pub type TypeGenerics<'a, 'i> = (TokenLessThan, Box<Separated<'a, 'i, ExprType<'a, 'i>, TokenComma>>, TokenGreaterThan);
#[derive(Debug, Clone)]
pub enum ExprType<'a, 'i> {
    // can't be parsed, only generated
    Parenthesized(ExprTypeParenthesized<'a, 'i>),
    String(TokenStringType),
    Int(TokenIntType),
    Float(TokenFloatType),
    Bool(TokenBoolType),
    Unit(ExprTypeUnit),
    // struct, enum, typedef, ...
    UserType(ExprTypeUserType<'a, 'i>),
    Generic(Generic<'i>),
    Function(Box<ExprFunctionType<'a, 'i>>),
    Never(TokenBang),
    // only used from rust in transformations (e.g. Generator-struct fields)
    /// def-span for the typechecker to convert it to a generic
    Any(SpanWithId),
}
#[derive(Debug, Clone)]
pub struct ExprTypeParenthesized<'a, 'i> {
    pub open: TokenOpenParen,
    pub typ: Box<ExprType<'a, 'i>>,
    pub close: TokenCloseParen,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprTypeParenthesized<'a, 'i> {
    pub fn new(open: TokenOpenParen, typ: Box<ExprType<'a, 'i>>, close: TokenCloseParen) -> Self {
        ExprTypeParenthesized { open, typ, close, span: open.span | close.span }
    }
}
impl<'a, 'i> Spanned for ExprTypeParenthesized<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
#[derive(Debug, Clone)]
pub struct ExprTypeUnit {
    pub open: TokenOpenParen,
    pub close: TokenCloseParen,
    pub span: SpanWithId,
}
impl ExprTypeUnit {
    pub fn new(open: TokenOpenParen, close: TokenCloseParen) -> Self {
        ExprTypeUnit { open, close, span: open.span | close.span }
    }
}
impl Spanned for ExprTypeUnit {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
#[derive(Debug, Clone)]
pub struct ExprTypeUserType<'a, 'i> {
    pub name: TokenIdent<'i>,
    pub generics: Option<TypeGenerics<'a, 'i>>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprTypeUserType<'a, 'i> {
    pub fn new(name: TokenIdent<'i>, generics: Option<TypeGenerics<'a, 'i>>) -> Self {
        let span = match &generics {
            Some((_open, _generics, close)) => name.span | close.span,
            None => name.span,
        };
        ExprTypeUserType { name, generics, span }
    }
}
impl<'a, 'i> Spanned for ExprTypeUserType<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> ExprType<'a, 'i> {
    pub fn name(&self) -> &'i str {
        match self {
            ExprType::Parenthesized(ExprTypeParenthesized { typ, .. }) => typ.name(),
            ExprType::String(_) => "string",
            ExprType::Int(_) => "int",
            ExprType::Float(_) => "float",
            ExprType::Bool(_) => "bool",
            ExprType::Unit(_) => "()",
            ExprType::UserType(ExprTypeUserType { name, .. }) => name.ident,
            ExprType::Generic(g) => g.ident.ident,
            ExprType::Function(_) => "fn",
            ExprType::Never(_) => "!",
            ExprType::Any(_) => "any",
        }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprType<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        if let Ok(fn_type) = ExprFunctionType::parse(parser, depth.next()) {
            return Ok(ExprType::Function(Box::new(fn_type)));
        }
        let res = match parser.peek_token(0)? {
            Token::StringType(t) => {
                trace!("{} TokenStringType::parse        ({:?})", depth.next(), parser.peek_token(0));
                ExprType::String(t)
            },
            Token::IntType(t) => {
                trace!("{} TokenIntType::parse        ({:?})", depth.next(), parser.peek_token(0));
                ExprType::Int(t)
            },
            Token::FloatType(t) => {
                trace!("{} TokenFloatType::parse        ({:?})", depth.next(), parser.peek_token(0));
                ExprType::Float(t)
            },
            Token::BoolType(t) => {
                trace!("{} TokenBoolType::parse        ({:?})", depth.next(), parser.peek_token(0));
                ExprType::Bool(t)
            },
            Token::OpenParen(open) => {
                trace!("{} Unit::parse        ({:?})", depth.next(), parser.peek_token(0));
                match parser.peek_token(1)? {
                    Token::CloseParen(close) => {
                        drop(parser.next_token());
                        ExprType::Unit(ExprTypeUnit::new(open, close))
                    }
                    _ => return Err(InternalError::Backtrack(Backtrack {
                        span: parser.lexer.next_span_from(1),
                        expected: Cow::Borrowed(&[Expected::Token(TokenType::CloseParen)]),
                    })),
                }
            }
            // defer struct type resolution until the typechecker
            // otherwise using struct B as field in struct A won't work if B is defined after A
            Token::Ident(name) => {
                trace!("{} TokenIdent::parse ({:?})        ({:?})", depth.next(), parser.generic_names(), parser.peek_token(0));
                if let Some(generic) = parser.get_generic(name.ident) {
                    trace!("{} Generic::parse        ({:?})", depth.next().next(), parser.peek_token(0));
                    ExprType::Generic(generic)
                } else {
                    trace!("{} UserType::parse        ({:?})", depth.next().next(), parser.peek_token(0));
                    drop(parser.next_token());
                    let generics = parser.parse(depth.last())?;
                    return Ok(ExprType::UserType(ExprTypeUserType::new(name, generics)))
                }
            },
            Token::DqString(dq) if dq.string == "any" => {
                trace!("{} TokenDqString::parse        ({:?})", depth.next(), parser.peek_token(0));
                ExprType::Any(dq.span_with_id())
            },
            Token::Bang(b) => {
                trace!("{} TokenBang::parse        ({:?})", depth.next(), parser.peek_token(0));
                ExprType::Never(b)
            },
            _ => return Err(InternalError::Backtrack(Backtrack {
                span: parser.lexer.next_span(),
                expected: Cow::Borrowed(&[Expected::Type]),
            })),
        };
        drop(parser.next_token());
        Ok(res)
    }
}
impl<'a, 'i> From<Generic<'i>> for ExprType<'a, 'i> {
    fn from(g: Generic<'i>) -> Self {
        ExprType::Generic(g)
    }
}
impl<'a, 'i> Spanned for ExprType<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            ExprType::Parenthesized(t) => t.span,
            ExprType::String(t) => t.span_with_id(),
            ExprType::Int(t) => t.span_with_id(),
            ExprType::Float(t) => t.span_with_id(),
            ExprType::Bool(t) => t.span_with_id(),
            ExprType::Unit(t) => t.span_with_id(),
            ExprType::UserType(t) => t.span_with_id(),
            ExprType::Generic(t) => t.span_with_id(),
            ExprType::Function(t) => t.span_with_id(),
            ExprType::Never(t) => t.span_with_id(),
            &ExprType::Any(span) => span,
        }
    }
}
impl<'a, 'i> Display for ExprType<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::Parenthesized(ExprTypeParenthesized { typ, .. }) => write!(f, "({typ})"),
            ExprType::String(_) => write!(f, "string"),
            ExprType::Int(_) => write!(f, "int"),
            ExprType::Float(_) => write!(f, "float"),
            ExprType::Bool(_) => write!(f, "bool"),
            ExprType::Unit(_) => write!(f, "()"),
            ExprType::UserType(ExprTypeUserType { name, generics, .. }) => {
                write!(f, "{}", name.ident)?;
                if let Some((_open, generics, _close)) = generics {
                    write!(f, "<{}>", generics)?;
                }
                Ok(())
            },
            ExprType::Generic(g) => write!(f, "{}<{},{},{}; {},{},{}>", g.ident.ident, g.def_ident.file_id(), g.def_ident.start(), g.def_ident.end(), g.ident.file_id(), g.ident.start(), g.ident.end()),
            ExprType::Function(fun) => Display::fmt(fun, f),
            ExprType::Never(_) => write!(f, "!"),
            ExprType::Any(_) => write!(f, "\"any\""),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprGenerics<'a, 'i> {
    pub open: TokenLessThan,
    pub generics: Option<Separated<'a, 'i, Generic<'i>, TokenComma>>,
    pub close: TokenGreaterThan,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprGenerics<'a, 'i> {
    pub fn new(open: TokenLessThan, generics: Option<Separated<'a, 'i, Generic<'i>, TokenComma>>, close: TokenGreaterThan) -> Self {
        ExprGenerics { open, generics, close, span: open.span | close.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprGenerics<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprGenerics::new(
            parser.parse(depth.next())?,
            parser.parse::<Option<Separated<NewGeneric, _>>>(depth.next())?.map(Separated::from),
            parser.parse(depth.last())?,
        ))
    }
}
impl<'a, 'i> Display for ExprGenerics<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.generics.iter().join(", "))
    }
}
impl<'a, 'i> Spanned for ExprGenerics<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}

#[derive(Debug, Clone, Eq)]
pub struct ExprLabel<'i> {
    pub apostrophe: TokenApostrophe,
    pub ident: TokenIdent<'i>,
    pub span: SpanWithId,
}
impl<'i> ExprLabel<'i> {
    pub fn new(apostrophe: TokenApostrophe, ident: TokenIdent<'i>) -> Self {
        ExprLabel { apostrophe, ident, span: apostrophe.span | ident.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprLabel<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprLabel::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
        ))
    }
}
impl<'i> Spanned for ExprLabel<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprLabel<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.ident.ident)
    }
}
impl<'i> PartialEq<ExprLabel<'i>> for ExprLabel<'i> {
    fn eq(&self, other: &ExprLabel<'i>) -> bool {
        self.ident.ident.eq(other.ident.ident)
    }
}
#[derive(Debug, Clone)]
pub struct ExprLabelDef<'i> {
    pub label: ExprLabel<'i>,
    pub colon: TokenColon,
    pub span: SpanWithId,
}
impl<'i> ExprLabelDef<'i> {
    pub fn new(label: ExprLabel<'i>, colon: TokenColon) -> Self {
        let span = label.span | colon.span;
        ExprLabelDef { label, colon, span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprLabelDef<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprLabelDef::new(
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'i> Spanned for ExprLabelDef<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprLabelDef<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:", self.label)
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "()")]
pub struct ExprUnit {
    pub open: TokenOpenParen,
    pub close: TokenCloseParen,
    pub span: SpanWithId,
}
impl ExprUnit {
    pub fn new(open: TokenOpenParen, close: TokenCloseParen) -> ExprUnit {
        ExprUnit { open, close, span: open.span | close.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprUnit {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprUnit::new(
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl Spanned for ExprUnit {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ExprVariable<'i> {
    pub binding: Binding<'i>,
    // The binding's span is from the definition site.
    // Variables are used for usage sites, where we need to store the span as well.
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprVariable<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let (binding, span) = Binding::parse_existing(parser, depth.last())?;
        Ok(ExprVariable { binding, span })
    }
}
impl<'i> Spanned for ExprVariable<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprVariable<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.binding)
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}", int.value)]
pub struct ExprInteger {
    pub int: TokenInteger,
}
impl<'a, 'i> Parse<'a, 'i> for ExprInteger {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprInteger {
            int: parser.parse(depth.last())?,
        })
    }
}
impl Spanned for ExprInteger {
    fn span_with_id(&self) -> SpanWithId {
        self.int.span
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}", float.value)]
pub struct ExprFloat {
    pub float: TokenFloat,
}
impl<'a, 'i> Parse<'a, 'i> for ExprFloat {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprFloat {
            float: parser.parse(depth.last())?,
        })
    }
}
impl Spanned for ExprFloat {
    fn span_with_id(&self) -> SpanWithId {
        self.float.span
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}", b.value)]
pub struct ExprBool {
    pub b: TokenBool,
}
impl<'a, 'i> Parse<'a, 'i> for ExprBool {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprBool {
            b: parser.parse(depth.last())?,
        })
    }
}
impl Spanned for ExprBool {
    fn span_with_id(&self) -> SpanWithId {
        self.b.span
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "{}", string.string)]
pub struct ExprString {
    pub string: TokenDqString,
}
impl<'a, 'i> Parse<'a, 'i> for ExprString {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprString {
            string: parser.parse(depth.last())?,
        })
    }
}
impl Spanned for ExprString {
    fn span_with_id(&self) -> SpanWithId {
        self.string.span
    }
}

#[derive(Debug, Clone)]
pub enum ExprFormatStringPart<'a, 'i> {
    Str(&'i str),
    Escaped(&'i str),
    /// expr, format-string specifier
    FmtArg(&'a Expr<'a, 'i>, Option<(TokenColon, Specifier, SpanWithId)>),
}
#[derive(Debug, Clone)]
pub struct ExprFormatString<'a, 'i> {
    pub parts: Vec<ExprFormatStringPart<'a, 'i>>,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprFormatString<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let mut fmtstr: TokenFormatString = parser.parse(depth.next())?;
        let mut parts = Vec::new();

        if fmtstr.rogue {
            fmtstr.parts.pop();
        }

        for part in &fmtstr.parts {
            match part {
                TokenFormatStringPart::Str(s) => parts.push(ExprFormatStringPart::Str(s)),
                TokenFormatStringPart::Escaped(s) => parts.push(ExprFormatStringPart::Escaped(s)),
                &TokenFormatStringPart::FormatArg(s, part_start) => {
                    let part_end = part_start + s.len();
                    let part_lexer = Lexer::new_in(parser.diagnostics, fmtstr.file_id(), part_start, part_end, LexerMode::UnexpectedCharacterEof);
                    let old_lexer = std::mem::replace(&mut parser.lexer, part_lexer);

                    let expr = parser.parse_scoped(depth.next())?;

                    let part_lexer = std::mem::replace(&mut parser.lexer, old_lexer);
                    let next = part_lexer.next();
                    let spec = match next {
                        Ok(Token::Eof(_)) => None,
                        Ok(Token::Colon(colon)) => {
                            let spec_str = &s[colon.end() - part_start..];
                            let spec_span = SpanWithId::new(colon.file_id(), colon.end(), colon.end() + spec_str.len());
                            let spec = match rt_format::parser::parse_specifier(spec_str, &mut util::NoValues) {
                                Ok(spec) => spec,
                                Err(()) => {
                                    parser.diagnostics.error(ErrorCode::InvalidFormatStringSpecifier)
                                        .with_error_label(Span::new(colon.file_id(), part_start, part_end), "this is not a valid format specifier")
                                        .emit();
                                    Specifier {
                                        format: Format::Display,
                                        ..Specifier::default()
                                    }
                                }
                            };
                            Some((colon, spec, spec_span))
                        }
                        _ => {
                            let arg_diagnostics_span = Span::new(fmtstr.file_id(), part_start, part_end);
                            let mut diag = parser.diagnostics.error(ErrorCode::InvalidFormatString)
                                .with_error_label(fmtstr.diagnostics_span(), "in this format string")
                                .with_error_label(arg_diagnostics_span, "in this format argument");
                            match next {
                                Ok(token) => diag = diag.with_info_label(arg_diagnostics_span, format!("expected end of format string, got `{}`", token)),
                                Err(e) => diag = diag.with_info_label(arg_diagnostics_span, format!("error: `{:?}`", e)),
                            }
                            diag.emit();
                            None
                        }
                    };
                    parts.push(ExprFormatStringPart::FmtArg(expr, spec));
                }
            }
        }

        Ok(ExprFormatString {
            parts,
            span: SpanWithId::from(fmtstr.span),
        })
    }
}
impl<'a, 'i> Spanned for ExprFormatString<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprFormatString<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "f\"")?;
        for part in &self.parts {
            match part {
                ExprFormatStringPart::Str(s) => write!(f, "{}", s)?,
                ExprFormatStringPart::Escaped(s) => write!(f, "\\{}", s)?,
                ExprFormatStringPart::FmtArg(s, spec) => {
                    write!(f, "{{{}", s)?;
                    if let Some((_colon, spec, _)) = spec {
                        write!(f, ":{}", spec)?;
                    }
                    write!(f, "}}")?;
                },
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprBind<'a, 'i> {
    pub let_token: TokenLet,
    pub pattern: ExprPattern<'a, 'i>,
    pub assign: TokenAssign,
    pub expr: &'a Expr<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprBind<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let let_token: TokenLet = parser.parse(depth.next())?;
        // hack to not have the pattern-binding in scope during the expression
        let guard = parser.push_scope(ScopeType::Synthetic);
        let pattern = parser.parse(depth.next())?;
        drop(guard);
        let assign = parser.parse(depth.next())?;
        let expr: &Expr = parser.parse(depth.last())?;
        // add pattern-binding as available from here on
        let binding = match &pattern {
            ExprPattern::Typed(typed) => typed.pattern.binding,
            ExprPattern::Untyped(untyped) => untyped.binding,
        };
        parser.add_binding(binding.ident, binding.mutable);
        let span = let_token.span | expr.span_with_id();
        Ok(ExprBind { let_token, pattern, assign, expr, span })
    }
}
impl<'a, 'i> Spanned for ExprBind<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprBind<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.pattern, self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprStaticSignature<'a, 'i> {
    pub static_token: TokenStatic,
    pub pattern: ExprPattern<'a, 'i>,
    pub assign: TokenAssign,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprStaticSignature<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let static_token: TokenStatic = parser.parse(depth.next())?;
        let pattern = parser.parse(depth.next())?;
        let assign: TokenAssign = parser.parse(depth.next())?;
        let span = static_token.span | assign.span;
        Ok(ExprStaticSignature { static_token, pattern, assign, span })
    }
}
impl<'a, 'i> Spanned for ExprStaticSignature<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprStaticSignature<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "static {} =", self.pattern)
    }
}
#[derive(Debug, Clone)]
pub struct ExprStatic<'a, 'i> {
    pub sig: ExprStaticSignature<'a, 'i>,
    pub expr: &'a Expr<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprStatic<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // add the static binding to the global scope instead of the inner scope
        let guard = parser.push_scope(ScopeType::Synthetic);
        let sig: ExprStaticSignature = parser.parse(depth.next())?;
        drop(guard);
        let expr: &Expr = parser.parse(depth.last())?;
        let span = sig.span | expr.span_with_id();
        Ok(ExprStatic { sig, expr, span })
    }
}
impl<'a, 'i> Spanned for ExprStatic<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprStatic<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.sig, self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprAssign<'a, 'i> {
    pub lhs: ExprAssignLhs<'a, 'i>,
    pub assign: TokenAssign,
    pub expr: &'a Expr<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprAssign<'a, 'i> {
    pub fn new(lhs: ExprAssignLhs<'a, 'i>, assign: TokenAssign, expr: &'a Expr<'a, 'i>) -> Self {
        let span = lhs.span_with_id() | expr.span_with_id();
        ExprAssign { lhs, assign, expr, span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprAssign<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprAssign::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'a, 'i> Spanned for ExprAssign<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprAssign<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.lhs, self.expr)
    }
}
#[derive(Debug, Clone, Display)]
pub enum ExprAssignLhs<'a, 'i> {
    Variable(ExprVariable<'i>),
    FieldAccess(ExprFieldAccess<'a, 'i>),
}
impl<'a, 'i> Parse<'a, 'i> for ExprAssignLhs<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let err1 = match ExprFieldAccess::parse(parser, depth.next()) {
            Ok(field_access) => return Ok(ExprAssignLhs::FieldAccess(field_access)),
            Err(e) => e,
        };
        let err2 = match ExprVariable::parse(parser, depth.last()) {
            Ok(variable) => return Ok(ExprAssignLhs::Variable(variable)),
            Err(e) => e,
        };
        Err(helper::last_error(&[err1, err2]))
    }
}
impl<'a, 'i> Spanned for ExprAssignLhs<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            ExprAssignLhs::Variable(v) => v.span_with_id(),
            ExprAssignLhs::FieldAccess(fa) => fa.span_with_id(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExprFieldAccess<'a, 'i> {
    pub variable: ExprVariable<'i>,
    pub dot: TokenDot,
    pub fields: Separated<'a, 'i, TokenIdent<'i>, TokenDot>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprFieldAccess<'a, 'i> {
    pub fn new(variable: ExprVariable<'i>, dot: TokenDot, fields: Separated<'a, 'i, TokenIdent<'i>, TokenDot>) -> Self {
        assert!(!fields.is_empty());
        assert!(!fields.is_terminated());
        let span = variable.span | fields.diagnostics_span().unwrap();
        ExprFieldAccess { variable, dot, fields, span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprFieldAccess<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let variable: ExprVariable = parser.parse(depth.next())?;
        let dot: TokenDot = parser.parse(depth.next())?;
        let fields: Separated<TokenIdent, TokenDot> = parser.parse(depth.last())?;
        // check length
        if fields.is_empty() {
            return Err(InternalError::Backtrack(Backtrack {
                span: parser.lexer.next_span(),
                expected: Cow::Borrowed( & [Expected::Token(TokenType::Ident)]),
            }));
        }
        // check trailing dot
        if fields.is_terminated() {
            parser.diagnostics.error(ErrorCode::InvalidExpression)
                .with_error_label(fields.diagnostics_span().unwrap(), "trailing dot")
                .emit();
        }
        Ok(ExprFieldAccess::new(variable, dot, fields))
    }
}
impl<'a, 'i> Spanned for ExprFieldAccess<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprFieldAccess<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.variable, self.fields.iter().map(|f| f.ident).join("."))
    }
}


#[derive(Debug, Clone)]
pub struct ExprAccess<'a, 'i> {
    pub variable: ExprVariable<'i>,
    pub dot: TokenDot,
    pub accesses: Separated<'a, 'i, FieldOrMethod<'a, 'i>, TokenDot>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprAccess<'a, 'i> {
    pub fn new(variable: ExprVariable<'i>, dot: TokenDot, accesses: Separated<'a, 'i, FieldOrMethod<'a, 'i>, TokenDot>) -> Self {
        assert!(!accesses.is_empty());
        assert!(!accesses.is_terminated());
        let span = variable.span | accesses.diagnostics_span().unwrap();
        ExprAccess { variable, dot, accesses, span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprAccess<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let variable: ExprVariable = parser.parse(depth.next())?;
        let dot: TokenDot = parser.parse(depth.next())?;
        let accesses: Separated<FieldOrMethod, TokenDot> = parser.parse(depth.last())?;
        // check length
        if accesses.is_empty() {
            return Err(InternalError::Backtrack(Backtrack {
                span: parser.lexer.next_span(),
                expected: Cow::Borrowed(&[Expected::Token(TokenType::Ident)]),
            }));
        }
        // check trailing dot
        if accesses.is_terminated() {
            parser.diagnostics.error(ErrorCode::InvalidExpression)
                .with_error_label(accesses.diagnostics_span().unwrap(), "trailing dot")
                .emit();
        }
        Ok(ExprAccess::new(variable, dot, accesses))
    }
}
impl<'a, 'i> Spanned for ExprAccess<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprAccess<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.variable, self.accesses.iter().join("."))
    }
}


#[derive(Debug, Clone)]
pub enum FieldOrMethod<'a, 'i> {
    Field(TokenIdent<'i>),
    Method(ExprMethodCall<'a, 'i>),
}
impl<'a, 'i> From<TokenIdent<'i>> for FieldOrMethod<'a, 'i> {
    fn from(ident: TokenIdent<'i>) -> Self {
        FieldOrMethod::Field(ident)
    }
}
impl<'a, 'i> Parse<'a, 'i> for FieldOrMethod<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let err1 = match ExprMethodCall::parse(parser, depth.next()) {
            Ok(fuction_call) => return Ok(FieldOrMethod::Method(fuction_call)),
            Err(e) => e,
        };
        let err2 = match TokenIdent::parse(parser, depth.last()) {
            Ok(field) => return Ok(FieldOrMethod::Field(field)),
            Err(e) => e,
        };
        Err(helper::last_error(&[err1, err2]))
    }
}
impl<'a, 'i> Spanned for FieldOrMethod<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            FieldOrMethod::Method(function_call) => function_call.span_with_id(),
            FieldOrMethod::Field(field) => field.span_with_id(),
        }
    }
}
impl<'a, 'i> Display for FieldOrMethod<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FieldOrMethod::Field(field) => Display::fmt(field, f),
            FieldOrMethod::Method(function_call) => Display::fmt(function_call, f),
        }
    }
}


#[derive(Debug, Clone)]
pub struct ExprBoolNot<'a, 'i> {
    pub bang: TokenBang,
    pub expr: &'a Expr<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprBoolNot<'a, 'i> {
    pub fn new(bang: TokenBang, expr: &'a Expr<'a, 'i>) -> Self {
        ExprBoolNot { bang, expr, span: bang.span | expr.span_with_id() }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprBoolNot<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprBoolNot::new(
            parser.parse(depth.next())?,
            Expr::try_parse_until_excluding(parser, ParseUntil::BooleanExpr, depth.last())?,
        ))
    }
}
impl<'a, 'i> Spanned for ExprBoolNot<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprBoolNot<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "!{}", self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprNeg<'a, 'i> {
    pub minus: TokenMinus,
    pub expr: &'a Expr<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprNeg<'a, 'i> {
    pub fn new(minus: TokenMinus, expr: &'a Expr<'a, 'i>) -> Self {
        ExprNeg { minus, expr, span: minus.span | expr.span_with_id() }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprNeg<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprNeg::new(
            parser.parse(depth.next())?,
            Expr::try_parse_until_excluding(parser, ParseUntil::Math, depth.last())?,
        ))
    }
}
impl<'a, 'i> Spanned for ExprNeg<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprNeg<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "-{}", self.expr)
    }
}

macro_rules! binop {
    ($($exprname:ident, $name:ident, $token:ident, $tokenvariant:ident $(, $assign_exprname:ident, $assign_token:ident)?;)+) => {
        $(
            #[derive(Debug, Clone)]
            pub struct $exprname<'a, 'i> {
                pub a: &'a Expr<'a, 'i>,
                pub op: $token,
                pub b: &'a Expr<'a, 'i>,
                pub span: SpanWithId,
            }
            impl<'a, 'i> $exprname<'a, 'i> {
                pub fn new(a: &'a Expr<'a, 'i>, op: $token, b: &'a Expr<'a, 'i>) -> $exprname<'a, 'i> {
                    Self { a, op, b, span: a.span_with_id() | b.span_with_id() }
                }
                pub(in crate::parser) fn new_as_expr(a: &'a Expr<'a, 'i>, op: Token<'i>, b: &'a Expr<'a, 'i>) -> Expr<'a, 'i> {
                    let op = match op {
                        Token::$tokenvariant(token @ $token { .. }) => token,
                        _ => unreachable!(),
                    };
                    Expr::$name(Self::new(a, op, b))
                }
            }
            impl<'a, 'i> Parse<'a, 'i> for $exprname<'a, 'i> {
                fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
                    Ok($exprname::new(
                        parser.parse(depth.next())?,
                        parser.parse(depth.next())?,
                        parser.parse(depth.next())?,
                    ))
                }
            }
            impl<'a, 'i> Spanned for $exprname<'a, 'i> {
                fn span_with_id(&self) -> SpanWithId {
                    self.span
                }
            }
            impl<'a, 'i> Display for $exprname<'a, 'i> {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    write!(f, "{} {}{}", self.a, self.op, self.b)
                }
            }
            $(
                #[derive(Debug, Clone)]
                pub struct $assign_exprname<'a, 'i> {
                    pub lhs: ExprAssignLhs<'a, 'i>,
                    pub op: $assign_token,
                    pub assign: TokenAssign,
                    pub expr: &'a Expr<'a, 'i>,
                    pub span: SpanWithId,
                }
                impl<'a, 'i> Parse<'a, 'i> for $assign_exprname<'a, 'i> {
                    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
                        let lhs: ExprAssignLhs = parser.parse(depth.next())?;
                        let op = parser.parse(depth.next())?;
                        let assign = parser.parse(depth.next())?;
                        let expr: &Expr = parser.parse(depth.next())?;
                        let span = lhs.span_with_id() | expr.span_with_id();
                        Ok($assign_exprname { lhs, op, assign, expr, span })
                    }
                }
                impl<'a, 'i> Spanned for $assign_exprname<'a, 'i> {
                    fn span_with_id(&self) -> SpanWithId {
                        self.span
                    }
                }
                impl<'a, 'i> Display for $assign_exprname<'a, 'i> {
                    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                        write!(f, "{} {}= {}", self.lhs, &self.op.to_string()[..1], self.expr)
                    }
                }
            )?
        )+
    }
}
binop! {
    ExprAdd, Add, TokenPlus, Plus, ExprAddAssign, TokenPlus;
    ExprSub, Sub, TokenMinus, Minus, ExprSubAssign, TokenMinus;
    ExprMul, Mul, TokenStar, Star, ExprMulAssign, TokenStar;
    ExprDiv, Div, TokenSlash, Slash, ExprDivAssign, TokenSlash;
    ExprMod, Mod, TokenPercent, Percent, ExprModAssign, TokenPercent;
    ExprXor, Xor, TokenCircumflex, Circumflex, ExprXorAssign, TokenCircumflex;
    ExprBoolAnd, BoolAnd, TokenDoubleAmp, DoubleAmp, ExprBoolAndAssign, TokenAmp;
    ExprBoolOr, BoolOr, TokenDoublePipe, DoublePipe, ExprBoolOrAssign, TokenPipe;
    ExprLessThan, LessThan, TokenLessThan, LessThan;
    ExprLessEquals, LessEquals, TokenLessEquals, LessEquals;
    ExprEquals, Equals, TokenEquals, Equals;
    ExprNotEquals, NotEquals, TokenNotEquals, NotEquals;
    ExprGreaterEquals, GreaterEquals, TokenGreaterEquals, GreaterEquals;
    ExprGreaterThan, GreaterThan, TokenGreaterThan, GreaterThan;
}

#[derive(Debug, Clone)]
pub struct ExprBlock<'a, 'i> {
    pub open: TokenOpenCurly,
    pub body: BlockBody<'a, 'i>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprBlock<'a, 'i> {
    pub fn new(open: TokenOpenCurly, body: BlockBody<'a, 'i>, close: TokenCloseCurly) -> Self {
        ExprBlock { open, body, close, span: open.span | close.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprBlock<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // TODO
        // self.consume_until(&[TokenType::Semicolon, TokenType::CloseCurly]);

        // self.diagnostics.error(ErrorCode::MissingSemicolon)
        //     .with_info_label(Span::new(span.file, span.end, span.end), "try adding a semicolon here")
        //     .emit(),
        //

        // let span = Span::new(start_span.file, start_span.start, body.last().map(|e| e.span.end).unwrap_or(start_span.start));
        // self.diagnostics.error(ErrorCode::UnclosedBlock)
        //     .with_error_label(span, "unclosed block")
        //     .emit();
        let open: TokenOpenCurly = parser.parse(depth.next())?;
        let body = parser.parse_scoped(depth.next())?;
        let close: TokenCloseCurly = parser.parse(depth.last())?;
        let span = open.span | close.span;
        Ok(ExprBlock { open, body, close, span })
    }
}
impl<'a, 'i> Spanned for ExprBlock<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprBlock<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.body.exprs.is_empty() {
            return write!(f, "{{}}");
        }
        writeln!(f, "{{")?;
        let mut padded = PadFmt::new(&mut *f);
        for (i, expr) in self.body.exprs.iter().enumerate() {
            write!(&mut padded, "{}", expr)?;
            let no_semicolon = (i == self.body.exprs.len() - 1 && !self.body.terminated_with_semicolon)
                || expr.is_item();
            if no_semicolon {
                writeln!(padded)?;
            } else {
                writeln!(padded, ";")?;
            }
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct BlockBody<'a, 'i> {
    pub exprs: Vec<&'a Expr<'a, 'i>>,
    pub terminated_with_semicolon: bool,
}
impl<'a, 'i> Parse<'a, 'i> for BlockBody<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        enum Last {
            Terminated,
            TerminatedWithSemicolon,
            Unterminated(Span),
        }

        let mut exprs = Vec::new();
        let mut last = Last::TerminatedWithSemicolon;

        while !parser.lexer.is_empty() && parser.peek_token(0).unwrap().typ() != TokenType::Eof && parser.peek_token(0).unwrap().typ() != TokenType::CloseCurly {
            let expr = Expr::try_parse_until_including(parser, ParseUntil::All, depth.next())?;

            let trailing_semicolon = match parser.peek_token(0)? {
                Token::Semicolon(_) => {
                    drop(parser.next_token());
                    true
                }
                _ => false,
            };

            // handle missing semicolon
            match last {
                Last::Terminated | Last::TerminatedWithSemicolon => (),
                Last::Unterminated(span) => parser.diagnostics.error(ErrorCode::MissingSemicolon)
                    .with_info_label(span, "try adding a semicolon here")
                    .emit(),
            }
            if trailing_semicolon {
                last = Last::TerminatedWithSemicolon;
            } else {
                last = match expr.is_item() {
                    true => Last::Terminated,
                    false => Last::Unterminated(expr.diagnostics_span()),
                };
            }

            exprs.push(expr);
        }
        Ok(BlockBody {
            exprs,
            terminated_with_semicolon: matches!(last, Last::TerminatedWithSemicolon),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ExprParenthesized<'a, 'i> {
    pub open: TokenOpenParen,
    pub expr: &'a Expr<'a, 'i>,
    pub close: TokenCloseParen,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprParenthesized<'a, 'i> {
    pub fn new(open: TokenOpenParen, expr: &'a Expr<'a, 'i>, close: TokenCloseParen) -> Self {
        ExprParenthesized { open, expr, close, span: open.span | close.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprParenthesized<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // TODO
        // self.diagnostics.error(ErrorCode::UnclosedParen)
        //     .with_error_label(span, "unclosed parenthesis")
        //     .with_info_label(Span::new(expr.span.file, expr.span.end, expr.span.end), "try inserting a `)` here")
        //     .emit();
        Ok(ExprParenthesized::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'a, 'i> Spanned for ExprParenthesized<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprParenthesized<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprIfElse<'a, 'i> {
    pub if_token: TokenIf,
    pub condition: &'a Expr<'a, 'i>,
    pub then: ExprBlock<'a, 'i>,
    pub else_ifs: Vec<(TokenElse, TokenIf, &'a Expr<'a, 'i>, ExprBlock<'a, 'i>)>,
    pub els: Option<(TokenElse, ExprBlock<'a, 'i>)>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprIfElse<'a, 'i> {
    pub fn iter_branches(&self) -> impl Iterator<Item = (Option<&'a Expr<'a, 'i>>, &ExprBlock<'a, 'i>)> {
        ::std::iter::once((Some(self.condition), &self.then))
            .chain(self.else_ifs.iter().map(|(_, _, cond, block)| (Some(*cond), block)))
            .chain(self.els.iter().map(|(_, block)| (None, block)))
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprIfElse<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let if_token: TokenIf = parser.parse(depth.next())?;
        let condition = Expr::try_parse_until_including(parser, ParseUntil::All, depth.next())?;
        let then: ExprBlock = parser.parse(depth.next())?;
        let else_ifs: Vec<(_, _, _, ExprBlock)> = parser.parse(depth.next())?;
        let els: Option<(TokenElse, ExprBlock)> = parser.parse(depth.last())?;

        let span = if_token.span
            | then.span
            | else_ifs.last().map(|(_, _, _, block)| block.span_with_id())
            | els.as_ref().map(|(_, block)| block.span_with_id());
        Ok(ExprIfElse { if_token, condition, then, else_ifs, els, span })
    }
}
impl<'a, 'i> Spanned for ExprIfElse<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprIfElse<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "if {} {}", self.condition, self.then)?;
        for (_, _, cond, block) in &self.else_ifs {
            write!(f, "else if {} {}", cond, block)?;
        }
        if let Some((_, block)) = &self.els {
            write!(f, "else {}", block)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprMatch<'a, 'i> {
    pub match_token: TokenMatch,
    pub expr: &'a Expr<'a, 'i>,
    pub open: TokenOpenCurly,
    pub arms: Vec<(ExprMatchPattern<'a, 'i>, TokenFatArrow, &'a Expr<'a, 'i>)>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprMatch<'a, 'i> {
    pub fn new(match_token: TokenMatch, expr: &'a Expr<'a, 'i>, open: TokenOpenCurly, arms: Vec<(ExprMatchPattern<'a, 'i>, TokenFatArrow, &'a Expr<'a, 'i>)>, close: TokenCloseCurly) -> Self {
        ExprMatch { match_token, expr, open, arms, close, span: match_token.span | close.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprMatch<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprMatch::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse::<Separated<'a, 'i, Scoped<(ExprMatchPattern<'a, 'i>, TokenFatArrow, &'a Expr<'a, 'i>)>, TokenComma>>(depth.next())?
                .into_iter().map(|Scoped((pattern, arrow, expr))| (pattern, arrow, expr)).collect(),
            parser.parse(depth.last())?,
        ))
    }
}
impl<'a, 'i> Spanned for ExprMatch<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprMatch<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "match ({}) ", self.expr)?;
        if self.arms.is_empty() {
            return write!(f, "{{}}");
        }
        writeln!(f, "{{")?;
        let mut padded = PadFmt::new(&mut *f);
        for (pat, _, expr) in &self.arms {
            writeln!(padded, "{} => {},", pat, expr)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprWhile<'a, 'i> {
    pub label: Option<ExprLabelDef<'i>>,
    pub while_token: TokenWhile,
    pub condition: &'a Expr<'a, 'i>,
    pub block: ExprBlock<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprWhile<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let label: Option<ExprLabelDef<'i>> = parser.parse(depth.next())?;
        let while_token: TokenWhile = parser.parse(depth.next())?;
        let condition = Expr::try_parse_until_including(parser, ParseUntil::All, depth.next())?;
        let _guard = parser.push_scope(ScopeType::While(label.clone()));
        let block: ExprBlock = parser.parse(depth.next())?;
        let span = label.as_ref().map(Spanned::span_with_id) | while_token.span | block.span;
        Ok(ExprWhile { label, while_token, condition, block, span })
    }
}
impl<'a, 'i> Spanned for ExprWhile<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprWhile<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "{} ", label)?;
        }
        write!(f, "while {} {}", self.condition, self.block)
    }
}

#[derive(Debug, Clone)]
pub struct ExprFor<'a, 'i> {
    pub label: Option<ExprLabelDef<'i>>,
    pub for_token: TokenFor,
    pub binding: Binding<'i>,
    pub in_token: TokenIn,
    pub expr: &'a Expr<'a, 'i>,
    pub block: ExprBlock<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprFor<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let label: Option<ExprLabelDef<'i>> = parser.parse(depth.next())?;
        let for_token: TokenFor = parser.parse(depth.next())?;
        // don't have the binding in the current scope in the expr
        let guard = parser.push_scope(ScopeType::Synthetic);
        let binding = Binding::parse_new(parser, depth.next())?;
        drop(guard);
        let in_token = parser.parse(depth.next())?;
        let expr = Expr::try_parse_until_including(parser, ParseUntil::All, depth.next())?;
        let _guard = parser.push_scope(ScopeType::For(label.clone()));
        let added_binding = parser.add_binding(binding.ident, binding.mutable);
        assert_eq!(added_binding, binding);
        let block: ExprBlock = parser.parse(depth.next())?;
        let span = label.as_ref().map(Spanned::span_with_id) | for_token.span | block.span;
        Ok(ExprFor { label, for_token, binding, in_token, expr, block, span })
    }
}
impl<'a, 'i> Spanned for ExprFor<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprFor<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "{} ", label)?;
        }
        write!(f, "for {} in {} {}", self.binding, self.expr, self.block)
    }
}

#[derive(Debug, Clone)]
pub struct ExprLoop<'a, 'i> {
    pub label: Option<ExprLabelDef<'i>>,
    pub loop_token: TokenLoop,
    pub block: ExprBlock<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprLoop<'a, 'i> {
    pub fn new(label: Option<ExprLabelDef<'i>>, loop_token: TokenLoop, block: ExprBlock<'a, 'i>) -> Self {
        let span = label.as_ref().map(Spanned::span_with_id) | loop_token.span | block.span;
        ExprLoop { label, loop_token, block, span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprLoop<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let label: Option<ExprLabelDef<'i>> = parser.parse(depth.next())?;
        let loop_token = parser.parse(depth.next())?;
        let _guard = parser.push_scope(ScopeType::Loop(label.clone()));
        let block = parser.parse(depth.next())?;
        Ok(ExprLoop::new(label, loop_token, block))
    }
}
impl<'a, 'i> Spanned for ExprLoop<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprLoop<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "{} ", label)?;
        }
        write!(f, "loop {}", self.block)
    }
}

#[derive(Debug, Clone)]
pub struct ExprBreak<'a, 'i> {
    pub break_token: TokenBreak,
    pub label: Option<ExprLabel<'i>>,
    pub expr: Option<&'a Expr<'a, 'i>>,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprBreak<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let break_token: TokenBreak = parser.parse(depth.next())?;
        let label: Option<ExprLabel> = parser.parse(depth.next())?;
        let expr: Option<&Expr> = parser.parse(depth.last())?;
        let span = break_token.span | label.as_ref().map(Spanned::span_with_id) | expr.map(Spanned::span_with_id);
        Ok(ExprBreak { break_token, label, expr, span })
    }
}
impl<'a, 'i> Spanned for ExprBreak<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprBreak<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "break")?;
        if let Some(label) = &self.label {
            write!(f, " {}", label)?;
        }
        if let Some(expr) = &self.expr {
            write!(f, " {}", expr)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprContinue<'i> {
    pub continue_token: TokenContinue,
    pub label: Option<ExprLabel<'i>>,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprContinue<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let continue_token: TokenContinue = parser.parse(depth.next())?;
        let label: Option<ExprLabel> = parser.parse(depth.next())?;
        let span = continue_token.span | label.as_ref().map(Spanned::span_with_id);
        Ok(ExprContinue { continue_token, label, span })
    }
}
impl<'i> Spanned for ExprContinue<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprContinue<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "continue")?;
        if let Some(label) = &self.label {
            write!(f, " {}", label)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprReturn<'a, 'i> {
    pub return_token: TokenReturn,
    pub expr: Option<&'a Expr<'a, 'i>>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprReturn<'a, 'i> {
    pub fn new(return_token: TokenReturn, expr: Option<&'a Expr<'a, 'i>>) -> Self {
        ExprReturn { return_token, expr, span: return_token.span | expr.map(Spanned::span_with_id) }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprReturn<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprReturn::new(
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'a, 'i> Spanned for ExprReturn<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprReturn<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "return")?;
        if let Some(expr) = &self.expr {
            write!(f, " {}", expr)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprYield<'a, 'i> {
    pub yield_token: TokenYield,
    pub expr: Option<&'a Expr<'a, 'i>>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprYield<'a, 'i> {
    pub fn new(yield_token: TokenYield, expr: Option<&'a Expr<'a, 'i>>) -> Self {
        ExprYield { yield_token, expr, span: yield_token.span | expr.map(|e| e.span_with_id()) }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprYield<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprYield::new(
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'a, 'i> Spanned for ExprYield<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprYield<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "yield")?;
        if let Some(expr) = &self.expr {
            write!(f, " {}", expr)?;
        }
        Ok(())
    }
}

pub type ExprFunctionCall<'a, 'i> = ExprFunctionOrMethodCall<'a, 'i, ExprVariable<'i>>;
pub type ExprMethodCall<'a, 'i> = ExprFunctionOrMethodCall<'a, 'i, TokenIdent<'i>>;
#[derive(Debug, Clone)]
pub struct ExprFunctionOrMethodCall<'a, 'i, T> {
    pub name: T,
    pub open: TokenOpenParen,
    pub args: Separated<'a, 'i, &'a Expr<'a, 'i>, TokenComma>,
    pub close: TokenCloseParen,
    pub span: SpanWithId,
}
impl<'a, 'i, T: Spanned> ExprFunctionOrMethodCall<'a, 'i, T> {
    pub fn new(name: T, open: TokenOpenParen, args: Separated<'a, 'i, &'a Expr<'a, 'i>, TokenComma>, close: TokenCloseParen) -> Self {
        let span = name.span_with_id() | close.span;
        ExprFunctionOrMethodCall { name, open, args, close, span }
    }
}
impl<'a, 'i, T: Parse<'a, 'i> + Spanned> Parse<'a, 'i> for ExprFunctionOrMethodCall<'a, 'i, T> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprFunctionOrMethodCall::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'a, 'i, T> Spanned for ExprFunctionOrMethodCall<'a, 'i, T> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i, T: Display> Display for ExprFunctionOrMethodCall<'a, 'i, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.args)
    }
}

#[derive(Debug, Clone)]
pub struct ExprFunctionType<'a, 'i> {
    pub fn_token: TokenFn,
    pub generics: Option<ExprGenerics<'a, 'i>>,
    pub open: TokenOpenParen,
    pub args: Separated<'a, 'i, ExprType<'a, 'i>, TokenComma>,
    pub close: TokenCloseParen,
    pub ret_type: Option<(TokenArrow, ExprType<'a, 'i>)>,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprFunctionType<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // function types can use the surrounding generics
        // but they are desugared into becoming part of the function's generics
        let existing_generics: Vec<_> = parser.generics();
        // scope for generics
        let _scope_guard = parser.push_scope(ScopeType::Synthetic);

        let fn_token: TokenFn = parser.parse(depth.next())?;
        let mut generics: Option<ExprGenerics> = parser.parse(depth.next())?;
        if !existing_generics.is_empty() {
            let generics_separated = match &mut generics {
                None => {
                    generics = Some(ExprGenerics {
                        open: TokenLessThan { span: fn_token.span.end_span() },
                        generics: Some(Separated::default()),
                        close: TokenGreaterThan { span: fn_token.span.end_span() },
                        span: fn_token.span.end_span(),
                    });
                    generics.as_mut().unwrap().generics.as_mut().unwrap()
                },
                Some(g) => g.generics.get_or_insert_with(Separated::default),
            };

            // prepend existing generics
            for existing in existing_generics {
                let comma = TokenComma {
                    span: existing.span_with_id().end_span(),
                };
                generics_separated.push_front(existing, Some(comma));
            }
        }
        let open = parser.parse(depth.next())?;
        let args = parser.parse(depth.next())?;
        let close: TokenCloseParen = parser.parse(depth.next())?;
        let ret_type: Option<(TokenArrow, ExprType)> = parser.parse(depth.next())?;
        let span = fn_token.span | close.span | ret_type.as_ref().map(|(_, t)| t.span_with_id());
        Ok(ExprFunctionType { fn_token, generics, open, args, close, ret_type, span })
    }
}
impl<'a, 'i> Spanned for ExprFunctionType<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprFunctionType<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn")?;
        if let Some(generics) = &self.generics {
            write!(f, "{}", generics)?;
        }
        write!(f, "({})", self.args)?;
        if let Some((_arrow, ret_type)) = &self.ret_type {
            write!(f, " -> {}", ret_type)?;
        }
        Ok(())
    }
}
#[derive(Debug, Clone)]
pub struct ExprFunctionSignature<'a, 'i> {
    pub gen_token: Option<TokenGen>,
    pub fn_token: TokenFn,
    pub name: Option<TokenIdent<'i>>,
    pub generics: Option<ExprGenerics<'a, 'i>>,
    pub open: TokenOpenParen,
    pub self_arg: Option<Binding<'i>>,
    pub self_arg_comma: Option<TokenComma>,
    pub args: Separated<'a, 'i, ExprPatternTyped<'a, 'i>, TokenComma>,
    pub varargs: Option<(Option<ExprType<'a, 'i>>, TokenDotDotDot)>,
    pub close: TokenCloseParen,
    pub ret_type: Option<(TokenArrow, ExprType<'a, 'i>)>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprFunctionSignature<'a, 'i> {
    pub fn new(gen_token: Option<TokenGen>, fn_token: TokenFn, name: Option<TokenIdent<'i>>, generics: Option<ExprGenerics<'a, 'i>>, open: TokenOpenParen, self_arg: Option<Binding<'i>>, self_arg_comma: Option<TokenComma>, args: Separated<'a, 'i, ExprPatternTyped<'a, 'i>, TokenComma>, varargs: Option<(Option<ExprType<'a, 'i>>, TokenDotDotDot)>, close: TokenCloseParen, ret_type: Option<(TokenArrow, ExprType<'a, 'i>)>) -> Self {
        let span = gen_token.as_ref().map(Spanned::span_with_id) | fn_token.span | close.span | ret_type.as_ref().map(|(_, typ)| typ.span_with_id());
        ExprFunctionSignature { gen_token, fn_token, name, generics, open, self_arg, self_arg_comma, args, varargs, close, ret_type, span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprFunctionSignature<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let gen_token = parser.parse(depth.next())?;
        let fn_token = parser.parse(depth.next())?;
        let name = parser.parse(depth.next())?;
        let generics = parser.parse(depth.next())?;
        let open = parser.parse(depth.next())?;
        type SelfWithArgs<'a, 'i> = Option<(SelfBinding<'i>, Option<(TokenComma, Separated<'a, 'i, ExprPatternTyped<'a, 'i>, TokenComma>)>)>;
        let self_with_args: SelfWithArgs<'a, 'i> = parser.parse(depth.next())?;
        let (self_arg, self_arg_comma, args) = match self_with_args {
            Some((self_binding, rest)) => match rest {
                Some((comma, args)) => (Some(self_binding.b), Some(comma), args),
                None => (Some(self_binding.b), None, Separated::default()),
            }
            None => (None, None, parser.parse(depth.next())?),
        };
        let varargs = parser.parse(depth.next())?;
        let close = parser.parse(depth.next())?;
        let ret_type = parser.parse(depth.last())?;
        Ok(ExprFunctionSignature::new(gen_token, fn_token, name, generics, open, self_arg, self_arg_comma, args, varargs, close, ret_type))
    }
}
impl<'a, 'i> Spanned for ExprFunctionSignature<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprFunctionSignature<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn")?;
        if let Some(name) = self.name {
            write!(f, " {}", name.ident)?;
        }
        if let Some(generics) = &self.generics {
            write!(f, "{}", generics)?;
        }
        // self-arg is duplicated into the args - no need to use self.self_arg here
        write!(f, "({})", self.args)?;
        if let Some((_arrow, ret_type)) = &self.ret_type {
            write!(f, " -> {}", ret_type)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprFunctionDefinition<'a, 'i> {
    pub sig: ExprFunctionSignature<'a, 'i>,
    pub captures: IndexSet<Binding<'i>>,
    pub body: ExprBlock<'a, 'i>,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprFunctionDefinition<'a, 'i> {
    /// WARNING: Doesn't handle generator transformation
    pub fn new(sig: ExprFunctionSignature<'a, 'i>, captures: IndexSet<Binding<'i>>, body: ExprBlock<'a, 'i>) -> Self {
        let span = sig.span | body.span;
        ExprFunctionDefinition { sig, captures, body, span }
    }
    pub fn arg_diagnostics_span(&self) -> Span {
        Span::new(self.sig.open.file_id(), self.sig.open.start(), self.sig.close.end())
    }
    fn parse_with_generics(parser: &mut Parser<'a, '_, 'i>, depth: Depth, generics: &[Generic<'i>]) -> Result<Self, InternalError> {
        // scope for generics and argument-bindings
        let scope_guard = parser.push_scope(ScopeType::Function);

        for &generic in generics {
            parser.add_generic(generic);
        }
        trace!("{} ExprFunctionDefinition::parse_with_generics ({:?})        ({:?})", depth, parser.generic_names(), parser.peek_token(0));
        let result = Self::parse_internal(parser, depth.next());
        drop(scope_guard);
        result
    }
    fn parse_internal(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        trace!("{} ExprFunctionDefinition::parse_internal        ({:?})", depth, parser.peek_token(0));
        let mark = parser.lexer.mark();
        let sig: ExprFunctionSignature = parser.parse(depth.next())?;
        let old_captures = parser.captures.replace(IndexSet::new());
        let body = parser.parse(depth.last());
        let captures = std::mem::replace(&mut parser.captures, old_captures).unwrap();
        let body = body?;
        mark.apply();
        let fun = ExprFunctionDefinition::new(sig, captures, body);
        Ok(match fun.sig.gen_token {
            Some(gen_token) => {
                let fun = generator_transform::transform_generator(parser, gen_token, fun);
                if let Some(ident) = fun.sig.name {
                    parser.meta_info.generators.insert(ident.ident.to_string(), (String::new(), fun.to_string()));
                }
                fun
            },
            None => fun
        })
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprFunctionDefinition<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Self::parse_with_generics(parser, depth, &[])
    }
}
impl<'a, 'i> Spanned for ExprFunctionDefinition<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprFunctionDefinition<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.sig, self.body)
    }
}

pub type ExprStructDefFields<'a, 'i> = Separated<'a, 'i, (TokenIdent<'i>, TokenColon, ExprType<'a, 'i>), TokenComma>;
#[derive(Debug, Clone)]
pub struct ExprStructDefinition<'a, 'i> {
    pub struct_token: TokenStruct,
    pub name: TokenIdent<'i>,
    pub generics: Option<ExprGenerics<'a, 'i>>,
    pub open: TokenOpenCurly,
    pub fields: ExprStructDefFields<'a, 'i>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprStructDefinition<'a, 'i> {
    pub fn new(struct_token: TokenStruct, name: TokenIdent<'i>, generics: Option<ExprGenerics<'a, 'i>>, open: TokenOpenCurly, fields: ExprStructDefFields<'a, 'i>, close: TokenCloseCurly) -> Self {
        ExprStructDefinition { struct_token, name, generics, open, fields, close, span: struct_token.span | close.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprStructDefinition<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // scope for generics
        let _scope_guard = parser.push_scope(ScopeType::Synthetic);
        Ok(ExprStructDefinition::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'a, 'i> Spanned for ExprStructDefinition<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprStructDefinition<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {}", self.name.ident)?;
        if let Some(generics) = &self.generics {
            write!(f, "{}", generics)?;
        }
        writeln!(f, " {{")?;
        let mut padfmt = PadFmt::new(&mut *f);
        for (name, _colon, typ) in &self.fields {
            writeln!(padfmt, "{}: {},", name.ident, typ)?;
        }
        write!(f, "}}")
    }
}

pub type ExprStructInitFields<'a, 'i> = Separated<'a, 'i, (TokenIdent<'i>, TokenColon, &'a Expr<'a, 'i>), TokenComma>;
#[derive(Debug, Clone)]
pub struct ExprStructInitialization<'a, 'i> {
    pub name: TokenIdent<'i>,
    pub open: TokenOpenCurly,
    pub fields: ExprStructInitFields<'a, 'i>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprStructInitialization<'a, 'i> {
    pub fn new(name: TokenIdent<'i>, open: TokenOpenCurly, fields: ExprStructInitFields<'a, 'i>, close: TokenCloseCurly) -> Self {
        ExprStructInitialization { name, open, fields, close, span: name.span | close.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprStructInitialization<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let name: TokenIdent = parser.parse(depth.next())?;
        if parser.meta_info.user_types.get(name.ident).is_none() {
            return Err(InternalError::Backtrack(Backtrack {
                span: name.diagnostics_span(),
                expected: Cow::Borrowed(&[Expected::Token(TokenType::Ident)]),
            }));
        }
        let open = parser.parse(depth.next())?;
        let fields: ExprStructInitFields = parser.parse(depth.next())?;
        let close: TokenCloseCurly = parser.parse(depth.next())?;
        Ok(ExprStructInitialization::new(name, open, fields, close))
    }
}
impl<'a, 'i> Spanned for ExprStructInitialization<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprStructInitialization<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.name.ident)?;
        if self.fields.is_empty() {
            return write!(f, "{{}}");
        }
        writeln!(f, "{{ ")?;
        let mut padfmt = PadFmt::new(&mut *f);
        for (name, _comma, expr) in &self.fields {
            writeln!(padfmt, "{}: {},", name.ident, expr)?;
        }
        writeln!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct ExprEnumDefinition<'a, 'i> {
    pub enum_token: TokenEnum,
    pub name: TokenIdent<'i>,
    pub generics: Option<ExprGenerics<'a, 'i>>,
    pub open: TokenOpenCurly,
    pub variants: Separated<'a, 'i, ExprEnumVariant<'a, 'i>, TokenComma>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprEnumDefinition<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // scope for generics
        let _scope_guard = parser.push_scope(ScopeType::Synthetic);
        let enum_token: TokenEnum = parser.parse(depth.next())?;
        let name = parser.parse(depth.next())?;
        let generics = parser.parse(depth.next())?;
        let open = parser.parse(depth.next())?;
        let variants = parser.parse(depth.next())?;
        let close: TokenCloseCurly = parser.parse(depth.last())?;
        let span = enum_token.span | close.span;
        Ok(ExprEnumDefinition { enum_token, name, generics, open, variants, close, span })
    }
}
impl<'a, 'i> Spanned for ExprEnumDefinition<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprEnumDefinition<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "enum {}", self.name.ident)?;
        if let Some(generics) = &self.generics {
            write!(f, "{}", generics)?;
        }
        writeln!(f, " {{")?;
        let mut padded = PadFmt::new(&mut *f);
        for variant in &self.variants {
            writeln!(padded, "{}", variant)?;
        }
        writeln!(f, "}}")
    }
}
#[derive(Debug, Clone)]
pub struct ExprEnumVariant<'a, 'i> {
    pub name: TokenIdent<'i>,
    pub fields: Option<(TokenOpenParen, Separated<'a, 'i, ExprType<'a, 'i>, TokenComma>, TokenCloseParen)>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprEnumVariant<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprEnumVariant {
            name: parser.parse(depth.next())?,
            fields: parser.parse(depth.last())?,
        })
    }
}
impl<'a, 'i> Display for ExprEnumVariant<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name.ident)?;
        if let Some((_open, fields, _close)) = &self.fields {
            let joined = fields.iter().map(|typ| typ.to_string()).join(", ");
            write!(f, "({})", joined)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprEnumInitialization<'i> {
    pub enum_name: TokenIdent<'i>,
    pub double_colon: TokenDoubleColon,
    pub variant_name: TokenIdent<'i>,
    pub span: SpanWithId,
}
impl<'i> ExprEnumInitialization<'i> {
    pub fn new(enum_name: TokenIdent<'i>, double_colon: TokenDoubleColon, variant_name: TokenIdent<'i>) -> Self {
        ExprEnumInitialization { enum_name, double_colon, variant_name, span: enum_name.span | variant_name.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprEnumInitialization<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let enum_name: TokenIdent = parser.parse(depth.next())?;
        let double_colon = parser.parse(depth.next())?;
        let variant_name: TokenIdent = parser.parse(depth.last())?;
        match parser.meta_info.user_types.get(enum_name.ident) {
            Some(UserType::Enum(e)) if e.variants.iter().any(|v| v.name.ident == variant_name.ident && v.fields.is_none()) => {
                Ok(ExprEnumInitialization::new(enum_name, double_colon, variant_name))
            }
            _ => Err(InternalError::Backtrack(Backtrack {
                span: enum_name.diagnostics_span(),
                expected: Cow::Borrowed(&[Expected::Token(TokenType::Ident)]),
            })),
        }
    }
}
impl<'i> Spanned for ExprEnumInitialization<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprEnumInitialization<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.enum_name.ident, self.variant_name.ident)
    }
}

#[derive(Clone, Debug)]
pub struct ExprImplBlock<'a, 'i> {
    pub impl_token: TokenImpl,
    pub name: TokenIdent<'i>,
    pub generics: Option<ExprGenerics<'a, 'i>>,
    pub open: TokenOpenCurly,
    pub functions: Vec<ExprFunctionDefinition<'a, 'i>>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'a, 'i> ExprImplBlock<'a, 'i> {
    pub fn new(impl_token: TokenImpl, name: TokenIdent<'i>, generics: Option<ExprGenerics<'a, 'i>>, open: TokenOpenCurly, functions: Vec<ExprFunctionDefinition<'a, 'i>>, close: TokenCloseCurly) -> Self {
        ExprImplBlock { impl_token, name, generics, open, functions, close, span: impl_token.span | close.span }
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprImplBlock<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // scope for generics
        let _scope_guard = parser.push_scope(ScopeType::Synthetic);
        let impl_token = parser.parse(depth.next())?;
        let name: TokenIdent = parser.parse(depth.next())?;
        let mut generics: Option<ExprGenerics> = parser.parse(depth.next())?;
        let open = parser.parse(depth.next())?;

        // fix generics to point to their original struct / enum generic
        (|| {
            let user_type = match parser.meta_info.user_types.get(name.ident) {
                Some(user_type) => user_type,
                None => return,
            };
            let expected_generics = user_type.generics();
            let expected_generic_span = expected_generics.map(|g| g.span_with_id()).unwrap_or_else(|| user_type.name_span().end_span());
            let expected_generics = expected_generics.and_then(|g| g.generics.as_ref());
            let expected_generic_iter = match expected_generics {
                Some(generics) => Either::Left(generics.iter().map(Some).chain(std::iter::repeat(None))),
                None => Either::Right(std::iter::repeat(None)),
            };
            let generic_span = generics.as_ref().map(|g| g.span_with_id()).unwrap_or_else(|| name.span.end_span());
            let generics = generics.as_mut().and_then(|g| g.generics.as_mut());
            let generics_iter = match generics {
                Some(generics) => Either::Left(generics.iter_mut().map(Some).chain(std::iter::repeat_with(|| None))),
                None => Either::Right(std::iter::repeat_with(|| None)),
            };
            let mut iter = expected_generic_iter.zip(generics_iter);
            loop {
                match iter.next() {
                    Some((Some(expected), Some(generic))) => {
                        if expected.def_ident.ident != generic.def_ident.ident {
                            parser.diagnostics.error(ErrorCode::MismatchedGeneric)
                                .with_error_label(generic.def_ident.diagnostics_span(), format!("expected generic name `{}`, found `{}`", expected.def_ident.ident, generic.def_ident.ident))
                                .with_note("generics of a type must have the same name in the type def and impl-block")
                                .emit();
                        }
                    },
                    Some((Some(expected), None)) => parser.diagnostics.error(ErrorCode::MissingGeneric)
                        .with_error_label(generic_span.diagnostics_span(), format!("missing generic `{}`", expected.def_ident.ident))
                        .with_info_label(expected.def_ident.diagnostics_span(), "defined here")
                        .emit(),
                    Some((None, Some(generic))) => parser.diagnostics.error(ErrorCode::TooManyGenerics)
                        .with_error_label(generic.diagnostics_span(), format!("too many generics, unknown generic `{}`", generic.def_ident.ident))
                        .with_info_label(expected_generic_span.diagnostics_span(), "expected generics defined here")
                        .emit(),
                    Some((None, None)) => break,
                    None => unreachable!(),
                }
            }
        })();

        let mut functions = Vec::new();
        let generic_list: Vec<_> = generics.iter().flat_map(|g| g.generics.iter().flat_map(|g| g.iter())).copied().collect();
        let close = loop {
            if let Ok(close) = parser.parse(depth.last()) {
                break close
            }
            let function = ExprFunctionDefinition::parse_with_generics(parser, depth.next(), &generic_list)?;
            functions.push(function);
        };

        // desugar:
        // * generics: generics defined on the impl-block-target are moved into the function
        // * self-args: insert self-args into args
        for fun in &mut functions {
            // generics
            if !generic_list.is_empty() {
                match &mut fun.sig.generics {
                    None => fun.sig.generics = Some(generics.clone().unwrap()),
                    Some(g) => {
                        let g = g.generics.get_or_insert_with(Separated::default);
                        for (&generic, delim) in generics.as_ref().unwrap().generics.as_ref().unwrap().iter_with_delimiters() {
                            let comma = delim.copied().unwrap_or(TokenComma {
                                span: generic.span_with_id().end_span(),
                            });
                            g.push_front(generic, Some(comma))
                        }
                    }
                }
            }

            // self-args
            if let Some(self_arg) = fun.sig.self_arg {
                let typed = ExprPatternTyped::new(
                    ExprPatternUntyped {
                        binding: self_arg,
                    },
                    TokenColon {
                        span: self_arg.ident.span.end_span(),
                    },
                    ExprType::UserType(ExprTypeUserType::new(
                        name,
                        match generics.clone() {
                            Some(ExprGenerics { open, generics: Some(generics), close, .. }) => {
                                Some((open, Box::new(Separated::from(generics)), close))
                            }
                            _ => None,
                        },
                    )),
                );
                fun.sig.args.push_front(typed, fun.sig.self_arg_comma);
            }
        }

        Ok(ExprImplBlock::new(impl_token, name, generics, open, functions, close))
    }
}
impl<'a, 'i> Spanned for ExprImplBlock<'a, 'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'a, 'i> Display for ExprImplBlock<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "impl {}", self.name.ident)?;
        if let Some(generics) = &self.generics {
            write!(f, "{}", generics)?;
        }
        writeln!(f, " {{")?;
        let mut padded = PadFmt::new(&mut *f);
        for function in &self.functions {
            writeln!(padded, "{}", function)?;
        }
        writeln!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct ExprInclude {
    pub include_token: TokenInclude,
    pub file: TokenDqString,
    pub span: SpanWithId,
}
impl<'a, 'i> Parse<'a, 'i> for ExprInclude {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let include_token: TokenInclude = parser.parse(depth.next())?;
        let file: TokenDqString = parser.parse(depth.next())?;
        let span = include_token.span | file.span;
        Ok(ExprInclude { include_token, file, span })
    }
}
impl Spanned for ExprInclude {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl Display for ExprInclude {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "include {}", self.file)
    }
}
