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
    fn parse_new(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Binding<'i>, InternalError> {
        trace!("{} Binding::parse_new        ({:?})", depth, parser.peek_token(0));
        Binding::parse_new_internal(parser, false, depth)
    }
    /// Return the newly created binding, allowing `self` as name
    fn parse_self(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Binding<'i>, InternalError> {
        trace!("{} Binding::parse_self        ({:?})", depth, parser.peek_token(0));
        Binding::parse_new_internal(parser, true, depth)
    }
    fn parse_new_internal(parser: &mut Parser<'i, '_>, require_self: bool, depth: Depth) -> Result<Binding<'i>, InternalError> {
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
    fn parse_existing(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<(Binding<'i>, SpanWithId), InternalError> {
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
        let mut binding = match parser.get_binding(ident.ident) {
            Some(binding) => binding,
            None if ident.ident.contains("::") => parser.add_rogue_binding(ident, Some(TokenMut { span: SpanWithId::from(ident.span.diagnostics_span()) })),
            None => parser.diagnostic_unknown_identifier(ident, |d| d.with_info_label(ident.span.diagnostics_span(), format!("use `let {} = ...` to create a new binding", ident))),
        };
        // hack to display the function call as `Target::function(...)` until we have proper path resolution
        // (the existing binding's ident only contains `function` as the target and function name
        // do not appear next to each other, e.g. `enum Foo { Foo(int) }` or `impl Foo { fn foo() {} }`)
        binding.ident.ident = ident.ident;
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
        write!(f, "{}/*{}*/", self.ident.ident, self.id)
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
impl<'i> Parse<'i> for NewBinding<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Parse<'i> for SelfBinding<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
    fn parse_new(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Generic<'i>, InternalError> {
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
impl<'i> Parse<'i> for NewGeneric<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Parse<'i> for ExprLiteral {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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

#[derive(Debug, Display, rebo_derive::Functions)]
#[function(fn span_with_id(&self) -> SpanWithId = expr => expr.span_with_id())]
#[allow(clippy::large_enum_variant)]
pub enum Expr<'i> {
    Literal(ExprLiteral),
    /// f"abc {expr} def"
    FormatString(ExprFormatString<'i>),
    /// let pattern = expr
    Bind(ExprBind<'i>),
    /// static pattern = expr
    Static(ExprStatic<'i>),
    /// ident = expr
    Assign(ExprAssign<'i>),
    // unops
    /// !expr
    BoolNot(ExprBoolNot<'i>),
    /// -expr
    Neg(ExprNeg<'i>),
    // binops
    /// expr + expr
    Add(ExprAdd<'i>),
    /// expr - expr
    Sub(ExprSub<'i>),
    /// expr * expr
    Mul(ExprMul<'i>),
    /// expr / expr
    Div(ExprDiv<'i>),
    /// expr % expr
    Mod(ExprMod<'i>),
    /// expr ^ expr
    Xor(ExprXor<'i>),
    /// expr && expr
    BoolAnd(ExprBoolAnd<'i>),
    /// expr || bar
    BoolOr(ExprBoolOr<'i>),
    // binop-assign
    /// lhs += expr
    AddAssign(ExprAddAssign<'i>),
    /// lhs -= expr
    SubAssign(ExprSubAssign<'i>),
    /// lhs *= expr
    MulAssign(ExprMulAssign<'i>),
    /// lhs /= expr
    DivAssign(ExprDivAssign<'i>),
    /// lhs %= expr
    ModAssign(ExprModAssign<'i>),
    /// lhs ^= expr
    XorAssign(ExprXorAssign<'i>),
    /// lhs &= expr
    BoolAndAssign(ExprBoolAndAssign<'i>),
    /// lhs |= bar
    BoolOrAssign(ExprBoolOrAssign<'i>),
    // comparison ops
    /// expr < expr
    LessThan(ExprLessThan<'i>),
    /// expr <= expr
    LessEquals(ExprLessEquals<'i>),
    /// expr == expr
    Equals(ExprEquals<'i>),
    /// expr != expr
    NotEquals(ExprNotEquals<'i>),
    /// expr >= expr
    GreaterEquals(ExprGreaterEquals<'i>),
    /// expr > expr
    GreaterThan(ExprGreaterThan<'i>),
    /// { expr;... }
    Block(ExprBlock<'i>),
    /// foo
    Variable(ExprVariable<'i>),
    /// foo.bar.baz().qux.quux().corge
    Access(ExprAccess<'i>),
    /// (expr)
    Parenthesized(ExprParenthesized<'i>),
    /// if expr {...} else if expr {...} else if expr {...} else {...}
    IfElse(ExprIfElse<'i>),
    /// match expr { pat => expr, pat => expr, ... }
    Match(ExprMatch<'i>),
    /// 'label: while expr {...}
    While(ExprWhile<'i>),
    /// 'label: for binding in expr {...}
    For(ExprFor<'i>),
    /// 'label: loop {...}
    Loop(ExprLoop<'i>),
    /// break 'label expr
    Break(ExprBreak<'i>),
    /// continue 'label
    Continue(ExprContinue<'i>),
    /// return expr
    Return(ExprReturn<'i>),
    /// yield expr
    Yield(ExprYield<'i>),
    /// (ident::)?ident(expr, expr, ...)
    FunctionCall(ExprFunctionCall<'i>),
    /// gen? fn ident(pat: typ, pat: typ, ...) -> typ { expr... }
    FunctionDefinition(ExprFunctionDefinition<'i>),
    /// struct ident { ident: typ, ident: typ, ... }
    StructDefinition(ExprStructDefinition<'i>),
    /// ident { ident: expr, ident: expr, ... }
    StructInitialization(ExprStructInitialization<'i>),
    /// enum ident { ident, ident(typ, typ, ...), ... }
    EnumDefinition(ExprEnumDefinition<'i>),
    // enum tuple-variant initialization is handled with an associated function
    /// C-Like enum variants: ident::ident
    EnumInitialization(ExprEnumInitialization<'i>),
    /// impl ident { fn foo(...) {...} fn bar(self, ...) {...} ... }
    ImplBlock(ExprImplBlock<'i>),
}
impl<'i> Spanned for Expr<'i> {
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
impl<'i> Expr<'i> {
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
    fn try_parse_until_including(parser: &mut Parser<'i, '_>, until: ParseUntil, depth: Depth) -> Result<&'i Expr<'i>, InternalError> {
        trace!("{} Expr::try_parse_until_including {:?}        ({:?})", depth, until, parser.peek_token(0));
        Expr::try_parse_until(parser, until, PartialOrd::ge, depth)
    }
    pub(in crate::parser) fn try_parse_until_excluding(parser: &mut Parser<'i, '_>, until: ParseUntil, depth: Depth) -> Result<&'i Expr<'i>, InternalError> {
        trace!("{} Expr::try_parse_until_excluding {:?}        ({:?})", depth, until, parser.peek_token(0));
        Expr::try_parse_until(parser, until, PartialOrd::gt, depth)
    }
    fn try_parse_until(parser: &mut Parser<'i, '_>, until: ParseUntil, cmp: fn(&ParseUntil, &ParseUntil) -> bool, depth: Depth) -> Result<&'i Expr<'i>, InternalError> {
        type ParseFn<'i, 'b> = fn(&mut Parser<'i, 'b>, Depth) -> Result<&'i Expr<'i>, InternalError>;
        let mut fns: Vec<ParseFn<'i, '_>> = Vec::new();
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
        let others: &[ParseFn<'i, '_>] = &[
            |parser: &mut Parser<'i, '_>, depth| {
                let expr = ExprFunctionDefinition::parse(parser, depth)?;
                let expr = &*parser.arena.alloc(Expr::FunctionDefinition(expr));
                let fun = match expr {
                    Expr::FunctionDefinition(fun) => fun,
                    _ => unreachable!("we just created you"),
                };
                parser.add_free_function_to_meta_info(fun);
                Ok(expr)
            },
            |parser: &mut Parser<'i, '_>, depth| {
                let expr = &*parser.arena.alloc(Expr::ImplBlock(ExprImplBlock::parse(parser, depth)?));
                let impl_block = match expr {
                    Expr::ImplBlock(impl_block) => impl_block,
                    _ => unreachable!("we just created you"),
                };
                parser.add_impl_block_functions_to_meta_info(impl_block);
                Ok(expr)
            },
            |parser: &mut Parser<'i, '_>, depth| {
                let static_expr = &*parser.arena.alloc(Expr::Static(ExprStatic::parse(parser, depth)?));
                let static_ = match static_expr {
                    Expr::Static(stati) => stati,
                    _ => unreachable!("we just created you"),
                };
                // add static to globals if it isn't there already (needed for included statics)
                parser.meta_info.add_static(parser.diagnostics, static_);
                Ok(static_expr)
            },
            |parser: &mut Parser<'i, '_>, depth| {
                let include = ExprInclude::parse(parser, depth)?;
                // We don't actually want to return an error, as that would mean that this function
                // is called several more times when other expressions try to parse expressions.
                // Instead, we return Unit to consume the tokens, as we did in fact parse the include correctly.
                let err = Ok(&*parser.arena.alloc(Expr::Literal(ExprLiteral::Unit(ExprUnit {
                    open: TokenOpenParen { span: include.include_token.span | include.file.span },
                    close: TokenCloseParen { span: include.file.span.end_span() },
                    span: include.include_token.span | include.file.span,
                }))));

                let file = match parser.meta_info.included_files.get(&include.diagnostics_span()) {
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
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::StructInitialization(ExprStructInitialization::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Break(ExprBreak::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Continue(ExprContinue::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Return(ExprReturn::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Yield(ExprYield::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::IfElse(ExprIfElse::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Match(ExprMatch::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::While(ExprWhile::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::For(ExprFor::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Loop(ExprLoop::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Parenthesized(ExprParenthesized::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Block(ExprBlock::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::BoolNot(ExprBoolNot::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Neg(ExprNeg::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Bind(ExprBind::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::EnumInitialization(ExprEnumInitialization::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::FunctionCall(ExprFunctionCall::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Literal(ExprLiteral::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::FormatString(ExprFormatString::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Assign(ExprAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::AddAssign(ExprAddAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::SubAssign(ExprSubAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::MulAssign(ExprMulAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::DivAssign(ExprDivAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::BoolAndAssign(ExprBoolAndAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::BoolOrAssign(ExprBoolOrAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Access(ExprAccess::parse(parser, depth)?))),
            |parser: &mut Parser<'i, '_>, depth| Ok(parser.arena.alloc(Expr::Variable(ExprVariable::parse(parser, depth)?))),
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

    fn try_parse_compare(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<&'i Expr<'i>, InternalError> {
        trace!("{} Expr::try_parse_compare        ({:?})", depth, parser.peek_token(0));
        let mark = parser.lexer.mark();
        let left = Expr::try_parse_until_excluding(parser, ParseUntil::Compare, depth.next())?;
        trace!("{}    left: {}", depth, left);
        let (constructor, token): (fn(&'i Expr<'i>, Token<'i>, &'i Expr<'i>) -> Expr<'i>, _) = match parser.next_token()? {
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
impl<'i> Parse<'i> for &'i Expr<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Expr::try_parse_until_including(parser, ParseUntil::All, depth.last())
    }
}

pub type TypeGenerics<'i> = (TokenLessThan, Box<Separated<'i, ExprType<'i>, TokenComma>>, TokenGreaterThan);
#[derive(Debug, Clone)]
pub enum ExprType<'i> {
    // can't be parsed, only generated for span-reasons -> ignore parens everywhere
    Parenthesized(ExprTypeParenthesized<'i>),
    String(TokenStringType),
    Int(TokenIntType),
    Float(TokenFloatType),
    Bool(TokenBoolType),
    Unit(ExprTypeUnit),
    // struct, enum, typedef, ...
    UserType(ExprTypeUserType<'i>),
    Generic(Generic<'i>),
    Function(Box<ExprFunctionType<'i>>),
    Never(TokenBang),
    // only used from rust in transformations (e.g. Generator-struct fields)
    /// def-span for the typechecker to convert it to a generic
    Any(SpanWithId),
}
#[derive(Debug, Clone)]
pub struct ExprTypeParenthesized<'i> {
    pub open: TokenOpenParen,
    pub typ: Box<ExprType<'i>>,
    pub close: TokenCloseParen,
    pub span: SpanWithId,
}
impl<'i> ExprTypeParenthesized<'i> {
    pub fn new(open: TokenOpenParen, typ: Box<ExprType<'i>>, close: TokenCloseParen) -> Self {
        ExprTypeParenthesized { open, typ, close, span: open.span | close.span }
    }
}
impl<'i> Spanned for ExprTypeParenthesized<'i> {
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
pub struct ExprTypeUserType<'i> {
    pub name: TokenIdent<'i>,
    pub generics: Option<TypeGenerics<'i>>,
    pub span: SpanWithId,
}
impl<'i> ExprTypeUserType<'i> {
    pub fn new(name: TokenIdent<'i>, generics: Option<TypeGenerics<'i>>) -> Self {
        let span = match &generics {
            Some((_open, _generics, close)) => name.span | close.span,
            None => name.span,
        };
        ExprTypeUserType { name, generics, span }
    }
}
impl<'i> Spanned for ExprTypeUserType<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> ExprType<'i> {
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
impl<'i> Parse<'i> for ExprType<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> From<Generic<'i>> for ExprType<'i> {
    fn from(g: Generic<'i>) -> Self {
        ExprType::Generic(g)
    }
}
impl<'i> Spanned for ExprType<'i> {
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
impl<'i> Display for ExprType<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::Parenthesized(ExprTypeParenthesized { typ, .. }) => write!(f, "{typ}"),
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
pub struct ExprGenerics<'i> {
    pub open: TokenLessThan,
    pub generics: Option<Separated<'i, Generic<'i>, TokenComma>>,
    pub close: TokenGreaterThan,
    pub span: SpanWithId,
}
impl<'i> ExprGenerics<'i> {
    pub fn new(open: TokenLessThan, generics: Option<Separated<'i, Generic<'i>, TokenComma>>, close: TokenGreaterThan) -> Self {
        ExprGenerics { open, generics, close, span: open.span | close.span }
    }
}
impl<'i> Parse<'i> for ExprGenerics<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprGenerics::new(
            parser.parse(depth.next())?,
            parser.parse::<Option<Separated<NewGeneric, _>>>(depth.next())?.map(Separated::from),
            parser.parse(depth.last())?,
        ))
    }
}
impl<'i> Display for ExprGenerics<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.generics.iter().join(", "))
    }
}
impl<'i> Spanned for ExprGenerics<'i> {
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
impl<'i> Parse<'i> for ExprLabel<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Parse<'i> for ExprLabelDef<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
#[display("()")]
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
impl<'i> Parse<'i> for ExprUnit {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Parse<'i> for ExprVariable<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
#[display("{}", int.value)]
pub struct ExprInteger {
    pub int: TokenInteger,
}
impl<'i> Parse<'i> for ExprInteger {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
#[display("{}", float.value)]
pub struct ExprFloat {
    pub float: TokenFloat,
}
impl<'i> Parse<'i> for ExprFloat {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
#[display("{}", b.value)]
pub struct ExprBool {
    pub b: TokenBool,
}
impl<'i> Parse<'i> for ExprBool {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
#[display("{}", string.string)]
pub struct ExprString {
    pub string: TokenDqString,
}
impl<'i> Parse<'i> for ExprString {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
pub enum ExprFormatStringPart<'i> {
    Str(&'i str),
    Escaped(&'i str),
    /// expr, format-string specifier
    FmtArg(&'i Expr<'i>, Option<(TokenColon, Specifier, SpanWithId)>),
}
#[derive(Debug, Clone)]
pub struct ExprFormatString<'i> {
    pub parts: Vec<ExprFormatStringPart<'i>>,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprFormatString<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprFormatString<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprFormatString<'i> {
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
pub struct ExprBind<'i> {
    pub let_token: TokenLet,
    pub pattern: ExprPattern<'i>,
    pub assign: TokenAssign,
    pub expr: &'i Expr<'i>,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprBind<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprBind<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprBind<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.pattern, self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprStaticSignature<'i> {
    pub static_token: TokenStatic,
    pub pattern: ExprPattern<'i>,
    pub assign: TokenAssign,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprStaticSignature<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        let static_token: TokenStatic = parser.parse(depth.next())?;
        let pattern = parser.parse(depth.next())?;
        let assign: TokenAssign = parser.parse(depth.next())?;
        let span = static_token.span | assign.span;
        Ok(ExprStaticSignature { static_token, pattern, assign, span })
    }
}
impl<'i> Spanned for ExprStaticSignature<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprStaticSignature<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "static {} =", self.pattern)
    }
}
#[derive(Debug, Clone)]
pub struct ExprStatic<'i> {
    pub sig: ExprStaticSignature<'i>,
    pub expr: &'i Expr<'i>,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprStatic<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        // add the static binding to the global scope instead of the inner scope
        let guard = parser.push_scope(ScopeType::Synthetic);
        let sig: ExprStaticSignature = parser.parse(depth.next())?;
        drop(guard);
        let expr: &Expr = parser.parse(depth.last())?;
        let span = sig.span | expr.span_with_id();
        Ok(ExprStatic { sig, expr, span })
    }
}
impl<'i> Spanned for ExprStatic<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprStatic<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.sig, self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprAssign<'i> {
    pub lhs: ExprAssignLhs<'i>,
    pub assign: TokenAssign,
    pub expr: &'i Expr<'i>,
    pub span: SpanWithId,
}
impl<'i> ExprAssign<'i> {
    pub fn new(lhs: ExprAssignLhs<'i>, assign: TokenAssign, expr: &'i Expr<'i>) -> Self {
        let span = lhs.span_with_id() | expr.span_with_id();
        ExprAssign { lhs, assign, expr, span }
    }
}
impl<'i> Parse<'i> for ExprAssign<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprAssign::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'i> Spanned for ExprAssign<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprAssign<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.lhs, self.expr)
    }
}
#[derive(Debug, Clone, Display)]
pub enum ExprAssignLhs<'i> {
    Variable(ExprVariable<'i>),
    FieldAccess(ExprFieldAccess<'i>),
}
impl<'i> Parse<'i> for ExprAssignLhs<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprAssignLhs<'i> {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            ExprAssignLhs::Variable(v) => v.span_with_id(),
            ExprAssignLhs::FieldAccess(fa) => fa.span_with_id(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExprFieldAccess<'i> {
    pub variable: ExprVariable<'i>,
    pub dot: TokenDot,
    pub fields: Separated<'i, TokenIdent<'i>, TokenDot>,
    pub span: SpanWithId,
}
impl<'i> ExprFieldAccess<'i> {
    pub fn new(variable: ExprVariable<'i>, dot: TokenDot, fields: Separated<'i, TokenIdent<'i>, TokenDot>) -> Self {
        assert!(!fields.is_empty());
        assert!(!fields.is_terminated());
        let span = variable.span | fields.diagnostics_span().unwrap();
        ExprFieldAccess { variable, dot, fields, span }
    }
}
impl<'i> Parse<'i> for ExprFieldAccess<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprFieldAccess<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprFieldAccess<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.variable, self.fields.iter().map(|f| f.ident).join("."))
    }
}


#[derive(Debug, Clone)]
pub struct ExprAccess<'i> {
    pub variable: ExprVariable<'i>,
    pub dot: TokenDot,
    pub accesses: Separated<'i, FieldOrMethod<'i>, TokenDot>,
    pub span: SpanWithId,
}
impl<'i> ExprAccess<'i> {
    pub fn new(variable: ExprVariable<'i>, dot: TokenDot, accesses: Separated<'i, FieldOrMethod<'i>, TokenDot>) -> Self {
        assert!(!accesses.is_empty());
        assert!(!accesses.is_terminated());
        let span = variable.span | accesses.diagnostics_span().unwrap();
        ExprAccess { variable, dot, accesses, span }
    }
}
impl<'i> Parse<'i> for ExprAccess<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprAccess<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprAccess<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.variable, self.accesses.iter().join("."))
    }
}


#[derive(Debug, Clone)]
pub enum FieldOrMethod<'i> {
    Field(TokenIdent<'i>),
    Method(ExprMethodCall<'i>),
}
impl<'i> From<TokenIdent<'i>> for FieldOrMethod<'i> {
    fn from(ident: TokenIdent<'i>) -> Self {
        FieldOrMethod::Field(ident)
    }
}
impl<'i> Parse<'i> for FieldOrMethod<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for FieldOrMethod<'i> {
    fn span_with_id(&self) -> SpanWithId {
        match self {
            FieldOrMethod::Method(function_call) => function_call.span_with_id(),
            FieldOrMethod::Field(field) => field.span_with_id(),
        }
    }
}
impl<'i> Display for FieldOrMethod<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FieldOrMethod::Field(field) => Display::fmt(field, f),
            FieldOrMethod::Method(function_call) => Display::fmt(function_call, f),
        }
    }
}


#[derive(Debug, Clone)]
pub struct ExprBoolNot<'i> {
    pub bang: TokenBang,
    pub expr: &'i Expr<'i>,
    pub span: SpanWithId,
}
impl<'i> ExprBoolNot<'i> {
    pub fn new(bang: TokenBang, expr: &'i Expr<'i>) -> Self {
        ExprBoolNot { bang, expr, span: bang.span | expr.span_with_id() }
    }
}
impl<'i> Parse<'i> for ExprBoolNot<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprBoolNot::new(
            parser.parse(depth.next())?,
            Expr::try_parse_until_excluding(parser, ParseUntil::BooleanExpr, depth.last())?,
        ))
    }
}
impl<'i> Spanned for ExprBoolNot<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprBoolNot<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "!{}", self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprNeg<'i> {
    pub minus: TokenMinus,
    pub expr: &'i Expr<'i>,
    pub span: SpanWithId,
}
impl<'i> ExprNeg<'i> {
    pub fn new(minus: TokenMinus, expr: &'i Expr<'i>) -> Self {
        ExprNeg { minus, expr, span: minus.span | expr.span_with_id() }
    }
}
impl<'i> Parse<'i> for ExprNeg<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprNeg::new(
            parser.parse(depth.next())?,
            Expr::try_parse_until_excluding(parser, ParseUntil::Math, depth.last())?,
        ))
    }
}
impl<'i> Spanned for ExprNeg<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprNeg<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "-{}", self.expr)
    }
}

macro_rules! binop {
    ($($exprname:ident, $name:ident, $token:ident, $tokenvariant:ident $(, $assign_exprname:ident, $assign_token:ident)?;)+) => {
        $(
            #[derive(Debug, Clone)]
            pub struct $exprname<'i> {
                pub a: &'i Expr<'i>,
                pub op: $token,
                pub b: &'i Expr<'i>,
                pub span: SpanWithId,
            }
            impl<'i> $exprname<'i> {
                pub fn new(a: &'i Expr<'i>, op: $token, b: &'i Expr<'i>) -> $exprname<'i> {
                    Self { a, op, b, span: a.span_with_id() | b.span_with_id() }
                }
                pub(in crate::parser) fn new_as_expr(a: &'i Expr<'i>, op: Token<'i>, b: &'i Expr<'i>) -> Expr<'i> {
                    let op = match op {
                        Token::$tokenvariant(token @ $token { .. }) => token,
                        _ => unreachable!(),
                    };
                    Expr::$name(Self::new(a, op, b))
                }
            }
            impl<'i> Parse<'i> for $exprname<'i> {
                fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
                    Ok($exprname::new(
                        parser.parse(depth.next())?,
                        parser.parse(depth.next())?,
                        parser.parse(depth.next())?,
                    ))
                }
            }
            impl<'i> Spanned for $exprname<'i> {
                fn span_with_id(&self) -> SpanWithId {
                    self.span
                }
            }
            impl<'i> Display for $exprname<'i> {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    write!(f, "{} {}{}", self.a, self.op, self.b)
                }
            }
            $(
                #[derive(Debug, Clone)]
                pub struct $assign_exprname<'i> {
                    pub lhs: ExprAssignLhs<'i>,
                    pub op: $assign_token,
                    pub assign: TokenAssign,
                    pub expr: &'i Expr<'i>,
                    pub span: SpanWithId,
                }
                impl<'i> Parse<'i> for $assign_exprname<'i> {
                    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
                        let lhs: ExprAssignLhs = parser.parse(depth.next())?;
                        let op = parser.parse(depth.next())?;
                        let assign = parser.parse(depth.next())?;
                        let expr: &Expr = parser.parse(depth.next())?;
                        let span = lhs.span_with_id() | expr.span_with_id();
                        Ok($assign_exprname { lhs, op, assign, expr, span })
                    }
                }
                impl<'i> Spanned for $assign_exprname<'i> {
                    fn span_with_id(&self) -> SpanWithId {
                        self.span
                    }
                }
                impl<'i> Display for $assign_exprname<'i> {
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
pub struct ExprBlock<'i> {
    pub open: TokenOpenCurly,
    pub body: BlockBody<'i>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'i> ExprBlock<'i> {
    pub fn new(open: TokenOpenCurly, body: BlockBody<'i>, close: TokenCloseCurly) -> Self {
        ExprBlock { open, body, close, span: open.span | close.span }
    }
}
impl<'i> Parse<'i> for ExprBlock<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprBlock<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprBlock<'i> {
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
pub struct BlockBody<'i> {
    pub exprs: Vec<&'i Expr<'i>>,
    pub terminated_with_semicolon: bool,
}
impl<'i> Parse<'i> for BlockBody<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
pub struct ExprParenthesized<'i> {
    pub open: TokenOpenParen,
    pub expr: &'i Expr<'i>,
    pub close: TokenCloseParen,
    pub span: SpanWithId,
}
impl<'i> ExprParenthesized<'i> {
    pub fn new(open: TokenOpenParen, expr: &'i Expr<'i>, close: TokenCloseParen) -> Self {
        ExprParenthesized { open, expr, close, span: open.span | close.span }
    }
}
impl<'i> Parse<'i> for ExprParenthesized<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprParenthesized<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprParenthesized<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprIfElse<'i> {
    pub if_token: TokenIf,
    pub condition: &'i Expr<'i>,
    pub then: ExprBlock<'i>,
    pub else_ifs: Vec<(TokenElse, TokenIf, &'i Expr<'i>, ExprBlock<'i>)>,
    pub els: Option<(TokenElse, ExprBlock<'i>)>,
    pub span: SpanWithId,
}
impl<'i> ExprIfElse<'i> {
    pub fn iter_branches(&self) -> impl Iterator<Item = (Option<&'i Expr<'i>>, &ExprBlock<'i>)> {
        ::std::iter::once((Some(self.condition), &self.then))
            .chain(self.else_ifs.iter().map(|(_, _, cond, block)| (Some(*cond), block)))
            .chain(self.els.iter().map(|(_, block)| (None, block)))
    }
}
impl<'i> Parse<'i> for ExprIfElse<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprIfElse<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprIfElse<'i> {
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
pub struct ExprMatch<'i> {
    pub match_token: TokenMatch,
    pub expr: &'i Expr<'i>,
    pub open: TokenOpenCurly,
    pub arms: Vec<(ExprMatchPattern<'i>, TokenFatArrow, &'i Expr<'i>)>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'i> ExprMatch<'i> {
    pub fn new(match_token: TokenMatch, expr: &'i Expr<'i>, open: TokenOpenCurly, arms: Vec<(ExprMatchPattern<'i>, TokenFatArrow, &'i Expr<'i>)>, close: TokenCloseCurly) -> Self {
        ExprMatch { match_token, expr, open, arms, close, span: match_token.span | close.span }
    }
}
impl<'i> Parse<'i> for ExprMatch<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprMatch::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse::<Separated<'i, Scoped<(ExprMatchPattern<'i>, TokenFatArrow, &'i Expr<'i>)>, TokenComma>>(depth.next())?
                .into_iter().map(|Scoped((pattern, arrow, expr))| (pattern, arrow, expr)).collect(),
            parser.parse(depth.last())?,
        ))
    }
}
impl<'i> Spanned for ExprMatch<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprMatch<'i> {
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
pub struct ExprWhile<'i> {
    pub label: Option<ExprLabelDef<'i>>,
    pub while_token: TokenWhile,
    pub condition: &'i Expr<'i>,
    pub block: ExprBlock<'i>,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprWhile<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        let label: Option<ExprLabelDef<'i>> = parser.parse(depth.next())?;
        let while_token: TokenWhile = parser.parse(depth.next())?;
        let condition = Expr::try_parse_until_including(parser, ParseUntil::All, depth.next())?;
        let _guard = parser.push_scope(ScopeType::While(label.clone()));
        let block: ExprBlock = parser.parse(depth.next())?;
        let span = label.as_ref().map(Spanned::span_with_id) | while_token.span | block.span;
        Ok(ExprWhile { label, while_token, condition, block, span })
    }
}
impl<'i> Spanned for ExprWhile<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprWhile<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "{} ", label)?;
        }
        write!(f, "while {} {}", self.condition, self.block)
    }
}

#[derive(Debug, Clone)]
pub struct ExprFor<'i> {
    pub label: Option<ExprLabelDef<'i>>,
    pub for_token: TokenFor,
    pub binding: Binding<'i>,
    pub in_token: TokenIn,
    pub expr: &'i Expr<'i>,
    pub block: ExprBlock<'i>,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprFor<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprFor<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprFor<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "{} ", label)?;
        }
        write!(f, "for {} in {} {}", self.binding, self.expr, self.block)
    }
}

#[derive(Debug, Clone)]
pub struct ExprLoop<'i> {
    pub label: Option<ExprLabelDef<'i>>,
    pub loop_token: TokenLoop,
    pub block: ExprBlock<'i>,
    pub span: SpanWithId,
}
impl<'i> ExprLoop<'i> {
    pub fn new(label: Option<ExprLabelDef<'i>>, loop_token: TokenLoop, block: ExprBlock<'i>) -> Self {
        let span = label.as_ref().map(Spanned::span_with_id) | loop_token.span | block.span;
        ExprLoop { label, loop_token, block, span }
    }
}
impl<'i> Parse<'i> for ExprLoop<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        let label: Option<ExprLabelDef<'i>> = parser.parse(depth.next())?;
        let loop_token = parser.parse(depth.next())?;
        let _guard = parser.push_scope(ScopeType::Loop(label.clone()));
        let block = parser.parse(depth.next())?;
        Ok(ExprLoop::new(label, loop_token, block))
    }
}
impl<'i> Spanned for ExprLoop<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprLoop<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "{} ", label)?;
        }
        write!(f, "loop {}", self.block)
    }
}

#[derive(Debug, Clone)]
pub struct ExprBreak<'i> {
    pub break_token: TokenBreak,
    pub label: Option<ExprLabel<'i>>,
    pub expr: Option<&'i Expr<'i>>,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprBreak<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        let break_token: TokenBreak = parser.parse(depth.next())?;
        let label: Option<ExprLabel> = parser.parse(depth.next())?;
        let expr: Option<&Expr> = parser.parse(depth.last())?;
        let span = break_token.span | label.as_ref().map(Spanned::span_with_id) | expr.map(Spanned::span_with_id);
        Ok(ExprBreak { break_token, label, expr, span })
    }
}
impl<'i> Spanned for ExprBreak<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprBreak<'i> {
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
impl<'i> Parse<'i> for ExprContinue<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
pub struct ExprReturn<'i> {
    pub return_token: TokenReturn,
    pub expr: Option<&'i Expr<'i>>,
    pub span: SpanWithId,
}
impl<'i> ExprReturn<'i> {
    pub fn new(return_token: TokenReturn, expr: Option<&'i Expr<'i>>) -> Self {
        ExprReturn { return_token, expr, span: return_token.span | expr.map(Spanned::span_with_id) }
    }
}
impl<'i> Parse<'i> for ExprReturn<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprReturn::new(
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'i> Spanned for ExprReturn<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprReturn<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "return")?;
        if let Some(expr) = &self.expr {
            write!(f, " {}", expr)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprYield<'i> {
    pub yield_token: TokenYield,
    pub expr: Option<&'i Expr<'i>>,
    pub span: SpanWithId,
}
impl<'i> ExprYield<'i> {
    pub fn new(yield_token: TokenYield, expr: Option<&'i Expr<'i>>) -> Self {
        ExprYield { yield_token, expr, span: yield_token.span | expr.map(|e| e.span_with_id()) }
    }
}
impl<'i> Parse<'i> for ExprYield<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprYield::new(
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'i> Spanned for ExprYield<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprYield<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "yield")?;
        if let Some(expr) = &self.expr {
            write!(f, " {}", expr)?;
        }
        Ok(())
    }
}

pub type ExprFunctionCall<'i> = ExprFunctionOrMethodCall<'i, ExprVariable<'i>>;
pub type ExprMethodCall<'i> = ExprFunctionOrMethodCall<'i, TokenIdent<'i>>;
#[derive(Debug, Clone)]
pub struct ExprFunctionOrMethodCall<'i, T> {
    pub name: T,
    pub open: TokenOpenParen,
    pub args: Separated<'i, &'i Expr<'i>, TokenComma>,
    pub close: TokenCloseParen,
    pub span: SpanWithId,
}
impl<'i, T: Spanned> ExprFunctionOrMethodCall<'i, T> {
    pub fn new(name: T, open: TokenOpenParen, args: Separated<'i, &'i Expr<'i>, TokenComma>, close: TokenCloseParen) -> Self {
        let span = name.span_with_id() | close.span;
        ExprFunctionOrMethodCall { name, open, args, close, span }
    }
}
impl<'i, T: Parse<'i> + Spanned> Parse<'i> for ExprFunctionOrMethodCall<'i, T> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprFunctionOrMethodCall::new(
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.next())?,
            parser.parse(depth.last())?,
        ))
    }
}
impl<'i, T> Spanned for ExprFunctionOrMethodCall<'i, T> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i, T: Display> Display for ExprFunctionOrMethodCall<'i, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.args)
    }
}

#[derive(Debug, Clone)]
pub struct ExprFunctionType<'i> {
    pub fn_token: TokenFn,
    pub generics: Option<ExprGenerics<'i>>,
    pub open: TokenOpenParen,
    pub args: Separated<'i, ExprType<'i>, TokenComma>,
    pub close: TokenCloseParen,
    pub ret_type: Option<(TokenArrow, ExprType<'i>)>,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprFunctionType<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprFunctionType<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprFunctionType<'i> {
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
pub struct ExprFunctionSignature<'i> {
    pub gen_token: Option<TokenGen>,
    pub fn_token: TokenFn,
    pub name: Option<TokenIdent<'i>>,
    pub generics: Option<ExprGenerics<'i>>,
    pub open: TokenOpenParen,
    pub self_arg: Option<Binding<'i>>,
    pub self_arg_comma: Option<TokenComma>,
    pub args: Separated<'i, ExprPatternTyped<'i>, TokenComma>,
    pub varargs: Option<(Option<ExprType<'i>>, TokenDotDotDot)>,
    pub close: TokenCloseParen,
    pub ret_type: Option<(TokenArrow, ExprType<'i>)>,
    pub span: SpanWithId,
}
impl<'i> ExprFunctionSignature<'i> {
    pub fn new(gen_token: Option<TokenGen>, fn_token: TokenFn, name: Option<TokenIdent<'i>>, generics: Option<ExprGenerics<'i>>, open: TokenOpenParen, self_arg: Option<Binding<'i>>, self_arg_comma: Option<TokenComma>, args: Separated<'i, ExprPatternTyped<'i>, TokenComma>, varargs: Option<(Option<ExprType<'i>>, TokenDotDotDot)>, close: TokenCloseParen, ret_type: Option<(TokenArrow, ExprType<'i>)>) -> Self {
        let span = gen_token.as_ref().map(Spanned::span_with_id) | fn_token.span | close.span | ret_type.as_ref().map(|(_, typ)| typ.span_with_id());
        ExprFunctionSignature { gen_token, fn_token, name, generics, open, self_arg, self_arg_comma, args, varargs, close, ret_type, span }
    }
}
impl<'i> Parse<'i> for ExprFunctionSignature<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        let gen_token = parser.parse(depth.next())?;
        let fn_token = parser.parse(depth.next())?;
        let name = parser.parse(depth.next())?;
        let generics = parser.parse(depth.next())?;
        let open = parser.parse(depth.next())?;
        type SelfWithArgs<'i> = Option<(SelfBinding<'i>, Option<(TokenComma, Separated<'i, ExprPatternTyped<'i>, TokenComma>)>)>;
        let self_with_args: SelfWithArgs<'i> = parser.parse(depth.next())?;
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
impl<'i> Spanned for ExprFunctionSignature<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprFunctionSignature<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn")?;
        if let Some(name) = self.name {
            write!(f, " {}", name.ident)?;
        }
        if let Some(generics) = &self.generics {
            write!(f, "{}", generics)?;
        }
        // if there is a self-arg, it's an ExprPatternTyped; we need to remove the type
        let self_arg = self.self_arg.as_ref().map(|&binding| ExprPatternUntyped { binding }.to_string());
        let args = self.args.iter().skip(self_arg.is_some() as usize).map(ToString::to_string);
        write!(f, "({})", self_arg.into_iter().chain(args).join(", "))?;
        if let Some((_arrow, ret_type)) = &self.ret_type {
            write!(f, " -> {}", ret_type)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprFunctionDefinition<'i> {
    pub sig: ExprFunctionSignature<'i>,
    pub captures: IndexSet<Binding<'i>>,
    pub body: ExprBlock<'i>,
    pub span: SpanWithId,
}
impl<'i> ExprFunctionDefinition<'i> {
    /// WARNING: Doesn't handle generator transformation
    pub fn new(sig: ExprFunctionSignature<'i>, captures: IndexSet<Binding<'i>>, body: ExprBlock<'i>) -> Self {
        let span = sig.span | body.span;
        ExprFunctionDefinition { sig, captures, body, span }
    }
    pub fn arg_diagnostics_span(&self) -> Span {
        Span::new(self.sig.open.file_id(), self.sig.open.start(), self.sig.close.end())
    }
    fn parse_with_generics(parser: &mut Parser<'i, '_>, depth: Depth, generics: &[Generic<'i>]) -> Result<Self, InternalError> {
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
    fn parse_internal(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
            Some(gen_token) => generator_transform::transform_generator(parser, gen_token, fun),
            None => fun
        })
    }
}
impl<'i> Parse<'i> for ExprFunctionDefinition<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Self::parse_with_generics(parser, depth, &[])
    }
}
impl<'i> Spanned for ExprFunctionDefinition<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprFunctionDefinition<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.sig, self.body)
    }
}

pub type ExprStructDefFields<'i> = Separated<'i, (TokenIdent<'i>, TokenColon, ExprType<'i>), TokenComma>;
#[derive(Debug, Clone)]
pub struct ExprStructDefinition<'i> {
    pub struct_token: TokenStruct,
    pub name: TokenIdent<'i>,
    pub generics: Option<ExprGenerics<'i>>,
    pub open: TokenOpenCurly,
    pub fields: ExprStructDefFields<'i>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'i> ExprStructDefinition<'i> {
    pub fn new(struct_token: TokenStruct, name: TokenIdent<'i>, generics: Option<ExprGenerics<'i>>, open: TokenOpenCurly, fields: ExprStructDefFields<'i>, close: TokenCloseCurly) -> Self {
        ExprStructDefinition { struct_token, name, generics, open, fields, close, span: struct_token.span | close.span }
    }
}
impl<'i> Parse<'i> for ExprStructDefinition<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprStructDefinition<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprStructDefinition<'i> {
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

pub type ExprStructInitFields<'i> = Separated<'i, (TokenIdent<'i>, TokenColon, &'i Expr<'i>), TokenComma>;
#[derive(Debug, Clone)]
pub struct ExprStructInitialization<'i> {
    pub name: TokenIdent<'i>,
    pub open: TokenOpenCurly,
    pub fields: ExprStructInitFields<'i>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'i> ExprStructInitialization<'i> {
    pub fn new(name: TokenIdent<'i>, open: TokenOpenCurly, fields: ExprStructInitFields<'i>, close: TokenCloseCurly) -> Self {
        ExprStructInitialization { name, open, fields, close, span: name.span | close.span }
    }
}
impl<'i> Parse<'i> for ExprStructInitialization<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprStructInitialization<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprStructInitialization<'i> {
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
pub struct ExprEnumDefinition<'i> {
    pub enum_token: TokenEnum,
    pub name: TokenIdent<'i>,
    pub generics: Option<ExprGenerics<'i>>,
    pub open: TokenOpenCurly,
    pub variants: Separated<'i, ExprEnumVariant<'i>, TokenComma>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'i> Parse<'i> for ExprEnumDefinition<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprEnumDefinition<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprEnumDefinition<'i> {
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
pub struct ExprEnumVariant<'i> {
    pub name: TokenIdent<'i>,
    pub fields: Option<(TokenOpenParen, Separated<'i, ExprType<'i>, TokenComma>, TokenCloseParen)>,
}
impl<'i> Parse<'i> for ExprEnumVariant<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprEnumVariant {
            name: parser.parse(depth.next())?,
            fields: parser.parse(depth.last())?,
        })
    }
}
impl<'i> Display for ExprEnumVariant<'i> {
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
impl<'i> Parse<'i> for ExprEnumInitialization<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
pub struct ExprImplBlock<'i> {
    pub impl_token: TokenImpl,
    pub name: TokenIdent<'i>,
    pub generics: Option<ExprGenerics<'i>>,
    pub open: TokenOpenCurly,
    pub functions: Vec<ExprFunctionDefinition<'i>>,
    pub close: TokenCloseCurly,
    pub span: SpanWithId,
}
impl<'i> ExprImplBlock<'i> {
    pub fn new(impl_token: TokenImpl, name: TokenIdent<'i>, generics: Option<ExprGenerics<'i>>, open: TokenOpenCurly, functions: Vec<ExprFunctionDefinition<'i>>, close: TokenCloseCurly) -> Self {
        ExprImplBlock { impl_token, name, generics, open, functions, close, span: impl_token.span | close.span }
    }
}
impl<'i> Parse<'i> for ExprImplBlock<'i> {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
impl<'i> Spanned for ExprImplBlock<'i> {
    fn span_with_id(&self) -> SpanWithId {
        self.span
    }
}
impl<'i> Display for ExprImplBlock<'i> {
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
impl<'i> Parse<'i> for ExprInclude {
    fn parse_marked(parser: &mut Parser<'i, '_>, depth: Depth) -> Result<Self, InternalError> {
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
