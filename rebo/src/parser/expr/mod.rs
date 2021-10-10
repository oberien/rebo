mod pattern;
mod helper;
pub use pattern::{ExprPattern, ExprPatternTyped, ExprPatternUntyped, ExprMatchPattern, ExprMatchPatternVariant};

use std::fmt::{self, Write, Display, Formatter, Debug};
use derive_more::Display;
use crate::parser::scope::BindingId;
use crate::util::PadFmt;
use crate::lexer::{TokenOpenParen, TokenCloseParen, TokenIdent, TokenInteger, TokenFloat, TokenBool, TokenDqString, TokenType, TokenStringType, TokenIntType, TokenFloatType, TokenBoolType, Token, TokenLet, TokenColon, TokenMut, TokenAssign, TokenOpenCurly, TokenCloseCurly, TokenComma, TokenArrow, TokenFn, TokenBang, TokenPlus, TokenMinus, TokenStar, TokenSlash, TokenDoubleAmp, TokenDoublePipe, TokenLessThan, TokenLessEquals, TokenEquals, TokenNotEquals, TokenGreaterEquals, TokenGreaterThan, TokenStruct, TokenDot, TokenIf, TokenElse, TokenWhile, TokenFormatString, TokenFormatStringPart, Lexer, TokenMatch, TokenFatArrow, TokenEnum, TokenDoubleColon, TokenImpl};
use crate::parser::{Parse, InternalError, Parser, Expected};
use crate::error_codes::ErrorCode;
use std::borrow::Cow;
use crate::parser::parse::{Separated, Spanned, Scoped};
use crate::parser::precedence::{BooleanExpr, Math};
use diagnostic::Span;
use itertools::Itertools;
use crate::common::Depth;
use indexmap::set::IndexSet;

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
                .with_error_label(ident.span, "using `self` as variable is not allowed here")
                .with_note("note: `self` is only allowed as first parameter of methods")
                .emit();
        }
        if require_self && ident.ident != "self" {
            return Err(InternalError::Backtrack(ident.span, Cow::Borrowed(&[Expected::Token(TokenType::Ident)])));
        }
        mark.apply();
        let binding = parser.add_binding(ident, mut_token);
        trace!("{} got binding {}", depth, binding);
        Ok(binding)
    }
    /// Return the existing binding and the usage-span
    fn parse_existing<'a>(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<(Binding<'i>, Span), InternalError> {
        trace!("{} Binding::parse_existing        ({:?})", depth, parser.peek_token(0));
        let ident: TokenIdent = parser.parse(depth.last())?;
        let binding = match parser.get_binding(ident.ident) {
            Some(binding) => binding,
            None => parser.diagnostic_unknown_identifier(ident, |d| d.with_info_label(ident.span, format!("use `let {} = ...` to create a new binding", ident))),
        };
        trace!("{} got binding {}", depth, binding);
        Ok((binding, ident.span))
    }
}
impl<'i> Spanned for Binding<'i> {
    fn span(&self) -> Span {
        let start = self.mutable.map(|m| m.span).unwrap_or(self.ident.span);
        Span::new(start.file, start.start, self.ident.span.end)
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
    fn span(&self) -> Span {
        self.b.span()
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
    fn span(&self) -> Span {
        self.b.span()
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
    fn span(&self) -> Span {
        self.ident.span()
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
    fn span(&self) -> Span {
        self.g.span()
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
    fn span(&self) -> Span {
        match self {
            ExprLiteral::Unit(e) => e.span(),
            ExprLiteral::Integer(e) => e.span(),
            ExprLiteral::Float(e) => e.span(),
            ExprLiteral::Bool(e) => e.span(),
            ExprLiteral::String(e) => e.span(),
        }
    }
}

#[derive(Debug, Display, rebo_derive::Functions)]
#[function(fn span(&self) -> Span = expr => expr.span())]
pub enum Expr<'a, 'i> {
    Literal(ExprLiteral),
    /// f"abc {expr} def"
    FormatString(ExprFormatString<'a, 'i>),
    /// let ident = expr
    Bind(ExprBind<'a, 'i>),
    /// ident = expr
    Assign(ExprAssign<'a, 'i>),
    // unops
    /// !expr
    BoolNot(ExprBoolNot<'a, 'i>),
    // binops
    /// expr + expr
    Add(ExprAdd<'a, 'i>),
    /// expr - expr
    Sub(ExprSub<'a, 'i>),
    /// expr * expr
    Mul(ExprMul<'a, 'i>),
    /// expr / expr
    Div(ExprDiv<'a, 'i>),
    /// expr && expr
    BoolAnd(ExprBoolAnd<'a, 'i>),
    /// expr || bar
    BoolOr(ExprBoolOr<'a, 'i>),
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
    /// foo.bar.baz
    FieldAccess(ExprFieldAccess<'a, 'i>),
    /// foo.bar.baz(args...)
    MethodCall(ExprMethodCall<'a, 'i>),
    /// (expr)
    Parenthesized(ExprParenthesized<'a, 'i>),
    /// if expr {...} else if {...} else if {...} else {...}
    IfElse(ExprIfElse<'a, 'i>),
    /// match expr { pat => expr, pat => expr, ... }
    Match(ExprMatch<'a, 'i>),
    /// while expr {...}
    While(ExprWhile<'a, 'i>),
    /// (ident::)*ident(expr, expr, ...)
    FunctionCall(ExprFunctionCall<'a, 'i>),
    /// fn ident(ident: typ, ident: typ, ...) -> typ { expr... }
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
    /// impl name { fn foo(...) {...} fn bar(self, ...) {...} }
    ImplBlock(ExprImplBlock<'a, 'i>),
}
impl<'a, 'i> Spanned for Expr<'a, 'i> {
    fn span(&self) -> Span {
        Expr::span(self)
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
                // functions must be parsed in their own scope
                let old_scopes = std::mem::take(&mut parser.scopes);
                let scope_guard = parser.push_scope();
                let result = ExprFunctionDefinition::parse(parser, depth);
                drop(scope_guard);
                parser.scopes = old_scopes;
                let expr = result?;

                if let Some(self_arg) = &expr.self_arg {
                    parser.diagnostics.error(ErrorCode::SelfBinding)
                        .with_error_label(self_arg.span(), "self-argument not allowed here")
                        .with_info_label(self_arg.span(), "self-argument is only allowed in methods")
                        .emit();
                }

                let fun = &*parser.arena.alloc(Expr::FunctionDefinition(expr));
                match fun {
                    Expr::FunctionDefinition(fun) => {
                        parser.meta_info.add_function(parser.diagnostics, Cow::Borrowed(fun.name.ident), fun);
                    },
                    _ => unreachable!("we just created you"),
                }
                Ok(fun)
            },
            |parser: &mut Parser<'a, '_, 'i>, depth| {
                let struct_def = &*parser.arena.alloc(Expr::StructDefinition(ExprStructDefinition::parse(parser, depth)?));
                match struct_def {
                    Expr::StructDefinition(struct_def) => {
                        parser.meta_info.add_struct(parser.diagnostics, struct_def);
                    },
                    _ => unreachable!("we just created you"),
                }
                Ok(struct_def)
            },
            |parser: &mut Parser<'a, '_, 'i>, depth| {
                let enum_def = &*parser.arena.alloc(Expr::EnumDefinition(ExprEnumDefinition::parse(parser, depth)?));
                match enum_def {
                    Expr::EnumDefinition(enum_def) => {
                        parser.meta_info.add_enum(parser.diagnostics, enum_def);
                        for variant in enum_def.variants.iter() {
                            if variant.fields.is_some() {
                                let enum_name = enum_def.name.ident.to_string();
                                let variant_name = variant.name.ident.to_string();
                                parser.meta_info.add_enum_initializer_function(parser.diagnostics, enum_name, variant_name);
                            }
                        }
                    },
                    _ => unreachable!("we just created you"),
                }
                Ok(enum_def)
            },
            |parser: &mut Parser<'a, '_, 'i>, depth| {
                let impl_block = &*parser.arena.alloc(Expr::ImplBlock(ExprImplBlock::parse(parser, depth)?));
                match impl_block {
                    Expr::ImplBlock(impl_block) => {
                        for fun in &impl_block.functions {
                            let path = format!("{}::{}", impl_block.name.ident, fun.name.ident);
                            parser.meta_info.add_function(parser.diagnostics, Cow::Owned(path), fun);
                        }
                    }
                    _ => unreachable!("we just created you"),
                }
                Ok(impl_block)
            },
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::StructInitialization(ExprStructInitialization::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::IfElse(ExprIfElse::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Match(ExprMatch::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::While(ExprWhile::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Parenthesized(ExprParenthesized::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Block(ExprBlock::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::BoolNot(ExprBoolNot::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Bind(ExprBind::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::FunctionCall(ExprFunctionCall::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::EnumInitialization(ExprEnumInitialization::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Literal(ExprLiteral::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::FormatString(ExprFormatString::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Assign(ExprAssign::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::MethodCall(ExprMethodCall::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::FieldAccess(ExprFieldAccess::parse(parser, depth)?))),
            |parser: &mut Parser<'a, '_, 'i>, depth| Ok(parser.arena.alloc(Expr::Variable(ExprVariable::parse(parser, depth)?))),
        ];
        fns.extend(others);

        let mut expected = Vec::new();
        for (i, f) in fns.iter().enumerate() {
            // check if we have memoization already
            let token = parser.peek_token(0)?;
            let memoized = if let Some(expr) = parser.pre_parsed.remove(&(token.span().file, token.span().start)) {
                Some(expr)
            } else if let Some(&(expr, memuntil)) = parser.memoization.get(&(token.span().file, token.span().start)) {
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
                    while parser.peek_token(0).unwrap().span().start < expr.span().end {
                        drop(parser.next_token());
                    }
                    trace!("{}    reusing {:?}: {}", depth, expr.span(), expr);
                    return Ok(expr);
                }
            }

            let mark = parser.lexer.mark();
            let next_depth = if i == fns.len() - 1 { depth.last() } else { depth.next() };
            match f(parser, next_depth) {
                Ok(expr) => {
                    trace!("{}    memoize {:?}: {} ({})", depth, expr.span(), expr, parser.memoization.iter().map(|(k, (v, _))| format!("{:?}: {}", k, v)).join("; "));
                    parser.memoization.entry((expr.span().file, expr.span().start))
                        .and_modify(|mem| if until >= mem.1 {
                            *mem = (expr, until);
                        }).or_insert((expr, until));
                    mark.apply();
                    return Ok(expr)
                },
                Err(InternalError::Backtrack(span, expect)) => expected.push((span, expect)),
                e @ Err(InternalError::Error(_)) => return e,
            }
        }
        let max_span = expected.iter().map(|(span, _)| span).max().copied().unwrap();
        let expected: IndexSet<_> = expected.into_iter()
            .filter(|(span, _)| *span == max_span)
            .flat_map(|(_, expected)| expected.into_owned())
            .collect();
        let mut expected: Vec<_> = expected.into_iter().collect();
        expected.sort();
        Err(InternalError::Backtrack(max_span, Cow::Owned(expected)))
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
            token => return Err(InternalError::Backtrack(token.span(), Cow::Borrowed(Expected::COMPARE_OP))),
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

#[derive(Debug, Clone)]
pub enum ExprType<'a, 'i> {
    String(TokenStringType),
    Int(TokenIntType),
    Float(TokenFloatType),
    Bool(TokenBoolType),
    Unit(TokenOpenParen, TokenCloseParen),
    // struct, enum, typedef, ...
    UserType(TokenIdent<'i>, Option<(TokenLessThan, Box<Separated<'a, 'i, ExprType<'a, 'i>, TokenComma>>, TokenGreaterThan)>),
    Generic(Generic<'i>),
}
impl<'a, 'i> Parse<'a, 'i> for ExprType<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let res = match parser.peek_token(0)? {
            Token::StringType(t) => ExprType::String(t),
            Token::IntType(t) => ExprType::Int(t),
            Token::FloatType(t) => ExprType::Float(t),
            Token::BoolType(t) => ExprType::Bool(t),
            Token::OpenParen(o) => match parser.peek_token(1)? {
                Token::CloseParen(c) => {
                    drop(parser.next_token());
                    ExprType::Unit(o, c)
                }
                _ => return Err(InternalError::Backtrack(
                    parser.lexer.next_span_from(1),
                    Cow::Borrowed(&[Expected::Token(TokenType::CloseParen)])
                )),
            }
            // defer struct type resolution until the typechecker
            // otherwise using struct B as field struct A won't work if B is defined after A
            Token::Ident(i) => {
                if let Some(generic) = parser.get_generic(i.ident) {
                    ExprType::Generic(generic)
                } else {
                    drop(parser.next_token());
                    let generics = parser.parse(depth.last())?;
                    return Ok(ExprType::UserType(i, generics))
                }
            },
            _ => return Err(InternalError::Backtrack(
                parser.lexer.next_span(),
                Cow::Borrowed(&[Expected::Type])
            )),
        };
        drop(parser.next_token());
        Ok(res)
    }
}
impl
<'a, 'i> From<Generic<'i>> for ExprType<'a, 'i> {
    fn from(g: Generic<'i>) -> Self {
        ExprType::Generic(g)
    }
}
impl<'a, 'i> Spanned for ExprType<'a, 'i> {
    fn span(&self) -> Span {
        match self {
            ExprType::String(t) => t.span(),
            ExprType::Int(t) => t.span(),
            ExprType::Float(t) => t.span(),
            ExprType::Bool(t) => t.span(),
            ExprType::Unit(o, c) => Span::new(o.span.file, o.span.start, c.span.end),
            ExprType::UserType(ut, g) => match g {
                Some((_open, _g, close)) => Span::new(ut.span().file, ut.span().start, close.span.end),
                None => ut.span()
            }
            ExprType::Generic(g) => g.span(),
        }
    }
}
impl<'a, 'i> Display for ExprType<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::String(_) => write!(f, "string"),
            ExprType::Int(_) => write!(f, "int"),
            ExprType::Float(_) => write!(f, "float"),
            ExprType::Bool(_) => write!(f, "bool"),
            ExprType::Unit(_, _) => write!(f, "()"),
            ExprType::UserType(ut, g) => {
                write!(f, "{}", ut.ident)?;
                if let Some((_open, g, _close)) = g {
                    write!(f, "{}", g)?;
                }
                Ok(())
            },
            ExprType::Generic(g) => write!(f, "{}", g.ident.ident),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprGenerics<'a, 'i> {
    pub open: TokenLessThan,
    pub generics: Option<Separated<'a, 'i, Generic<'i>, TokenComma>>,
    pub close: TokenGreaterThan,
}
impl<'a, 'i> Parse<'a, 'i> for ExprGenerics<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let open = parser.parse(depth.next())?;
        let generics: Option<Separated<NewGeneric, _>> = parser.parse(depth.next())?;
        let close = parser.parse(depth.last())?;
        Ok(ExprGenerics {
            open,
            generics: generics.map(Separated::from),
            close,
        })
    }
}
impl<'a, 'i> Display for ExprGenerics<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.generics.iter().join(", "))
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "()")]
pub struct ExprUnit {
    pub open: TokenOpenParen,
    pub close: TokenCloseParen,
}
impl<'a, 'i> Parse<'a, 'i> for ExprUnit {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprUnit {
            open: parser.parse(depth.next())?,
            close: parser.parse(depth.last())?,
        })
    }
}
impl Spanned for ExprUnit {
    fn span(&self) -> Span {
        Span::new(self.open.span.file, self.open.span.start, self.close.span.end)
    }
}

#[derive(Debug, Clone)]
pub struct ExprVariable<'i> {
    pub binding: Binding<'i>,
    // The binding's span is from the definition site.
    // Variables are used for usage sites, where we need to store the span as well.
    span: Span,
}
impl<'a, 'i> Parse<'a, 'i> for ExprVariable<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let (binding, span) = Binding::parse_existing(parser, depth.last())?;
        Ok(ExprVariable { binding, span })
    }
}
impl<'i> Spanned for ExprVariable<'i> {
    fn span(&self) -> Span {
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
    fn span(&self) -> Span {
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
    fn span(&self) -> Span {
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
    fn span(&self) -> Span {
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
    fn span(&self) -> Span {
        self.string.span
    }
}

#[derive(Debug, Clone)]
pub enum ExprFormatStringPart<'a, 'i> {
    Str(&'i str),
    Escaped(&'i str),
    FmtArg(&'a Expr<'a, 'i>),
}
#[derive(Debug, Clone)]
pub struct ExprFormatString<'a, 'i> {
    pub parts: Vec<ExprFormatStringPart<'a, 'i>>,
    pub span: Span,
}
impl<'a, 'i> Parse<'a, 'i> for ExprFormatString<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let fmtstr: TokenFormatString = parser.parse(depth.next())?;
        let mut parts = Vec::new();

        for part in fmtstr.parts {
            match part {
                TokenFormatStringPart::Str(s) => parts.push(ExprFormatStringPart::Str(s)),
                TokenFormatStringPart::Escaped(s) => parts.push(ExprFormatStringPart::Escaped(s)),
                TokenFormatStringPart::FormatArg(s, part_start) => {
                    // skip `{`
                    // let part_start = part_start + 1;
                    let part_end = part_start + s.len();
                    let part_lexer = Lexer::new_in(parser.diagnostics, fmtstr.span.file, part_start, part_end);
                    let old_lexer = std::mem::replace(&mut parser.lexer, part_lexer);

                    let expr = parser.parse_scoped(depth.next())?;
                    parts.push(ExprFormatStringPart::FmtArg(expr));

                    let part_lexer = std::mem::replace(&mut parser.lexer, old_lexer);
                    let next = part_lexer.next();
                    if !matches!(next, Ok(Token::Eof(_))) {
                        parser.diagnostics.error(ErrorCode::InvalidFormatString)
                            .with_error_label(fmtstr.span, "in this format string")
                            .with_error_label(Span::new(fmtstr.span.file, part_start, part_end), "in this format argument")
                            .emit();
                    }
                }
            }
        }

        Ok(ExprFormatString {
            parts,
            span: fmtstr.span,
        })
    }
}
impl<'a, 'i> Spanned for ExprFormatString<'a, 'i> {
    fn span(&self) -> Span {
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
                ExprFormatStringPart::FmtArg(s) => write!(f, "{{{}}}", s)?,
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
}
impl<'a, 'i> Parse<'a, 'i> for ExprBind<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // TODO
        // match self.consume_until(&[TokenType::Semicolon, TokenType::CloseCurly]) {
        //     Consumed::Found(span, _typ) => self.diagnostic_expected(ErrorCode::InvalidLetBinding, Span::new(let_span.file, let_span.start, span.end), &expected),
        //     Consumed::InstantEof | Consumed::Eof(_) => self.diagnostic_expected(ErrorCode::IncompleteLetBinding, let_span, &expected),
        // }
        // // recover with new rogue binding
        // let rogue_binding = self.add_rogue_binding("rogue_binding", is_mut, let_span);
        // self.arena.alloc(Expr::new(let_span, ExprType::Bind(rogue_binding, self.arena.alloc(Expr::new(let_span, ExprType::Unit)))))
        Ok(ExprBind {
            let_token: parser.parse(depth.next())?,
            pattern: parser.parse(depth.next())?,
            assign: parser.parse(depth.next())?,
            expr: parser.parse(depth.last())?,
        })
    }
}
impl<'a, 'i> Spanned for ExprBind<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.let_token.span.file, self.let_token.span.start, self.expr.span().end)
    }
}
impl<'a, 'i> Display for ExprBind<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.pattern, self.expr)
    }
}

#[derive(Debug, Clone)]
pub struct ExprAssign<'a, 'i> {
    pub lhs: ExprAssignLhs<'a, 'i>,
    pub assign: TokenAssign,
    pub expr: &'a Expr<'a, 'i>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprAssign<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let lhs: ExprAssignLhs = parser.parse(depth.next())?;
        let assign = parser.parse(depth.next())?;
        let expr = parser.parse(depth.last())?;
        // check mutability
        Ok(ExprAssign { lhs, assign, expr })
    }
}
impl<'a, 'i> Spanned for ExprAssign<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.lhs.span().file, self.lhs.span().start, self.expr.span().end)
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
    fn span(&self) -> Span {
        match self {
            ExprAssignLhs::Variable(v) => v.span(),
            ExprAssignLhs::FieldAccess(fa) => fa.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprFieldAccess<'a, 'i> {
    pub variable: ExprVariable<'i>,
    pub dot: TokenDot,
    pub fields: Separated<'a, 'i, TokenIdent<'i>, TokenDot>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprFieldAccess<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let variable: ExprVariable = parser.parse(depth.next())?;
        let dot: TokenDot = parser.parse(depth.next())?;
        let fields: Separated<TokenIdent, TokenDot> = parser.parse(depth.last())?;
        // check length
        if fields.is_empty() {
            return Err(InternalError::Backtrack(parser.lexer.next_span(), Cow::Borrowed(&[Expected::Token(TokenType::Ident)])));
        }
        // check trailing dot
        if fields.is_terminated() {
            parser.diagnostics.error(ErrorCode::InvalidExpression)
                .with_error_label(fields.span().unwrap(), "trailing dot")
                .emit();
        }
        Ok(ExprFieldAccess { variable, dot, fields })
    }
}
impl<'a, 'i> Spanned for ExprFieldAccess<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.variable.span.file, self.variable.span.start, self.fields.span().unwrap().end)
    }
}
impl<'a, 'i> Display for ExprFieldAccess<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.variable, self.fields.iter().map(|f| f.ident).join("."))
    }
}

#[derive(Debug, Clone)]
pub struct ExprMethodCall<'a, 'i> {
    pub variable: ExprVariable<'i>,
    pub dot: TokenDot,
    pub fields: Vec<(TokenIdent<'i>, TokenDot)>,
    pub fn_call: ExprFunctionCall<'a, 'i>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprMethodCall<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let variable: ExprVariable = parser.parse(depth.next())?;
        let dot: TokenDot = parser.parse(depth.next())?;
        // try to parse function after every field
        let mut fields = Vec::new();
        let fn_call = loop {
            match ExprFunctionCall::parse(parser, depth.next()) {
                Ok(fn_call) => break fn_call,
                Err(InternalError::Backtrack(..)) => fields.push(parser.parse(depth.next())?),
                Err(InternalError::Error(e)) => return Err(InternalError::Error(e)),
            }
        };
        Ok(ExprMethodCall { variable, dot, fields, fn_call })
    }
}
impl<'a, 'i> Spanned for ExprMethodCall<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.variable.span.file, self.variable.span.start, self.fn_call.span().end)
    }
}
impl<'a, 'i> Display for ExprMethodCall<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let fields = self.fields.iter().map(|(f, _)| format!("{}.", f.ident)).join(".");
        write!(f, "{}.{}{}", self.variable, fields, self.fn_call)
    }
}

#[derive(Debug, Clone)]
pub struct ExprBoolNot<'a, 'i> {
    pub bang: TokenBang,
    pub expr: &'a Expr<'a, 'i>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprBoolNot<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprBoolNot {
            bang: parser.parse(depth.next())?,
            expr: parser.parse(depth.last())?,
        })
    }
}
impl<'a, 'i> Spanned for ExprBoolNot<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.bang.span.file, self.bang.span.start, self.expr.span().end)
    }
}
impl<'a, 'i> Display for ExprBoolNot<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "!{}", self.expr)
    }
}

macro_rules! binop {
    ($($exprname:ident, $name:ident, $token:ident, $tokenvariant:ident;)+) => {
        $(
            #[derive(Debug, Clone)]
            pub struct $exprname<'a, 'i> {
                pub a: &'a Expr<'a, 'i>,
                pub op: $token,
                pub b: &'a Expr<'a, 'i>,
            }
            impl<'a, 'i> $exprname<'a, 'i> {
                pub(in crate::parser) fn new_as_expr(a: &'a Expr<'a, 'i>, op: Token<'i>, b: &'a Expr<'a, 'i>) -> Expr<'a, 'i> {
                    let op = match op {
                        Token::$tokenvariant(token @ $token { .. }) => token,
                        _ => unreachable!(),
                    };
                    Expr::$name(Self { a, op, b })
                }
            }
            impl<'a, 'i> Parse<'a, 'i> for $exprname<'a, 'i> {
                fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
                    Ok($exprname {
                        a: parser.parse(depth.next())?,
                        op: parser.parse(depth.next())?,
                        b: parser.parse(depth.last())?,
                    })
                }
            }
            impl<'a, 'i> Spanned for $exprname<'a, 'i> {
                fn span(&self) -> Span {
                    Span::new(self.a.span().file, self.a.span().start, self.b.span().end)
                }
            }
            impl<'a, 'i> Display for $exprname<'a, 'i> {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    write!(f, "{} {}{}", self.a, self.op, self.b)
                }
            }
        )+
    }
}
binop! {
    ExprAdd, Add, TokenPlus, Plus;
    ExprSub, Sub, TokenMinus, Minus;
    ExprMul, Mul, TokenStar, Star;
    ExprDiv, Div, TokenSlash, Slash;
    ExprBoolAnd, BoolAnd, TokenDoubleAmp, DoubleAmp;
    ExprBoolOr, BoolOr, TokenDoublePipe, DoublePipe;
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
        let open = parser.parse(depth.next())?;
        let body = parser.parse_scoped(depth.next())?;
        let close = parser.parse(depth.last())?;
        Ok(ExprBlock { open, body, close })
    }
}
impl<'a, 'i> Spanned for ExprBlock<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.open.span.file, self.open.span.start, self.close.span.end)
    }
}
impl<'a, 'i> Display for ExprBlock<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        let mut padded = PadFmt::new(&mut *f);
        for (i, expr) in self.body.exprs.iter().enumerate() {
            write!(&mut padded, "{}", expr)?;
            if i == self.body.exprs.len() - 1 && !self.body.terminated_with_semicolon {
                writeln!(padded)?;
            } else {
                writeln!(padded, ";")?;
            }
        }
        writeln!(f, "}}")
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
            // TODO: recover
            // match parser.consume_until(&[TokenType::Semicolon, TokenType::CloseCurly]) {
            //     Consumed::InstantEof | Consumed::Eof(_) => {
            //         return Err(InternalError::Backtrack(expr.span(), Cow::Borrowed(&[Expected::Token(TokenType::Semicolon), Expected::Token(TokenType::CloseCurly)])))
            //     }
            //     Consumed::Found(span, Token::Semicolon(_)) => {
            //
            //     }
            // }
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
                    .with_info_label(Span::new(span.file, span.end, span.end), "try adding a semicolon here")
                    .emit(),
            }
            if trailing_semicolon {
                last = Last::TerminatedWithSemicolon;
            } else {
                last = match expr {
                    Expr::FunctionDefinition(_)
                    | Expr::StructDefinition(_)
                    | Expr::EnumDefinition(_)
                    | Expr::ImplBlock(_)
                    | Expr::IfElse(_)
                    | Expr::Match(_)
                    | Expr::While(_)
                    | Expr::Block(_) => Last::Terminated,
                    _ => Last::Unterminated(expr.span()),
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
}
impl<'a, 'i> Parse<'a, 'i> for ExprParenthesized<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // TODO
        // self.diagnostics.error(ErrorCode::UnclosedParen)
        //     .with_error_label(span, "unclosed parenthesis")
        //     .with_info_label(Span::new(expr.span.file, expr.span.end, expr.span.end), "try inserting a `)` here")
        //     .emit();
        Ok(ExprParenthesized {
            open: parser.parse(depth.next())?,
            expr: parser.parse(depth.next())?,
            close: parser.parse(depth.last())?,
        })
    }
}
impl<'a, 'i> Spanned for ExprParenthesized<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.open.span.file, self.open.span.start, self.close.span.end)
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
        let if_token = parser.parse(depth.next())?;
        let condition = Expr::try_parse_until_including(parser, ParseUntil::All, depth.next())?;
        Ok(ExprIfElse {
            if_token,
            condition,
            then: parser.parse(depth.next())?,
            else_ifs: parser.parse(depth.next())?,
            els: parser.parse(depth.last())?,
        })
    }
}
impl<'a, 'i> Spanned for ExprIfElse<'a, 'i> {
    fn span(&self) -> Span {
        let end = self.els.as_ref().map(|(_, block)| block.span().end)
            .or_else(|| self.else_ifs.last().map(|(_, _, _, block)| block.span().end))
            .unwrap_or(self.then.span().end);
        Span::new(self.if_token.span.file, self.if_token.span.start, end)
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
}
impl<'a, 'i> Parse<'a, 'i> for ExprMatch<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let match_token: TokenMatch = parser.parse(depth.next())?;
        let expr = parser.parse(depth.next())?;
        let open = parser.parse(depth.next())?;
        let arms: Separated<'a, 'i, Scoped<(ExprMatchPattern<'a, 'i>, TokenFatArrow, &'a Expr<'a, 'i>)>, TokenComma> = parser.parse(depth.next())?;
        let close: TokenCloseCurly = parser.parse(depth.last())?;

        Ok(ExprMatch {
            match_token,
            expr,
            open,
            arms: arms.into_iter().map(|Scoped((pattern, arrow, expr))| (pattern, arrow, expr)).collect(),
            close,
        })
    }
}
impl<'a, 'i> Spanned for ExprMatch<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.match_token.span.file, self.match_token.span.start, self.close.span.end)
    }
}
impl<'a, 'i> Display for ExprMatch<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "match {} {{", self.expr)?;
        let mut padded = PadFmt::new(&mut *f);
        for (pat, _, expr) in &self.arms {
            writeln!(padded, "{} => {},", pat, expr)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprWhile<'a, 'i> {
    pub while_token: TokenWhile,
    pub condition: &'a Expr<'a, 'i>,
    pub block: ExprBlock<'a, 'i>,
}
impl<'a, 'i> Parse<'a, 'i> for ExprWhile<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let while_token = parser.parse(depth.next())?;
        let condition = Expr::try_parse_until_including(parser, ParseUntil::All, depth.next())?;
        Ok(ExprWhile {
            while_token,
            condition,
            block: parser.parse(depth.next())?,
        })
    }
}
impl<'a, 'i> Spanned for ExprWhile<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.while_token.span.file, self.while_token.span.start, self.block.span().end)
    }
}
impl<'a, 'i> Display for ExprWhile<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "while {} {}", self.condition, self.block)
    }
}

#[derive(Debug, Clone)]
pub struct ExprFunctionCall<'a, 'i> {
    pub name: TokenIdent<'i>,
    pub open: TokenOpenParen,
    pub args: Separated<'a, 'i, &'a Expr<'a, 'i>, TokenComma>,
    pub close: TokenCloseParen,
}
impl<'a, 'i> Parse<'a, 'i> for ExprFunctionCall<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let name = (|| {
            let pathed = (|| {
                let mark = parser.lexer.mark();
                let name: TokenIdent = parser.parse(depth.next())?;
                let _dcolon: TokenDoubleColon = parser.parse(depth.next())?;
                let ident: TokenIdent = parser.parse(depth.last())?;
                let span = Span::new(name.span.file, name.span.start, ident.span.end);
                let full = TokenIdent {
                    span,
                    ident: parser.diagnostics.resolve_span(span),
                };
                mark.apply();
                Ok(full)
            })();

            let err1 = match pathed {
                Ok(pathed) => return Ok(pathed),
                Err(e) => e,
            };
            let err2 = match TokenIdent::parse(parser, depth.last()) {
                Ok(ident) => return Ok(ident),
                Err(e) => e,
            };
            Err(helper::last_error(&[err1, err2]))
        })()?;

        let open = parser.parse(depth.next())?;
        let args = parser.parse(depth.next())?;
        let close = parser.parse(depth.last())?;
        // TODO
        // let close = match parser.parse() {
        //     Ok(close) => close,
        //     Err(InternalError::Error(e)) => return Err(InternalError::Error(e)),
        //     Err(InternalError::Backtrack(span, _)) => match parser.consume_until(&[TokenType::Comma, TokenType::CloseParen]) {
        //         Consumed::Found(span, Token::CloseParen(close)) => {
        //             self.diagnostics.error(ErrorCode::InvalidFunctionArgument)
        //                 .with_error_label(Span::new(span.file, span.start, span.end-1), "can't parse this argument as expression")
        //                 .with_note("arguments must be separated with commas")
        //                 .emit();
        //             // consume closeparen
        //             drop(parser.next_token());
        //
        //         }
        //         Consumed::Eof(span) => break span,
        //         Consumed::InstantEof => break open_paren_span,
        //     }
        // }
        Ok(ExprFunctionCall { name, open, args, close })
    }
}
impl<'a, 'i> Spanned for ExprFunctionCall<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.name.span.file, self.name.span.start, self.close.span.end)
    }
}
impl<'a, 'i> Display for ExprFunctionCall<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name.ident, self.args)
    }
}

#[derive(Debug, Clone)]
pub struct ExprFunctionDefinition<'a, 'i> {
    pub fn_token: TokenFn,
    pub name: TokenIdent<'i>,
    pub generics: Option<ExprGenerics<'a, 'i>>,
    pub open: TokenOpenParen,
    pub self_arg: Option<Binding<'i>>,
    pub self_arg_comma: Option<TokenComma>,
    pub args: Separated<'a, 'i, ExprPatternTyped<'a, 'i>, TokenComma>,
    pub close: TokenCloseParen,
    pub ret_type: Option<(TokenArrow, ExprType<'a, 'i>)>,
    pub body: ExprBlock<'a, 'i>,
}
impl<'a, 'i> ExprFunctionDefinition<'a, 'i> {
    pub fn arg_span(&self) -> Span {
        Span::new(self.open.span.file, self.open.span.start, self.close.span.end)
    }
}
impl<'a, 'i> Parse<'a, 'i> for ExprFunctionDefinition<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // scope for generics and argument-bindings
        let _scope_guard = parser.push_scope();
        let fn_token = parser.parse(depth.next())?;
        let name = parser.parse(depth.next())?;
        let generics = parser.parse(depth.next())?;
        let open = parser.parse(depth.next())?;
        let self_with_args: Option<(SelfBinding<'i>, Option<(TokenComma, Separated<'a, 'i, ExprPatternTyped<'a, 'i>, TokenComma>)>)> = parser.parse(depth.next())?;
        let (self_arg, self_arg_comma, args) = match self_with_args {
            Some((self_binding, rest)) => match rest {
                Some((comma, args)) => (Some(self_binding.b), Some(comma), args),
                None => (Some(self_binding.b), None, Separated::default()),
            }
            None => (None, None, parser.parse(depth.next())?),
        };
        let close = parser.parse(depth.next())?;
        let ret_type = parser.parse(depth.next())?;
        let body = parser.parse(depth.last())?;

        Ok(ExprFunctionDefinition { fn_token, name, generics, open, self_arg, self_arg_comma, args, close, ret_type, body })
    }
}
impl<'a, 'i> Spanned for ExprFunctionDefinition<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.fn_token.span.file, self.fn_token.span.start, self.body.span().end)
    }
}
impl<'a, 'i> Display for ExprFunctionDefinition<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}({}) ", self.name.ident, self.args)?;
        if let Some((_arrow, ret_type)) = &self.ret_type {
            write!(f, "-> {} ", ret_type)?;
        }
        write!(f, "{}", self.body)
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
}
impl<'a, 'i> Parse<'a, 'i> for ExprStructDefinition<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // scope for generics
        let _scope_guard = parser.push_scope();
        Ok(ExprStructDefinition {
            struct_token: parser.parse(depth.next())?,
            name: parser.parse(depth.next())?,
            generics: parser.parse(depth.next())?,
            open: parser.parse(depth.next())?,
            fields: parser.parse(depth.next())?,
            close: parser.parse(depth.last())?,
        })
    }
}
impl<'a, 'i> Spanned for ExprStructDefinition<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.struct_token.span.file, self.struct_token.span.start, self.close.span.end)
    }
}
impl<'a, 'i> Display for ExprStructDefinition<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {} {{ ", self.name.ident)?;
        for (name, _comma, typ) in &self.fields {
            write!(f, "{}: {}, ", name.ident, typ)?;
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
}
impl<'a, 'i> Parse<'a, 'i> for ExprStructInitialization<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        let name: TokenIdent = parser.parse(depth.next())?;
        let open = parser.parse(depth.next())?;
        let fields: ExprStructInitFields = parser.parse(depth.next())?;
        let close: TokenCloseCurly = parser.parse(depth.next())?;
        Ok(ExprStructInitialization {
            name,
            open,
            fields,
            close,
        })
    }
}
impl<'a, 'i> Spanned for ExprStructInitialization<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.name.span.file, self.name.span.start, self.close.span.end)
    }
}
impl<'a, 'i> Display for ExprStructInitialization<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {{ ", self.name.ident)?;
        for (name, _comma, expr) in &self.fields {
            write!(f, "{}: {}, ", name.ident, expr)?;
        }
        write!(f, "}}")
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
}
impl<'a, 'i> Parse<'a, 'i> for ExprEnumDefinition<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // scope for generics
        let _scope_guard = parser.push_scope();
        Ok(ExprEnumDefinition {
            enum_token: parser.parse(depth.next())?,
            name: parser.parse(depth.next())?,
            generics: parser.parse(depth.next())?,
            open: parser.parse(depth.next())?,
            variants: parser.parse(depth.next())?,
            close: parser.parse(depth.last())?,
        })
    }
}
impl<'a, 'i> Spanned for ExprEnumDefinition<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.enum_token.span.file, self.enum_token.span.start, self.close.span.end)
    }
}
impl<'a, 'i> Display for ExprEnumDefinition<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "enum {} {{", self.name.ident)?;
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
}
impl<'a, 'i> Parse<'a, 'i> for ExprEnumInitialization<'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        Ok(ExprEnumInitialization {
            enum_name: parser.parse(depth.next())?,
            double_colon: parser.parse(depth.next())?,
            variant_name: parser.parse(depth.next())?,
        })
    }
}
impl<'i> Spanned for ExprEnumInitialization<'i> {
    fn span(&self) -> Span {
        Span::new(self.enum_name.span.file, self.enum_name.span.start, self.variant_name.span.end)
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
}
impl<'a, 'i> Parse<'a, 'i> for ExprImplBlock<'a, 'i> {
    fn parse_marked(parser: &mut Parser<'a, '_, 'i>, depth: Depth) -> Result<Self, InternalError> {
        // scope for generics
        let _scope_guard = parser.push_scope();
        let impl_token = parser.parse(depth.next())?;
        let name: TokenIdent = parser.parse(depth.next())?;
        let generics: Option<ExprGenerics> = parser.parse(depth.next())?;
        let open = parser.parse(depth.next())?;
        let mut functions: Vec<ExprFunctionDefinition<'a, 'i>> = parser.parse(depth.next())?;
        let close = parser.parse(depth.last())?;

        // desugar self-args: insert self-args into args
        for fun in &mut functions {
            if let Some(self_arg) = fun.self_arg {
                let typed = ExprPatternTyped {
                    pattern: ExprPatternUntyped {
                        binding: self_arg,
                    },
                    colon_token: TokenColon {
                        span: Span::new(self_arg.ident.span.file, self_arg.ident.span.end, self_arg.ident.span.end),
                    },
                    typ: ExprType::UserType(TokenIdent {
                        ident: name.ident,
                        span: self_arg.ident.span,
                    }, match generics.clone() {
                        Some(ExprGenerics { open, generics: Some(generics), close }) => {
                            Some((open, Box::new(Separated::from(generics)), close,))
                        }
                        _ => None,
                    }),

                };
                fun.args.prepend(typed, fun.self_arg_comma);
            }
        }

        Ok(ExprImplBlock { impl_token, name, generics, open, functions, close })
    }
}
impl<'a, 'i> Spanned for ExprImplBlock<'a, 'i> {
    fn span(&self) -> Span {
        Span::new(self.impl_token.span.file, self.impl_token.span.start, self.close.span.end)
    }
}
impl<'a, 'i> Display for ExprImplBlock<'a, 'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "impl {} {{", self.name.ident)?;
        let mut padded = PadFmt::new(&mut *f);
        for function in &self.functions {
            writeln!(padded, "{}", function)?;
        }
        writeln!(f, "}}")
    }
}
