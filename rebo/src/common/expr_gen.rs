use std::cell::{Cell, RefCell};
use std::iter;
use std::rc::Rc;
use diagnostic::{Diagnostics, ErrorCode, FileId, Span};
use indexmap::IndexSet;
use typed_arena::Arena;
use rebo::lexer::{TokenBoolType, TokenCloseCurly, TokenCloseParen, TokenColon, TokenComma, TokenDot, TokenFatArrow, TokenFloatType, TokenGreaterThan, TokenIdent, TokenIntType, TokenLessThan, TokenLoop, TokenMatch, TokenMut, TokenOpenCurly, TokenStringType, TokenUnderscore};
use rebo::parser::{Binding, BindingId, ExprAccess, ExprBlock, ExprFunctionDefinition, ExprImplBlock, ExprLabelDef, ExprLoop, ExprMatch, ExprMatchPattern, ExprMethodCall, ExprPatternTyped, ExprReturn, ExprStructDefinition, ExprStructInitialization, ExprType, FieldOrMethod, Generic};
use crate::lexer::{Radix, TokenApostrophe, TokenArrow, TokenAssign, TokenBang, TokenBool, TokenDoubleColon, TokenDqString, TokenFloat, TokenFn, TokenImpl, TokenInteger, TokenMinus, TokenOpenParen, TokenReturn, TokenStruct};
use crate::parser::{BlockBody, Expr, ExprAssign, ExprAssignLhs, ExprBool, ExprBoolNot, ExprEnumInitialization, ExprFieldAccess, ExprFloat, ExprFunctionCall, ExprFunctionSignature, ExprGenerics, ExprInteger, ExprLabel, ExprLiteral, ExprNeg, ExprParenthesized, ExprPatternUntyped, ExprString, ExprUnit, ExprVariable};

pub trait BuildExpr<'a, 'i> {
    type Expr;
    fn build_expr(self, gen: &ExprGen<'a, 'i>) -> Self::Expr;
}

pub struct ExprGen<'a, 'i> {
    indent: Cell<usize>,
    arena: &'a Arena<Expr<'a, 'i>>,
    file_name: &'static str,
    code: RefCell<String>,
}

impl<'a, 'i> ExprGen<'a, 'i> {
    /// Add the provided snippet and indent future code
    #[must_use]
    fn next_fake_span_indent(&self, snippet: &str) -> Span {
        let span = self.next_fake_span(snippet);
        self.indent();
        span
    }
    fn indent(&self) {
        let indent = self.indent.get();
        self.indent.set(indent + 4);
    }
    /// Unindent, then add the provided snippet
    #[must_use]
    fn next_fake_span_unindent(&self, snippet: &str) -> Span {
        self.unindent();
        self.next_fake_span(snippet)
    }
    fn unindent(&self) {
        let indent = self.indent.get();
        assert!(indent >= 4);
        self.indent.set(indent - 4);
    }
    #[must_use]
    fn next_fake_span(&self, snippet: &str) -> Span {
        assert_ne!(snippet.len(), 0);
        let mut code = self.code.borrow_mut();
        let start = code.len();
        // apply indentation
        for c in snippet.chars() {
            code.push(c);
            if c == '\n' {
                code.extend(iter::repeat(' ').take(self.indent.get()))
            }
        }
        let end = code.len();
        Span::new(FileId::synthetic(self.file_name), start, end)
    }
}

#[must_use]
pub struct ExprBuilder<'a, 'i> {
    expr: Box<dyn FnOnce(&ExprGen<'a, 'i>) -> &'a Expr<'a, 'i> + 'a>,
}
impl<'a, 'i> ExprBuilder<'a, 'i> {
    pub fn from_expr(expr: &'a Expr<'a, 'i>) -> Self {
        Self::make(move |gen| ExprParenthesized {
            open: TokenOpenParen { span: gen.next_fake_span("(") },
            expr: {
                let _ = gen.next_fake_span(&expr.to_string());
                expr
            },
            close: TokenCloseParen { span: gen.next_fake_span(") ") },
        })
    }
    pub fn unit() -> Self {
        Self::make(|gen| ExprLiteral::Unit(ExprUnit {
            open: TokenOpenParen { span: gen.next_fake_span("(") },
            close: TokenCloseParen { span: gen.next_fake_span(") ") },
        }))
    }
    pub fn literal<T: Into<ExprLiteralBuilder>>(t: T) -> Self {
        let lit = t.into();
        Self::make(move |gen| lit.build(gen))
    }
    pub fn binding_existing(binding: Binding<'i>) -> ExprBuilderBinding {
        binding.into()
    }
    pub fn binding_new(name: &'i str, mutable: bool) -> ExprBuilderBinding {
        ExprBuilderBinding {
            inner: Rc::new(RefCell::new(ExprBuilderBindingInner {
                id: BindingId::unique(),
                name,
                mutable,
                mut_span: None,
                def_span: None,
            })),
        }
    }

    // FormatString(ExprFormatString<'a, 'i>),
    // Bind(ExprBind<'a, 'i>),
    // Static(ExprStatic<'a, 'i>),
    pub fn assign(binding: impl Into<ExprBuilderBinding<'i>>) -> ExprAssignBuilder<'i> {
        ExprAssignBuilder { binding: binding.into(), fields: Vec::new() }
    }
    // Assign(ExprAssign<'a, 'i>),
    // unops
    pub fn bool_not(inner: Self) -> Self {
        Self::make(move |gen| ExprBoolNot {
            bang: TokenBang { span: gen.next_fake_span("!") },
            expr: inner.build_expr(gen),
        })
    }
    pub fn neg(inner: Self) -> Self {
        Self::make(move |gen| ExprNeg {
            minus: TokenMinus { span: gen.next_fake_span("-") },
            expr: inner.build_expr(gen),
        })
    }
    // // binops
    // Add(ExprAdd<'a, 'i>),
    // Sub(ExprSub<'a, 'i>),
    // Mul(ExprMul<'a, 'i>),
    // Div(ExprDiv<'a, 'i>),
    // Mod(ExprMod<'a, 'i>),
    // Xor(ExprXor<'a, 'i>),
    // BoolAnd(ExprBoolAnd<'a, 'i>),
    // BoolOr(ExprBoolOr<'a, 'i>),
    // // binop-assign
    // AddAssign(ExprAddAssign<'a, 'i>),
    // SubAssign(ExprSubAssign<'a, 'i>),
    // MulAssign(ExprMulAssign<'a, 'i>),
    // DivAssign(ExprDivAssign<'a, 'i>),
    // ModAssign(ExprModAssign<'a, 'i>),
    // XorAssign(ExprXorAssign<'a, 'i>),
    // BoolAndAssign(ExprBoolAndAssign<'a, 'i>),
    // BoolOrAssign(ExprBoolOrAssign<'a, 'i>),
    // // comparison ops
    // LessThan(ExprLessThan<'a, 'i>),
    // LessEquals(ExprLessEquals<'a, 'i>),
    // Equals(ExprEquals<'a, 'i>),
    // NotEquals(ExprNotEquals<'a, 'i>),
    // GreaterEquals(ExprGreaterEquals<'a, 'i>),
    // GreaterThan(ExprGreaterThan<'a, 'i>),
    // Block(ExprBlock<'a, 'i>),
    /// Create a new BlockBuilder, default with terminating semicolon
    pub fn block() -> ExprBlockBuilder<'a, 'i> {
        ExprBlockBuilder { exprs: Vec::new(), terminated_with_semicolon: true }
    }
    pub fn variable(binding: impl Into<ExprBuilderBinding<'i>>) -> ExprBuilder<'a, 'i> {
        let binding = binding.into();
        ExprBuilder::make(|gen| ExprBuilder::build_expr_variable(binding, gen))
    }
    fn build_expr_variable(binding: impl Into<ExprBuilderBinding<'i>>, gen: &ExprGen<'a, 'i>) -> ExprVariable<'i> {
        let binding = binding.into();
        ExprVariable {
            binding: binding.build(gen),
            span: gen.next_fake_span(binding.name()),
        }
    }
    pub fn access(binding: impl Into<ExprBuilderBinding<'i>>) -> ExprAccessBuilder<'a, 'i> {
        ExprAccessBuilder { binding: binding.into(), accesses: Vec::new() }
    }
    // Parenthesized(ExprParenthesized<'a, 'i>),
    // IfElse(ExprIfElse<'a, 'i>),
    pub fn match_(expr: ExprBuilder<'a, 'i>) -> ExprMatchBuilder<'a, 'i> {
        ExprMatchBuilder { expr, arms: Vec::new() }
    }
    pub fn match_pattern_literal<T: Into<ExprLiteralBuilder>>(lit: T) -> ExprMatchPatternBuilder<'a, 'i> {
        ExprMatchPatternBuilder::Literal(lit.into())
    }
    // TODO: match pattern enum variant
    pub fn match_pattern_binding(binding: impl Into<ExprBuilderBinding<'i>>) -> ExprMatchPatternBuilder<'a, 'i> {
        ExprMatchPatternBuilder::Binding(binding.into())
    }
    pub fn match_pattern_wildcard() -> ExprMatchPatternBuilder<'a, 'i> {
        ExprMatchPatternBuilder::Wildcard
    }
    // While(ExprWhile<'a, 'i>),
    // For(ExprFor<'a, 'i>),
    pub fn loop_(label: Option<&'i str>, block: ExprBlockBuilder<'a, 'i>) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(move |gen| ExprLoop {
            label: label.map(|label| ExprLabelDef {
                label: ExprLabel {
                    apostrophe: TokenApostrophe { span: gen.next_fake_span("'") },
                    ident: TokenIdent {
                        span: gen.next_fake_span(label),
                        ident: label,
                    }
                },
                colon: TokenColon { span: gen.next_fake_span(": ") }
            }),
            loop_token: TokenLoop { span: gen.next_fake_span("loop ") },
            block: block.build_expr(gen),
        })
    }
    // Break(ExprBreak<'a, 'i>),
    // Continue(ExprContinue<'i>),
    pub fn return_(expr: Option<ExprBuilder<'a, 'i>>) -> ExprBuilder<'a, 'i> {
        Self::make(|gen| ExprReturn {
            return_token: TokenReturn { span: gen.next_fake_span("return ") },
            expr: expr.map(|expr| expr.build_expr(gen)),
        })
    }
    // Yield(ExprYield<'a, 'i>),
    pub fn function_call(binding: impl Into<ExprBuilderBinding<'i>>) -> ExprFunctionCallBuilder<'a, 'i> {
        ExprFunctionCallBuilder { binding: binding.into(), args: Vec::new() }
    }
    // TODO: anonymous functions
    pub fn function_definition(name: &'i str) -> ExprFunctionSignatureSelfArgBuilder<'i> {
        ExprFunctionSignatureSelfArgBuilder { name }
    }
    pub fn struct_definition(name: &'i str) -> ExprStructDefinitionBuilder<'a, 'i> {
        ExprStructDefinitionBuilder { name, generics: Vec::new(), fields: Vec::new() }
    }
    pub fn struct_initialization(name: &'i str) -> ExprStructInitializationBuilder<'a, 'i> {
        ExprStructInitializationBuilder { name, fields: Vec::new() }
    }
    // EnumDefinition(ExprEnumDefinition<'a, 'i>),
    pub fn enum_initialization(enum_name: &'i str, variant_name: &'i str) -> ExprBuilder<'a, 'i> {
        Self::make(move |gen| ExprEnumInitialization {
            enum_name: TokenIdent {
                span: gen.next_fake_span(enum_name),
                ident: enum_name,
            },
            double_colon: TokenDoubleColon { span: gen.next_fake_span("::") },
            variant_name: TokenIdent {
                span: gen.next_fake_span(variant_name),
                ident: variant_name,
            },
        })
    }
    pub fn impl_block(target: &'i str) -> ExprImplBlockBuilder<'a, 'i> {
        ExprImplBlockBuilder { target, functions: Vec::new() }
    }

    // TYPES
    pub fn from_expr_type(typ: ExprType<'a, 'i>) -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Parenthesized(typ) }
    }
    pub fn string_type() -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::String }
    }
    pub fn int_type() -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Int }
    }
    pub fn float_type() -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Float }
    }
    pub fn bool_type() -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Bool }
    }
    pub fn unit_type() -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Unit }
    }
    /// struct or enum type
    pub fn user_type(name: &'i str) -> ExprUserTypeBuilder<'a, 'i> {
        ExprUserTypeBuilder { name, generics: Vec::new() }
    }
    pub fn never_type() -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Never }
    }
    pub fn any_type() -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Any }
    }

    // builder functions

    fn make<E: Into<Expr<'a, 'i>>, F: FnOnce(&ExprGen<'a, 'i>) -> E + 'a>(f: F) -> Self {
        Self { expr: Box::new(move |gen| gen.arena.alloc(f(gen).into())) }
    }
    pub fn generate<B: BuildExpr<'a, 'i>>(
        builder: B,
        arena: &'a Arena<Expr<'a, 'i>>,
        diagnostics: &Diagnostics<impl ErrorCode>,
        file_name: &'static str
    ) -> B::Expr {
        let gen = ExprGen {
            indent: Cell::new(0),
            arena,
            file_name,
            code: RefCell::new(String::new()),
        };
        let expr = builder.build_expr(&gen);
        diagnostics.add_synthetic_file(gen.file_name, gen.code.into_inner());
        expr
    }
}
impl<'a, 'i> BuildExpr<'a, 'i> for ExprBuilder<'a, 'i> {
    type Expr = &'a Expr<'a, 'i>;
    fn build_expr(self, gen: &ExprGen<'a, 'i>) -> Self::Expr {
        (self.expr)(gen)
    }
}

#[must_use]
#[derive(Clone)]
pub struct ExprBuilderBinding<'i> {
    // Bindings must always have one single span where they are defined.
    // Thus, we must lazily initialize that def-span only once.
    inner: Rc<RefCell<ExprBuilderBindingInner<'i>>>,
}
struct ExprBuilderBindingInner<'i> {
    id: BindingId,
    name: &'i str,
    mutable: bool,
    mut_span: Option<Span>,
    def_span: Option<Span>,
}
impl<'i> From<Binding<'i>> for ExprBuilderBinding<'i> {
    fn from(binding: Binding<'i>) -> Self {
        // existing Bindings don't require wrapping in new spans as any usage
        // of the binding will have a new use-span in the ExprVariable
        ExprBuilderBinding {
            inner: Rc::new(RefCell::new(ExprBuilderBindingInner {
                id: binding.id,
                name: binding.ident.ident,
                mutable: binding.mutable.is_some(),
                mut_span: binding.mutable.map(|mutable| mutable.span),
                def_span: Some(binding.ident.span),
            })),
        }
    }
}
impl<'x, 'i> From<&'x ExprBuilderBinding<'i>> for ExprBuilderBinding<'i> {
    fn from(value: &'x ExprBuilderBinding<'i>) -> Self {
        value.clone()
    }
}
impl<'i> ExprBuilderBinding<'i> {
    pub fn id(&self) -> BindingId {
        self.inner.borrow().id
    }
    pub fn name(&self) -> &'i str {
        self.inner.borrow().name
    }
    fn build<'a>(&self, gen: &ExprGen<'a, 'i>) -> Binding<'i> {
        let (needs_def_span, needs_mut_span) = {
            let inner = self.inner.borrow();
            (
                inner.def_span.is_none(),
                inner.mut_span.is_none() && inner.mutable,
            )
        };
        if needs_mut_span {
            self.inner.borrow_mut().mut_span = Some(gen.next_fake_span("mut "));
        }
        if needs_def_span {
            let def_span = gen.next_fake_span(self.inner.borrow().name);
            self.inner.borrow_mut().def_span = Some(def_span);
        }
        let inner = self.inner.borrow();
        Binding {
            id: inner.id,
            mutable: inner.mutable.then(|| TokenMut { span: inner.mut_span.unwrap() }),
            ident: TokenIdent {
                span: inner.def_span.unwrap(),
                ident: inner.name,
            },
            rogue: false,
        }
    }
}

#[must_use]
pub struct ExprAssignBuilder<'i> {
    binding: ExprBuilderBinding<'i>,
    fields: Vec<&'i str>,
}
impl<'i> ExprAssignBuilder<'i> {
    pub fn access_field(mut self, name: &'i str) -> ExprAssignBuilder<'i> {
        self.fields.push(name);
        self
    }
    pub fn assign<'a>(self, rhs: ExprBuilder<'a, 'i>) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(|gen| ExprAssign {
            lhs: ExprAssignLhs::FieldAccess(ExprFieldAccess {
                variable: ExprBuilder::build_expr_variable(self.binding, gen),
                dot: TokenDot { span: gen.next_fake_span(".") },
                fields: self.fields.into_iter().map(|name| (
                    TokenDot { span: gen.next_fake_span(".") },
                    TokenIdent { ident: name, span: gen.next_fake_span(name) },
                )).collect(),
            }),
            assign: TokenAssign { span: gen.next_fake_span(" = ") },
            expr: rhs.build_expr(gen),
        })
    }
}

#[must_use]
pub struct ExprBlockBuilder<'a, 'i> {
    exprs: Vec<ExprBuilder<'a, 'i>>,
    terminated_with_semicolon: bool,
}
impl<'a, 'i> ExprBlockBuilder<'a, 'i> {
    pub fn insert_expr(&mut self, index: usize, expr: ExprBuilder<'a, 'i>) -> &mut Self {
        self.exprs.insert(index, expr);
        self
    }
    pub fn push_expr(&mut self, expr: ExprBuilder<'a, 'i>) -> &mut Self {
        self.exprs.push(expr);
        self
    }
    pub fn expr(mut self, expr: ExprBuilder<'a, 'i>) -> Self {
        self.push_expr(expr);
        self
    }
    pub fn terminate_with_semicolon(&mut self) -> &mut Self {
        self.terminated_with_semicolon = true;
        self
    }
    pub fn with_terminating_semicolon(mut self) -> Self {
        self.terminate_with_semicolon();
        self
    }
    pub fn terminate_without_semicolon(&mut self) -> &mut Self {
        self.terminated_with_semicolon = false;
        self
    }
    pub fn without_terminating_semicolon(mut self) -> Self {
        self.terminate_without_semicolon();
        self
    }
    pub fn build(self) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(|gen| self.build_expr(gen))
    }
}
impl<'a, 'i> BuildExpr<'a, 'i> for ExprBlockBuilder<'a, 'i> {
    type Expr = ExprBlock<'a, 'i>;
    fn build_expr(self, gen: &ExprGen<'a, 'i>) -> Self::Expr {
        ExprBlock {
            open: TokenOpenCurly { span: gen.next_fake_span_indent("{\n") },
            body: BlockBody {
                exprs: {
                    let res = self.exprs.into_iter().map(|expr| {
                        let res = expr.build_expr(gen);
                        // fake semicolon for better readability
                        let _ = gen.next_fake_span("\n");
                        res
                    }).collect();
                    // for better readability
                    if self.terminated_with_semicolon {
                        let _ = gen.next_fake_span(";");
                    }
                    let _ = gen.next_fake_span("\n");
                    res
                },
                terminated_with_semicolon: self.terminated_with_semicolon,
            },
            close: TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        }
    }
}

#[must_use]
pub struct ExprAccessBuilder<'a, 'i> {
    binding: ExprBuilderBinding<'i>,
    accesses: Vec<ExprBuilderFieldOrMethod<'a, 'i>>,
}
enum ExprBuilderFieldOrMethod<'a, 'i> {
    Field(&'i str),
    Method(&'i str, Vec<ExprBuilder<'a, 'i>>),
}
impl<'a, 'i> ExprAccessBuilder<'a, 'i> {
    pub fn field(mut self, name: &'i str) -> Self {
        self.accesses.push(ExprBuilderFieldOrMethod::Field(name));
        self
    }
    pub fn call_method(mut self, name: &'i str, args: Vec<ExprBuilder<'a, 'i>>) -> Self {
        self.accesses.push(ExprBuilderFieldOrMethod::Method(name, args));
        self
    }
    pub fn build(self) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(move |gen| ExprAccess {
            variable: ExprBuilder::build_expr_variable(self.binding, gen),
            dot: TokenDot { span: gen.next_fake_span(".") },
            accesses: self.accesses.into_iter().map(|acc| match acc {
                ExprBuilderFieldOrMethod::Field(name) => (
                    TokenDot { span: gen.next_fake_span(".") },
                    FieldOrMethod::Field(TokenIdent {
                        span: gen.next_fake_span(name),
                        ident: name,
                    }),
                ),
                ExprBuilderFieldOrMethod::Method(name, args) => (
                    TokenDot { span: gen.next_fake_span(".") },
                    FieldOrMethod::Method(ExprMethodCall {
                        name: TokenIdent { ident: name, span: gen.next_fake_span(name) },
                        open: TokenOpenParen { span: gen.next_fake_span("(") },
                        args: args.into_iter().map(|arg| (
                            TokenComma { span: gen.next_fake_span(", ") },
                            arg.build_expr(gen),
                        )).collect(),
                        close: TokenCloseParen { span: gen.next_fake_span(")") },
                    })
                ),
            }).collect(),
        })
    }
}

#[must_use]
pub struct ExprMatchBuilder<'a, 'i> {
    expr: ExprBuilder<'a, 'i>,
    arms: Vec<(ExprMatchPatternBuilder<'a, 'i>, ExprBuilder<'a, 'i>)>,
}
impl<'a, 'i> ExprMatchBuilder<'a, 'i> {
    pub fn push_arm(&mut self, pattern: ExprMatchPatternBuilder<'a, 'i>, expr: ExprBuilder<'a, 'i>) -> &mut Self {
        self.arms.push((pattern, expr));
        self
    }
    pub fn arm(mut self, pattern: ExprMatchPatternBuilder<'a, 'i>, expr: ExprBuilder<'a, 'i>) -> Self {
        self.push_arm(pattern, expr);
        self
    }
    pub fn build(self) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(|gen| ExprMatch {
            match_token: TokenMatch { span: gen.next_fake_span("match ") },
            expr: self.expr.build_expr(gen),
            open: TokenOpenCurly { span: gen.next_fake_span_indent(" {\n") },
            arms: self.arms.into_iter().map(|(pattern, expr)| {
                let res = (
                    pattern.build_expr(gen),
                    TokenFatArrow { span: gen.next_fake_span(" => ") },
                    expr.build_expr(gen),
                );
                // for readability sake
                let _ = gen.next_fake_span(",\n");
                res
            }).collect(),
            close: TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") }
        })
    }
}
#[must_use]
#[derive(Clone)]
pub enum ExprMatchPatternBuilder<'a, 'i> {
    Literal(ExprLiteralBuilder),
    // TODO: match pattern variant
    Variant(ExprMatchPatternVariantBuilder<'a, 'i>),
    Binding(ExprBuilderBinding<'i>),
    Wildcard,
}
// WTF why is this lifetime bound required???
impl<'a, 'i: 'a> BuildExpr<'a, 'i> for ExprMatchPatternBuilder<'a, 'i> {
    type Expr = ExprMatchPattern<'a, 'i>;
    fn build_expr(self, gen: &ExprGen<'a, 'i>) -> Self::Expr {
        match self {
            ExprMatchPatternBuilder::Literal(lit) => ExprMatchPattern::Literal(lit.build(gen)),
            ExprMatchPatternBuilder::Variant(_) => todo!("match pattern variant"),
            ExprMatchPatternBuilder::Binding(binding) => ExprMatchPattern::Binding(binding.build(gen)),
            ExprMatchPatternBuilder::Wildcard => ExprMatchPattern::Wildcard(TokenUnderscore { span: gen.next_fake_span("_") }),
        }
    }
}
#[must_use]
#[derive(Clone)]
pub struct ExprMatchPatternVariantBuilder<'a, 'i> {
    // TODO: match pattern variant
    marker: std::marker::PhantomData<(&'a (), &'i ())>,
}

#[must_use]
pub struct ExprFunctionCallBuilder<'a, 'i> {
    binding: ExprBuilderBinding<'i>,
    args: Vec<ExprBuilder<'a, 'i>>,
}
impl<'a, 'i> ExprFunctionCallBuilder<'a, 'i> {
    pub fn arg(mut self, arg: ExprBuilder<'a, 'i>) -> ExprFunctionCallBuilder<'a, 'i> {
        self.args.push(arg);
        self
    }
    pub fn call(self) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(|gen| ExprFunctionCall {
            name: ExprBuilder::build_expr_variable(self.binding, gen),
            open: TokenOpenParen { span: gen.next_fake_span("(") },
            args: self.args.into_iter().map(|arg| (
                TokenComma { span: gen.next_fake_span(", ") },
                arg.build_expr(gen),
            )).collect(),
            close: TokenCloseParen { span: gen.next_fake_span(")") },
        })
    }
}

#[must_use]
pub struct ExprFunctionSignatureSelfArgBuilder<'i> {
    name: &'i str,
}
impl<'i> ExprFunctionSignatureSelfArgBuilder<'i> {
    /// self-arg, if provided, must be the correct type as declared on the surrounding impl-block
    pub fn self_arg<'a>(self, binding: impl Into<ExprBuilderBinding<'i>>, typ: ExprTypeBuilder<'a, 'i>) -> ExprFunctionSignatureRetTypeBuilder<'a, 'i> {
        ExprFunctionSignatureRetTypeBuilder { name: self.name, self_arg: Some((binding.into(), typ)) }
    }
    pub fn no_self_arg<'a>(self) -> ExprFunctionSignatureRetTypeBuilder<'a, 'i> {
        ExprFunctionSignatureRetTypeBuilder { name: self.name, self_arg: None }
    }
}
#[must_use]
pub struct ExprFunctionSignatureRetTypeBuilder<'a, 'i> {
    name: &'i str,
    self_arg: Option<(ExprBuilderBinding<'i>, ExprTypeBuilder<'a, 'i>)>,
}
impl<'a, 'i> ExprFunctionSignatureRetTypeBuilder<'a, 'i> {
    pub fn ret_type(self, ret_type: ExprTypeBuilder<'a, 'i>) -> ExprFunctionSignatureBuilder<'a, 'i> {
        ExprFunctionSignatureBuilder { name: self.name, self_arg: self.self_arg, ret_type: Some(ret_type), generics: Vec::new(), args: Vec::new() }
    }
    pub fn no_ret_type(self) -> ExprFunctionSignatureBuilder<'a, 'i> {
        ExprFunctionSignatureBuilder { name: self.name, self_arg: self.self_arg, ret_type: None, generics: Vec::new(), args: Vec::new() }
    }
}
#[must_use]
pub struct ExprFunctionSignatureBuilder<'a, 'i> {
    name: &'i str,
    generics: Vec<&'i str>,
    self_arg: Option<(ExprBuilderBinding<'i>, ExprTypeBuilder<'a, 'i>)>,
    /// name, typ
    args: Vec<(ExprBuilderBinding<'i>, ExprTypeBuilder<'a, 'i>)>,
    ret_type: Option<ExprTypeBuilder<'a, 'i>>,
}
impl<'a, 'i> ExprFunctionSignatureBuilder<'a, 'i> {
    pub fn push_generic(&mut self, generic: &'i str) -> &mut Self {
        self.generics.push(generic);
        self
    }
    pub fn generic(mut self, generic: &'i str) -> Self {
        self.push_generic(generic);
        self
    }
    pub fn push_arg(&mut self, binding: impl Into<ExprBuilderBinding<'i>>, typ: ExprTypeBuilder<'a, 'i>) -> &mut Self {
        self.args.push((binding.into(), typ));
        self
    }
    pub fn arg(mut self, binding: impl Into<ExprBuilderBinding<'i>>, typ: ExprTypeBuilder<'a, 'i>) -> Self {
        self.push_arg(binding, typ);
        self
    }
    pub fn body(self, body: ExprBlockBuilder<'a, 'i>) -> ExprFunctionDefinitionBuilder<'a, 'i> {
        ExprFunctionDefinitionBuilder { sig: self, body, }
    }
}
impl<'a, 'i> BuildExpr<'a, 'i> for ExprFunctionSignatureBuilder<'a, 'i> {
    type Expr = ExprFunctionSignature<'a, 'i>;
    fn build_expr(self, gen: &ExprGen<'a, 'i>) -> Self::Expr {
        ExprFunctionSignature {
            gen_token: None,
            fn_token: TokenFn { span: gen.next_fake_span("fn ") },
            name: Some(TokenIdent { ident: self.name, span: gen.next_fake_span(self.name) }),
            generics: gen_expr_generics(gen, self.generics),
            open: TokenOpenParen { span: gen.next_fake_span("(") },
            self_arg: self.self_arg.as_ref().map(|(binding, _)| binding.build(gen)),
            self_arg_comma: (self.self_arg.is_some() && !self.args.is_empty())
                .then(|| TokenComma { span: gen.next_fake_span(", ") }),
            args: self.self_arg.into_iter().chain(self.args).map(|(binding, typ)| (
                TokenComma { span: gen.next_fake_span(", ") },
                ExprPatternTyped {
                    pattern: ExprPatternUntyped { binding: binding.build(gen) },
                    colon_token: TokenColon { span: gen.next_fake_span(": ") },
                    typ: typ.build(gen),
                }
            )).collect(),
            varargs: None,
            close: TokenCloseParen { span: gen.next_fake_span(")") },
            ret_type: self.ret_type.map(|typ| (
                TokenArrow { span: gen.next_fake_span(" -> ") },
                typ.build(gen),
            )),
        }
    }
}
#[must_use]
pub struct ExprFunctionDefinitionBuilder<'a, 'i> {
    sig: ExprFunctionSignatureBuilder<'a, 'i>,
    body: ExprBlockBuilder<'a, 'i>,
}
impl<'a, 'i> ExprFunctionDefinitionBuilder<'a, 'i> {
    pub fn build(self) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(|gen| self.build_expr(gen))
    }
}
impl<'a, 'i> BuildExpr<'a, 'i> for ExprFunctionDefinitionBuilder<'a, 'i> {
    type Expr = ExprFunctionDefinition<'a, 'i>;
    fn build_expr(self, gen: &ExprGen<'a, 'i>) -> Self::Expr {
        ExprFunctionDefinition {
            sig: self.sig.build_expr(gen),
            captures: IndexSet::new(),
            body: self.body.build_expr(gen),
        }
    }
}

#[must_use]
pub struct ExprStructDefinitionBuilder<'a, 'i> {
    name: &'i str,
    generics: Vec<&'i str>,
    /// name, type
    fields: Vec<(&'i str, ExprTypeBuilder<'a, 'i>)>,
}
impl<'a, 'i> ExprStructDefinitionBuilder<'a, 'i> {
    pub fn push_generic(&mut self, generic: &'i str) -> &mut Self {
        self.generics.push(generic);
        self
    }
    pub fn generic(mut self, generic: &'i str) -> Self {
        self.push_generic(generic);
        self
    }
    pub fn push_field(&mut self, name: &'i str, typ: ExprTypeBuilder<'a, 'i>) -> &mut Self {
        self.fields.push((name, typ));
        self
    }
    pub fn field(mut self, name: &'i str, typ: ExprTypeBuilder<'a, 'i>) -> Self {
        self.push_field(name, typ);
        self
    }
    pub fn build(self) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(|gen| ExprStructDefinition {
            struct_token: TokenStruct { span: gen.next_fake_span("struct ") },
            name: TokenIdent { ident: self.name, span: gen.next_fake_span(self.name) },
            generics: gen_expr_generics(gen, self.generics),
            open: TokenOpenCurly { span: gen.next_fake_span_indent(" {\n") },
            fields: self.fields.into_iter().map(|(field, typ)| (
                TokenComma { span: gen.next_fake_span(",\n") },
                (
                    TokenIdent { ident: field, span: gen.next_fake_span(field) },
                    TokenColon { span: gen.next_fake_span(": ") },
                    typ.build(gen),
                )
            )).collect(),
            close: TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        })
    }
}
#[must_use]
pub struct ExprStructInitializationBuilder<'a, 'i> {
    name: &'i str,
    /// name, value
    fields: Vec<(&'i str, ExprBuilder<'a, 'i>)>,
}
impl<'a, 'i> ExprStructInitializationBuilder<'a, 'i> {
    pub fn push_field(&mut self, name: &'i str, value: ExprBuilder<'a, 'i>) -> &mut Self {
        self.fields.push((name, value));
        self
    }
    pub fn field(mut self, name: &'i str, value: ExprBuilder<'a, 'i>) -> Self {
        self.push_field(name, value);
        self
    }
    pub fn build(self) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(|gen| ExprStructInitialization {
            name: TokenIdent { ident: self.name, span: gen.next_fake_span(self.name) },
            open: TokenOpenCurly { span: gen.next_fake_span_indent(" {\n") },
            fields: self.fields.into_iter().map(|(field, value)| (
                TokenComma { span: gen.next_fake_span(",\n") },
                (
                    TokenIdent { ident: field, span: gen.next_fake_span(field) },
                    TokenColon { span: gen.next_fake_span(": ") },
                    value.build_expr(gen),
                )
            )).collect(),
            close: TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        })
    }
}

#[must_use]
pub struct ExprTypeBuilder<'a, 'i> {
    inner: ExprTypeBuilderInner<'a, 'i>,
}
enum ExprTypeBuilderInner<'a, 'i> {
    /// wrap an original ExprType to ensure start and end of any use belong to our file
    Parenthesized(ExprType<'a, 'i>),
    String,
    Int,
    Float,
    Bool,
    Unit,
    // struct, enum, typedef, ...
    /// name, generics
    UserType(&'i str, Vec<ExprTypeBuilder<'a, 'i>>),
    // TODO: Generic(Generic<'i>),
    // TODO: Function(Box<ExprFunctionType<'a, 'i>>),
    Never,
    Any,
}
impl<'a, 'i> ExprTypeBuilder<'a, 'i> {
    fn name(&self) -> &'i str {
        match &self.inner {
            ExprTypeBuilderInner::Parenthesized(t) => t.name(),
            ExprTypeBuilderInner::String => "string",
            ExprTypeBuilderInner::Int => "int",
            ExprTypeBuilderInner::Float => "float",
            ExprTypeBuilderInner::Bool => "bool",
            ExprTypeBuilderInner::Unit => "unit",
            ExprTypeBuilderInner::UserType(name, _) => name,
            ExprTypeBuilderInner::Never => "!",
            ExprTypeBuilderInner::Any => "any",
        }
    }
    fn build(self, gen: &ExprGen<'a, 'i>) -> ExprType<'a, 'i> {
        match self.inner {
            ExprTypeBuilderInner::Parenthesized(typ) => ExprType::Parenthesized(
                TokenOpenParen { span: gen.next_fake_span("(") },
                Box::new(typ),
                TokenCloseParen { span: gen.next_fake_span(")") },
            ),
            ExprTypeBuilderInner::String => ExprType::String(TokenStringType { span: gen.next_fake_span("string") }),
            ExprTypeBuilderInner::Int => ExprType::Int(TokenIntType { span: gen.next_fake_span("int") }),
            ExprTypeBuilderInner::Float => ExprType::Float(TokenFloatType { span: gen.next_fake_span("float") }),
            ExprTypeBuilderInner::Bool => ExprType::Bool(TokenBoolType { span: gen.next_fake_span("bool") }),
            ExprTypeBuilderInner::Unit => ExprType::Unit(
                TokenOpenParen { span: gen.next_fake_span("(") },
                TokenCloseParen { span: gen.next_fake_span(")") },
            ),
            ExprTypeBuilderInner::UserType(name, generics) => ExprType::UserType(
                TokenIdent { ident: name, span: gen.next_fake_span(name) },
                Some((
                    TokenLessThan { span: gen.next_fake_span("<") },
                    Box::new(generics.into_iter().map(|generic| (
                        TokenComma { span: gen.next_fake_span(", ") },
                        generic.build(gen),
                    )).collect()),
                    TokenGreaterThan { span: gen.next_fake_span(">") },
                ))
            ),
            ExprTypeBuilderInner::Never => ExprType::Never(TokenBang { span: gen.next_fake_span("!") }),
            ExprTypeBuilderInner::Any => ExprType::Any,
        }
    }
}
#[must_use]
pub struct ExprUserTypeBuilder<'a, 'i> {
    name: &'i str,
    generics: Vec<ExprTypeBuilder<'a, 'i>>,
}
impl<'a, 'i> ExprUserTypeBuilder<'a, 'i> {
    pub fn generic(mut self, typ: ExprTypeBuilder<'a, 'i>) -> Self {
        self.generics.push(typ);
        self
    }
    pub fn build(self) -> ExprTypeBuilder<'a, 'i> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::UserType(self.name, self.generics) }
    }
}

#[must_use]
pub struct ExprImplBlockBuilder<'a, 'i> {
    target: &'i str,
    // TODO: generics
    functions: Vec<ExprFunctionDefinitionBuilder<'a, 'i>>
}
impl<'a, 'i> ExprImplBlockBuilder<'a, 'i> {
    pub fn push_function(&mut self, function: ExprFunctionDefinitionBuilder<'a, 'i>) -> &mut Self {
        if let Some((_, typ)) = &function.sig.self_arg {
            assert_eq!(typ.name(), self.target);
            // TODO: check self-type generics
        }
        self.functions.push(function);
        self
    }
    pub fn function(mut self, function: ExprFunctionDefinitionBuilder<'a, 'i>) -> Self {
        self.push_function(function);
        self
    }
    pub fn build(self) -> ExprBuilder<'a, 'i> {
        ExprBuilder::make(|gen| ExprImplBlock {
            impl_token: TokenImpl { span: gen.next_fake_span("impl ") },
            name: TokenIdent { ident: self.target, span: gen.next_fake_span(self.target) },
            // TODO: generics
            generics: None,
            open: TokenOpenCurly { span: gen.next_fake_span_indent(" {\n") },
            functions: self.functions.into_iter()
                .map(|fun| fun.build_expr(gen))
                .collect(),
            close: TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        })
    }
}


// helpers

fn gen_expr_generics<'a, 'i>(gen: &ExprGen<'a, 'i>, generics: Vec<&'i str>) -> Option<ExprGenerics<'a, 'i>> {
    if generics.is_empty() {
        return None;
    }
    Some(ExprGenerics {
        open: TokenLessThan { span: gen.next_fake_span("<") },
        generics: Some(generics.into_iter().map(|generic| {
            let ident = TokenIdent { ident: generic, span: gen.next_fake_span(generic) };
            (
                TokenComma { span: gen.next_fake_span(", ") },
                Generic { def_ident: ident, ident },
            )
        }).collect()),
        close: TokenGreaterThan { span: gen.next_fake_span(">") },
    })
}

#[derive(Clone)]
pub enum ExprLiteralBuilder {
    Unit,
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
}
impl ExprLiteralBuilder {
    fn build<'a, 'i>(self, gen: &ExprGen<'a, 'i>) -> ExprLiteral {
        match self {
            ExprLiteralBuilder::Unit => ExprLiteral::Unit(ExprUnit {
                open: TokenOpenParen { span: gen.next_fake_span("(") },
                close: TokenCloseParen { span: gen.next_fake_span(")") },
            }),
            ExprLiteralBuilder::Integer(value) => ExprLiteral::Integer(ExprInteger { int: TokenInteger { span: gen.next_fake_span(&format!("{value:?}")), value, radix: Radix::Dec } }),
            ExprLiteralBuilder::Float(value) => ExprLiteral::Float(ExprFloat { float: TokenFloat { span: gen.next_fake_span(&format!("{value:?}")), value, radix: Radix::Dec }, }),
            ExprLiteralBuilder::Bool(value) => ExprLiteral::Bool(ExprBool { b: TokenBool { span: gen.next_fake_span(&format!("{value:?}")), value } }),
            ExprLiteralBuilder::String(string) => ExprLiteral::String(ExprString { string: TokenDqString { span: gen.next_fake_span(&format!("{string:?}")), string } })
        }
    }
}
impl From<()> for ExprLiteralBuilder {
    fn from(_: ()) -> Self {
        ExprLiteralBuilder::Unit
    }
}
impl From<bool> for ExprLiteralBuilder {
    fn from(value: bool) -> Self {
        ExprLiteralBuilder::Bool(value)
    }
}
impl From<&'static str> for ExprLiteralBuilder {
    fn from(value: &'static str) -> Self {
        ExprLiteralBuilder::String(value.to_string())
    }
}
impl From<i64> for ExprLiteralBuilder {
    fn from(value: i64) -> Self {
        ExprLiteralBuilder::Integer(value)
    }
}
impl From<f64> for ExprLiteralBuilder {
    fn from(value: f64) -> Self {
        ExprLiteralBuilder::Float(value)
    }
}
