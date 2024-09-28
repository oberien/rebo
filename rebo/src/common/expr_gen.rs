use std::cell::{Cell, RefCell};
use std::fmt::{Display, Formatter};
use std::iter;
use std::rc::Rc;
use diagnostic::{Diagnostics, ErrorCode, FileId};
use indexmap::IndexSet;
use typed_arena::Arena;
use rebo::common::SpanWithId;
use rebo::lexer::{TokenBoolType, TokenCircumflex, TokenCloseCurly, TokenCloseParen, TokenColon, TokenComma, TokenDot, TokenDoubleAmp, TokenDoublePipe, TokenEquals, TokenFatArrow, TokenFloatType, TokenGreaterEquals, TokenGreaterThan, TokenIdent, TokenIntType, TokenLessThan, TokenLoop, TokenMatch, TokenMut, TokenOpenCurly, TokenPercent, TokenSlash, TokenStringType, TokenUnderscore};
use rebo::parser::{Binding, BindingId, ExprAccess, ExprAdd, ExprBlock, ExprBoolAnd, ExprDiv, ExprEquals, ExprFunctionDefinition, ExprGreaterEquals, ExprImplBlock, ExprLabelDef, ExprLessEquals, ExprLessThan, ExprLoop, ExprMatch, ExprMatchPattern, ExprMethodCall, ExprMod, ExprMul, ExprPatternTyped, ExprReturn, ExprStructDefinition, ExprStructInitialization, ExprSub, ExprType, ExprTypeParenthesized, ExprXor, FieldOrMethod, Generic};
use crate::common::Spanned;
use crate::lexer::{Radix, TokenApostrophe, TokenArrow, TokenAssign, TokenBang, TokenBool, TokenDoubleColon, TokenDqString, TokenFloat, TokenFn, TokenImpl, TokenInteger, TokenLessEquals, TokenMinus, TokenNotEquals, TokenOpenParen, TokenPlus, TokenReturn, TokenStar, TokenStruct};
use crate::parser::{BlockBody, Expr, ExprAssign, ExprAssignLhs, ExprBool, ExprBoolNot, ExprBoolOr, ExprEnumInitialization, ExprFieldAccess, ExprFloat, ExprFunctionCall, ExprFunctionSignature, ExprGenerics, ExprGreaterThan, ExprInteger, ExprLabel, ExprLiteral, ExprNeg, ExprNotEquals, ExprParenthesized, ExprPatternUntyped, ExprString, ExprTypeUnit, ExprTypeUserType, ExprUnit, ExprVariable};

// impl<'a, 'i> BuildExpr<'a, 'i> for MyExprBuilder<'a, 'i> {
//     type Expr = Expr<'a, 'i>;
//
//     fn build_expr(&self, gen: &ExprGen<'a, 'i>) -> Expr<'a, 'i> {
//         todo!()
//     }
// }

pub trait BuildExprPart<'old> {
    type ExprPart<'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new;
}
trait BuildExpr<'old> where Self: 'old {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new>
        where 'old: 'new;
}

pub struct ExprGen<'new> {
    indent: Cell<usize>,
    arena: &'new Arena<Expr<'new, 'new>>,
    file_id: FileId,
    file_name: String,
    code: RefCell<String>,
}

impl<'new> ExprGen<'new> {
    /// Add the provided snippet and indent future code
    #[must_use]
    fn next_fake_span_indent(&self, snippet: &str) -> SpanWithId {
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
    fn next_fake_span_unindent(&self, snippet: &str) -> SpanWithId {
        self.unindent();
        self.next_fake_span(snippet)
    }
    fn unindent(&self) {
        let indent = self.indent.get();
        assert!(indent >= 4);
        self.indent.set(indent - 4);
    }
    #[must_use]
    fn next_fake_span(&self, snippet: &str) -> SpanWithId {
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
        SpanWithId::new(self.file_id, start, end)
    }
}

// impl<'aold, 'iold, T> MyFn<'aold, 'iold> for T
// where
//     T: for<'x> BuildExpr<'aold, 'iold, Expr<'x, 'x> = Expr<'x, 'x>>,
//     T: 'aold + 'iold,
// {
//     fn call<'anew, 'inew>(&self, gen: &'_ ExprGen<'anew, 'inew>) -> Expr<'anew, 'inew> where 'aold: 'anew, 'iold: 'inew, 'aold: 'inew, 'anew: 'inew {
//         self.build_expr(gen)
//     }
// }

#[must_use]
pub struct ExprBuilder<'old> {
    expr: Box<dyn BuildExpr<'old> + 'old>,
}
impl<'old> Display for ExprBuilder<'old> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arena = Arena::new();
        let diagnostics: Diagnostics<crate::ErrorCode> = Diagnostics::new();
        let expr = ExprBuilder::generate(self, &arena, &diagnostics, FileId::new_synthetic_numbered(), "__ExprBuilder::display__".to_string());
        Display::fmt(expr, f)
    }
}
impl<'old> BuildExpr<'old> for ExprBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        self.expr.build_expr(gen)
    }
}
impl<'old> BuildExprPart<'old> for ExprBuilder<'old> {
    type ExprPart<'new> = &'new Expr<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        (self.expr).build_expr(gen)
    }
}

macro_rules! impl_buildexpr_fn {
            (
                pub fn $name:ident($gen:ident $(,$field:ident : $typ:ty)*) => ExprBuilder<'old>
                $expr:block
            ) => {
                pub fn $name($($field : $typ),*) -> ExprBuilder<'old> {
                    impl_buildexpr!(|$gen, $($field : $typ),*| $expr)
                }
            }
        }
macro_rules! impl_buildexpr {
    (
        |$gen:ident, $($field:ident : $typ:ty),*| $expr:expr
    ) => {{
        struct AnonymousExprBuilder<'old> {
            marker: std::marker::PhantomData<&'old ()>,
            $(
                $field: $typ,
            )*
        }
        impl<'old> BuildExprPart<'old> for AnonymousExprBuilder<'old> {
            type ExprPart<'new> = Expr<'new, 'new> where 'old: 'new;
            fn build_expr_part<'new>(&self, $gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
                let AnonymousExprBuilder { marker: _, $($field),* } = self;
                $expr
            }
        }
        impl<'old> BuildExpr<'old> for AnonymousExprBuilder<'old> {
            fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
                gen.arena.alloc(self.build_expr_part(gen))
            }
        }
        ExprBuilder { expr: Box::new(AnonymousExprBuilder { marker: std::marker::PhantomData, $($field),* }) }
    }};
}
impl<'old> ExprBuilder<'old> {
    impl_buildexpr_fn! {
        pub fn from_expr(gen, expr: &'old Expr<'old, 'old>) => ExprBuilder<'old> {
            Expr::Parenthesized(ExprParenthesized::new(
                TokenOpenParen { span: gen.next_fake_span("(") },
                {
                    let _ = gen.next_fake_span(&expr.to_string());
                    expr
                },
                TokenCloseParen { span: gen.next_fake_span(") ") },
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn from_block_with_new_spans(gen, block_body: BlockBody<'old, 'old>) => ExprBuilder<'old> {
            Expr::Block(ExprBlock::new(
                TokenOpenCurly { span: gen.next_fake_span_indent("{\n") },
                block_body.clone(),
                TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn unit(gen) => ExprBuilder<'old> {
            Expr::Literal(ExprLiteral::Unit(ExprUnit::new(
                TokenOpenParen { span: gen.next_fake_span("(") },
                TokenCloseParen { span: gen.next_fake_span(") ") },
            )))
        }
    }
    pub fn literal<T: Into<ExprLiteralBuilder>>(t: T) -> Self {
        let lit = t.into();
        impl_buildexpr!(|gen, lit: ExprLiteralBuilder| Expr::Literal(lit.build_expr_part(gen)))
    }
    pub fn binding_existing(binding: Binding<'_>) -> ExprBuilderBinding<'_> {
        binding.into()
    }
    pub fn binding_new(name: &'_ str, mutable: bool) -> ExprBuilderBinding<'_> {
        ExprBuilderBinding {
            inner: Rc::new(RefCell::new(ExprBuilderBindingInner {
                id: BindingId::unique(),
                new: true,
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
    pub fn assign<'x>(binding: impl Into<ExprBuilderBinding<'x>>) -> ExprAssignBuilder<'x> {
        ExprAssignBuilder { binding: binding.into(), fields: Vec::new() }
    }
    // Assign(ExprAssign<'a, 'i>),
    // unops
    impl_buildexpr_fn! {
        pub fn bool_not(gen, inner: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::BoolNot(ExprBoolNot::new(
                TokenBang { span: gen.next_fake_span("!") },
                inner.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn neg(gen, inner: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Neg(ExprNeg::new(
                TokenMinus { span: gen.next_fake_span("-") },
                inner.build_expr_part(gen),
            ))
        }
    }
    // binops
    impl_buildexpr_fn! {
        pub fn add(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Add(ExprAdd::new(
                a.build_expr_part(gen),
                TokenPlus { span: gen.next_fake_span(" + ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn sub(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Sub(ExprSub::new(
                a.build_expr_part(gen),
                TokenMinus { span: gen.next_fake_span(" - ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn mul(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Mul(ExprMul::new(
                a.build_expr_part(gen),
                TokenStar { span: gen.next_fake_span(" * ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn div(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Div(ExprDiv::new(
                a.build_expr_part(gen),
                TokenSlash { span: gen.next_fake_span(" / ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn mod_(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Mod(ExprMod::new(
                a.build_expr_part(gen),
                TokenPercent { span: gen.next_fake_span(" % ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn xor(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Xor(ExprXor::new(
                a.build_expr_part(gen),
                TokenCircumflex { span: gen.next_fake_span(" ^ ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn bool_and(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::BoolAnd(ExprBoolAnd::new(
                a.build_expr_part(gen),
                TokenDoubleAmp { span: gen.next_fake_span(" && ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn bool_or(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::BoolOr(ExprBoolOr::new(
                a.build_expr_part(gen),
                TokenDoublePipe { span: gen.next_fake_span(" || ") },
                b.build_expr_part(gen),
            ))
        }
    }
    // binop-assign
    // AddAssign(ExprAddAssign<'a, 'i>),
    // SubAssign(ExprSubAssign<'a, 'i>),
    // MulAssign(ExprMulAssign<'a, 'i>),
    // DivAssign(ExprDivAssign<'a, 'i>),
    // ModAssign(ExprModAssign<'a, 'i>),
    // XorAssign(ExprXorAssign<'a, 'i>),
    // BoolAndAssign(ExprBoolAndAssign<'a, 'i>),
    // BoolOrAssign(ExprBoolOrAssign<'a, 'i>),
    // comparison ops
    impl_buildexpr_fn! {
        pub fn less_than(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::LessThan(ExprLessThan::new(
                a.build_expr_part(gen),
                TokenLessThan { span: gen.next_fake_span(" < ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn less_equals(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::LessEquals(ExprLessEquals::new(
                a.build_expr_part(gen),
                TokenLessEquals { span: gen.next_fake_span(" <= ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn equals(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Equals(ExprEquals::new(
                a.build_expr_part(gen),
                TokenEquals { span: gen.next_fake_span(" == ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn not_equals(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::NotEquals(ExprNotEquals::new(
                a.build_expr_part(gen),
                TokenNotEquals { span: gen.next_fake_span(" != ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn greater_equals(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::GreaterEquals(ExprGreaterEquals::new(
                a.build_expr_part(gen),
                TokenGreaterEquals { span: gen.next_fake_span(" >> ") },
                b.build_expr_part(gen),
            ))
        }
    }
    impl_buildexpr_fn! {
        pub fn greater_than(gen, a: ExprBuilder<'old>, b: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::GreaterThan(ExprGreaterThan::new(
                a.build_expr_part(gen),
                TokenGreaterThan { span: gen.next_fake_span(" > ") },
                b.build_expr_part(gen),
            ))
        }
    }
    /// Create a new BlockBuilder, default with terminating semicolon
    pub fn block<'x>() -> ExprBlockBuilder<'x> {
        ExprBlockBuilder { exprs: Vec::new(), terminated_with_semicolon: true }
    }
    pub fn variable<'x>(binding: impl Into<ExprBuilderBinding<'x>>) -> ExprBuilder<'x> {
        let binding = binding.into();
        impl_buildexpr!(|gen, binding: ExprBuilderBinding<'old>| {
            Expr::Variable(ExprBuilder::build_expr_variable(binding.clone(), gen))
        })
    }
    fn build_expr_variable<'new>(binding: impl Into<ExprBuilderBinding<'old>>, gen: &ExprGen<'new>) -> ExprVariable<'new> where 'old: 'new {
        let binding = binding.into();
        ExprVariable {
            binding: binding.build_expr_part(gen),
            span: gen.next_fake_span(binding.name()),
        }
    }
    pub fn access<'x>(binding: impl Into<ExprBuilderBinding<'x>>) -> ExprAccessBuilder<'x> {
        ExprAccessBuilder { binding: binding.into(), accesses: Vec::new() }
    }
    impl_buildexpr_fn! {
        pub fn parenthesized(gen, expr: ExprBuilder<'old>) => ExprBuilder<'old> {
            Expr::Parenthesized(ExprParenthesized::new(
                TokenOpenParen { span: gen.next_fake_span("(") },
                expr.build_expr_part(gen),
                TokenCloseParen { span: gen.next_fake_span(") ") },
            ))
        }
    }
    // IfElse(ExprIfElse<'a, 'i>),
    pub fn match_(expr: ExprBuilder<'old>) -> ExprMatchBuilder<'old> {
        ExprMatchBuilder { expr, arms: Vec::new() }
    }
    pub fn match_pattern_literal<'x, T: Into<ExprLiteralBuilder>>(lit: T) -> ExprMatchPatternBuilder<'x> {
        ExprMatchPatternBuilder::Literal(lit.into())
    }
    // TODO: match pattern enum variant
    pub fn match_pattern_binding<'x>(binding: impl Into<ExprBuilderBinding<'x>>) -> ExprMatchPatternBuilder<'x> {
        ExprMatchPatternBuilder::Binding(binding.into())
    }
    pub fn match_pattern_wildcard<'x>() -> ExprMatchPatternBuilder<'x> {
        ExprMatchPatternBuilder::Wildcard
    }
    // While(ExprWhile<'a, 'i>),
    // For(ExprFor<'a, 'i>),
    pub fn loop_<'x>(label: Option<&'x str>, block: ExprBlockBuilder<'x>) -> ExprBuilder<'x> {
        impl_buildexpr!(|gen, label: Option<&'old str>, block: ExprBlockBuilder<'old>| {
            Expr::Loop(ExprLoop::new(
                label.map(|label| ExprLabelDef::new(
                    ExprLabel::new(
                        TokenApostrophe { span: gen.next_fake_span("'") },
                        TokenIdent {
                            span: gen.next_fake_span(label),
                            ident: label,
                        }
                    ),
                    TokenColon { span: gen.next_fake_span(": ") }
                )),
                TokenLoop { span: gen.next_fake_span("loop ") },
                block.build_expr_part(gen),
            ))
        })
    }
    // Break(ExprBreak<'a, 'i>),
    // Continue(ExprContinue<'i>),
    impl_buildexpr_fn! {
        pub fn return_(gen, expr: Option<ExprBuilder<'old>>) => ExprBuilder<'old> {
            Expr::Return(ExprReturn::new(
                TokenReturn { span: gen.next_fake_span("return ") },
                expr.as_ref().map(|expr| expr.build_expr_part(gen)),
            ))
        }
    }
    // Yield(ExprYield<'a, 'i>),
    pub fn function_call<'x>(binding: impl Into<ExprBuilderBinding<'x>>) -> ExprFunctionCallBuilder<'x> {
        ExprFunctionCallBuilder { binding: binding.into(), args: Vec::new() }
    }
    // TODO: anonymous functions
    pub fn function_definition(name: &'_ str) -> ExprFunctionSignatureSelfArgBuilder<'_> {
        ExprFunctionSignatureSelfArgBuilder { name }
    }
    pub fn struct_definition(name: &'_ str) -> ExprStructDefinitionBuilder<'_> {
        ExprStructDefinitionBuilder { name, generics: Vec::new(), fields: Vec::new() }
    }
    pub fn struct_initialization(name: &'_ str) -> ExprStructInitializationBuilder<'_> {
        ExprStructInitializationBuilder { name, fields: Vec::new() }
    }
    // EnumDefinition(ExprEnumDefinition<'a, 'i>),
    impl_buildexpr_fn! {
        pub fn enum_initialization(gen, enum_name: &'old str, variant_name: &'old str) => ExprBuilder<'old> {
            Expr::EnumInitialization(ExprEnumInitialization::new(
                TokenIdent {
                    span: gen.next_fake_span(enum_name),
                    ident: enum_name,
                },
                TokenDoubleColon { span: gen.next_fake_span("::") },
                TokenIdent {
                    span: gen.next_fake_span(variant_name),
                    ident: variant_name,
                },
            ))
        }
    }
    pub fn impl_block(target: &'_ str) -> ExprImplBlockBuilder<'_> {
        ExprImplBlockBuilder { target, functions: Vec::new() }
    }

    // TYPES
    pub fn from_expr_type(typ: ExprType<'old, 'old>) -> ExprTypeBuilder<'old> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Parenthesized(typ) }
    }
    pub fn string_type<'x>() -> ExprTypeBuilder<'x> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::String }
    }
    pub fn int_type<'x>() -> ExprTypeBuilder<'x> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Int }
    }
    pub fn float_type<'x>() -> ExprTypeBuilder<'x> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Float }
    }
    pub fn bool_type<'x>() -> ExprTypeBuilder<'x> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Bool }
    }
    pub fn unit_type<'x>() -> ExprTypeBuilder<'x> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Unit }
    }
    /// struct or enum type
    pub fn user_type(name: &'_ str) -> ExprUserTypeBuilder<'_> {
        ExprUserTypeBuilder { name, generics: Vec::new() }
    }
    pub fn never_type<'x>() -> ExprTypeBuilder<'x> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Never }
    }
    pub fn any_type<'x>() -> ExprTypeBuilder<'x> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::Any }
    }

    // builder functions

    pub fn generate<'new, B: BuildExprPart<'old>>(
        builder: &B,
        arena: &'new Arena<Expr<'new, 'new>>,
        diagnostics: &Diagnostics<impl ErrorCode>,
        file_id: FileId,
        file_name: String,
    ) -> B::ExprPart<'new>
        where 'old: 'new
    {
        let gen = ExprGen {
            indent: Cell::new(0),
            arena,
            file_id,
            file_name,
            code: RefCell::new(String::new()),
        };
        let expr = builder.build_expr_part(&gen);
        diagnostics.add_synthetic_numbered_file(gen.file_id, gen.file_name, gen.code.into_inner());
        expr
    }
}

#[must_use]
#[derive(Clone)]
pub struct ExprBuilderBinding<'old> {
    // Bindings must always have one single span where they are defined.
    // Thus, we must lazily initialize that def-span only once.
    inner: Rc<RefCell<ExprBuilderBindingInner<'old>>>,
}
struct ExprBuilderBindingInner<'old> {
    id: BindingId,
    /// Whether this Binding-Builder refers to an existing Binding from the outside,
    /// or whether it's a span that shall be newly created / generated.
    // If this is a new binding, we can't just generate the spans once and reuse them forever.
    // `Self::build_expr_part` can be called multiple times from within the same call to
    // `ExprBuilder::generate`, in which case the same span must be reused.
    // However, it can also be called from two different calls to `ExprBuilder::generate`,
    // in which case a new span must be generated the first time it's used within the new call.
    new: bool,
    name: &'old str,
    mutable: bool,
    // a binding can be used multiple times -- create spans only once
    mut_span: Option<SpanWithId>,
    def_span: Option<SpanWithId>,
}
impl<'old> From<Binding<'old>> for ExprBuilderBinding<'old> {
    fn from(binding: Binding<'old>) -> Self {
        ExprBuilderBinding {
            inner: Rc::new(RefCell::new(ExprBuilderBindingInner {
                id: binding.id,
                new: false,
                name: binding.ident.ident,
                mutable: binding.mutable.is_some(),
                mut_span: binding.mutable.map(|mutable| mutable.span),
                def_span: Some(binding.ident.span),
                // mut_span: None,
                // def_span: None,
            })),
        }
    }
}
impl<'x, 'old> From<&'x ExprBuilderBinding<'old>> for ExprBuilderBinding<'old> {
    fn from(value: &'x ExprBuilderBinding<'old>) -> Self {
        value.clone()
    }
}
impl<'old> ExprBuilderBinding<'old> {
    pub fn id(&self) -> BindingId {
        self.inner.borrow().id
    }
    pub fn name(&self) -> &'old str {
        self.inner.borrow().name
    }
    pub fn mutable(&self) -> bool {
        self.inner.borrow().mutable
    }
}
impl<'old> BuildExprPart<'old> for ExprBuilderBinding<'old> {
    type ExprPart<'new> = Binding<'old> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Binding<'old> where 'old: 'new {
        // A binding can be used multiple times -- create spans only the first time.
        // However, if it's used in a different FileId, we need to regenerate it.
        // That can happen if the same binding is used in multiple different generate-calls
        let (needs_def_span, needs_mut_span) = {
            let inner = self.inner.borrow();
            (
                inner.def_span.is_none() || (inner.new && gen.file_id != inner.def_span.unwrap().file_id()),
                inner.mutable && (inner.mut_span.is_none() || (inner.new && gen.file_id != inner.mut_span.unwrap().file_id())),
            )
        };
        if needs_mut_span {
            let mut_span = gen.next_fake_span("mut ");
            self.inner.borrow_mut().mut_span = Some(mut_span);
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
            span: inner.mut_span | inner.def_span.unwrap()
        }
    }
}

#[must_use]
pub struct ExprAssignBuilder<'old> {
    binding: ExprBuilderBinding<'old>,
    fields: Vec<&'old str>,
}
impl<'old> ExprAssignBuilder<'old> {
    pub fn access_field(mut self, name: &'old str) -> ExprAssignBuilder<'old> {
        self.fields.push(name);
        self
    }
    pub fn assign(self, rhs: ExprBuilder<'old>) -> ExprBuilder<'old> {
        let this = self;
        impl_buildexpr!(|gen, this: ExprAssignBuilder<'old>, rhs: ExprBuilder<'old>| Expr::Assign(ExprAssign::new(
            ExprAssignLhs::FieldAccess(ExprFieldAccess::new(
                ExprBuilder::build_expr_variable(this.binding.clone(), gen),
                TokenDot { span: gen.next_fake_span(".") },
                this.fields.iter().map(|name| (
                    TokenDot { span: gen.next_fake_span(".") },
                    TokenIdent { ident: name, span: gen.next_fake_span(name) },
                )).collect(),
            )),
            TokenAssign { span: gen.next_fake_span(" = ") },
            rhs.build_expr(gen),
        )))
    }
}

#[must_use]
pub struct ExprBlockBuilder<'old> {
    exprs: Vec<ExprBuilder<'old>>,
    terminated_with_semicolon: bool,
}
impl<'old> ExprBlockBuilder<'old> {
    pub fn is_empty(&self) -> bool {
        self.exprs.is_empty()
    }
    pub fn insert_expr(&mut self, index: usize, expr: ExprBuilder<'old>) -> &mut Self {
        self.exprs.insert(index, expr);
        self
    }
    pub fn push_expr(&mut self, expr: ExprBuilder<'old>) -> &mut Self {
        self.exprs.push(expr);
        self
    }
    pub fn expr(mut self, expr: ExprBuilder<'old>) -> Self {
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
    pub fn build(self) -> ExprBuilder<'old> {
        ExprBuilder { expr: Box::new(self) }
    }
}
impl<'old> BuildExprPart<'old> for ExprBlockBuilder<'old> {
    type ExprPart<'new> = ExprBlock<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprBlock::new(
            TokenOpenCurly { span: gen.next_fake_span_indent("{\n") },
            BlockBody {
                exprs: {
                    let res = self.exprs.iter().map(|expr| {
                        let res = expr.build_expr_part(gen);
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
            TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        )
    }
}
impl<'old> BuildExpr<'old> for ExprBlockBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        gen.arena.alloc(Expr::Block(self.build_expr_part(gen)))
    }
}

#[must_use]
pub struct ExprAccessBuilder<'old> {
    binding: ExprBuilderBinding<'old>,
    accesses: Vec<ExprBuilderFieldOrMethod<'old>>,
}
enum ExprBuilderFieldOrMethod<'old> {
    Field(&'old str),
    Method(&'old str, Vec<ExprBuilder<'old>>),
}
impl<'old> ExprAccessBuilder<'old> {
    pub fn field(mut self, name: &'old str) -> Self {
        self.accesses.push(ExprBuilderFieldOrMethod::Field(name));
        self
    }
    pub fn call_method(mut self, name: &'old str, args: Vec<ExprBuilder<'old>>) -> Self {
        self.accesses.push(ExprBuilderFieldOrMethod::Method(name, args));
        self
    }
    pub fn build(self) -> ExprBuilder<'old> {
        ExprBuilder { expr: Box::new(self) }
    }
}
impl<'old> BuildExprPart<'old> for ExprAccessBuilder<'old> {
    type ExprPart<'new> = ExprAccess<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprAccess::new(
            ExprBuilder::build_expr_variable(self.binding.clone(), gen),
            TokenDot { span: gen.next_fake_span(".") },
            self.accesses.iter().map(|acc| match acc {
                ExprBuilderFieldOrMethod::Field(name) => (
                    TokenDot { span: gen.next_fake_span(".") },
                    FieldOrMethod::Field(TokenIdent {
                        span: gen.next_fake_span(name),
                        ident: name,
                    }),
                ),
                ExprBuilderFieldOrMethod::Method(name, args) => (
                    TokenDot { span: gen.next_fake_span(".") },
                    FieldOrMethod::Method(ExprMethodCall::new(
                        TokenIdent { ident: name, span: gen.next_fake_span(name) },
                        TokenOpenParen { span: gen.next_fake_span("(") },
                        args.into_iter().map(|arg| (
                            TokenComma { span: gen.next_fake_span(", ") },
                            arg.build_expr(gen),
                        )).collect(),
                        TokenCloseParen { span: gen.next_fake_span(")") },
                    ))
                ),
            }).collect(),
        )
    }
}
impl<'old> BuildExpr<'old> for ExprAccessBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        gen.arena.alloc(Expr::Access(self.build_expr_part(gen)))
    }
}

#[must_use]
pub struct ExprMatchBuilder<'old> {
    expr: ExprBuilder<'old>,
    arms: Vec<(ExprMatchPatternBuilder<'old>, ExprBuilder<'old>)>,
}
impl<'old> ExprMatchBuilder<'old> {
    pub fn push_arm(&mut self, pattern: ExprMatchPatternBuilder<'old>, expr: ExprBuilder<'old>) -> &mut Self {
        self.arms.push((pattern, expr));
        self
    }
    pub fn arm(mut self, pattern: ExprMatchPatternBuilder<'old>, expr: ExprBuilder<'old>) -> Self {
        self.push_arm(pattern, expr);
        self
    }
    pub fn build(self) -> ExprBuilder<'old> {
        ExprBuilder { expr: Box::new(self) }
    }
}
impl<'old> BuildExprPart<'old> for ExprMatchBuilder<'old> {
    type ExprPart<'new> = ExprMatch<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprMatch::new(
            TokenMatch { span: gen.next_fake_span("match ") },
            self.expr.build_expr(gen),
            TokenOpenCurly { span: gen.next_fake_span_indent(" {\n") },
            self.arms.iter().map(|(pattern, expr)| {
                let res = (
                    pattern.build_expr_part(gen),
                    TokenFatArrow { span: gen.next_fake_span(" => ") },
                    expr.build_expr(gen),
                );
                // for readability sake
                let _ = gen.next_fake_span(",\n");
                res
            }).collect(),
            TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        )
    }
}
impl<'old> BuildExpr<'old> for ExprMatchBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        gen.arena.alloc(Expr::Match(self.build_expr_part(gen)))
    }
}

#[must_use]
#[derive(Clone)]
pub enum ExprMatchPatternBuilder<'old> {
    Literal(ExprLiteralBuilder),
    // TODO: match pattern variant
    Variant(ExprMatchPatternVariantBuilder<'old>),
    Binding(ExprBuilderBinding<'old>),
    Wildcard,
}
impl<'old> BuildExprPart<'old> for ExprMatchPatternBuilder<'old> {
    type ExprPart<'new> = ExprMatchPattern<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        match self {
            ExprMatchPatternBuilder::Literal(lit) => ExprMatchPattern::Literal(lit.build_expr_part(gen)),
            ExprMatchPatternBuilder::Variant(_) => todo!("match pattern variant"),
            ExprMatchPatternBuilder::Binding(binding) => ExprMatchPattern::Binding(binding.build_expr_part(gen)),
            ExprMatchPatternBuilder::Wildcard => ExprMatchPattern::Wildcard(TokenUnderscore { span: gen.next_fake_span("_") }),
        }
    }
}
impl<'old> Display for ExprMatchPatternBuilder<'old> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arena = Arena::new();
        let diagnostics: Diagnostics<crate::ErrorCode> = Diagnostics::new();
        let expr = ExprBuilder::generate(self, &arena, &diagnostics, FileId::new_synthetic_numbered(), "__ExprMatchPatternBuilder::display__".to_string());
        Display::fmt(&expr, f)
    }
}

#[must_use]
#[derive(Clone)]
pub struct ExprMatchPatternVariantBuilder<'old> {
    // TODO: match pattern variant
    marker: std::marker::PhantomData<&'old ()>,
}

#[must_use]
pub struct ExprFunctionCallBuilder<'old> {
    binding: ExprBuilderBinding<'old>,
    args: Vec<ExprBuilder<'old>>,
}
impl<'old> ExprFunctionCallBuilder<'old> {
    pub fn arg(mut self, arg: ExprBuilder<'old>) -> ExprFunctionCallBuilder<'old> {
        self.args.push(arg);
        self
    }
    pub fn call(self) -> ExprBuilder<'old> {
        ExprBuilder { expr: Box::new(self) }
    }
}
impl<'old> BuildExprPart<'old> for ExprFunctionCallBuilder<'old> {
    type ExprPart<'new> = ExprFunctionCall<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprFunctionCall::new(
            ExprBuilder::build_expr_variable(self.binding.clone(), gen),
            TokenOpenParen { span: gen.next_fake_span("(") },
            self.args.iter().map(|arg| (
                TokenComma { span: gen.next_fake_span(", ") },
                arg.build_expr(gen),
            )).collect(),
            TokenCloseParen { span: gen.next_fake_span(")") },
        )
    }
}
impl<'old> BuildExpr<'old> for ExprFunctionCallBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        gen.arena.alloc(Expr::FunctionCall(self.build_expr_part(gen)))
    }
}

#[must_use]
pub struct ExprFunctionSignatureSelfArgBuilder<'old> {
    name: &'old str,
}
impl<'old> ExprFunctionSignatureSelfArgBuilder<'old> {
    /// self-arg, if provided, must be the correct type as declared on the surrounding impl-block
    pub fn self_arg(self, binding: impl Into<ExprBuilderBinding<'old>>, typ: ExprTypeBuilder<'old>) -> ExprFunctionSignatureRetTypeBuilder<'old> {
        ExprFunctionSignatureRetTypeBuilder { name: self.name, self_arg: Some((binding.into(), typ)) }
    }
    pub fn no_self_arg(self) -> ExprFunctionSignatureRetTypeBuilder<'old> {
        ExprFunctionSignatureRetTypeBuilder { name: self.name, self_arg: None }
    }
}
#[must_use]
pub struct ExprFunctionSignatureRetTypeBuilder<'old> {
    name: &'old str,
    self_arg: Option<(ExprBuilderBinding<'old>, ExprTypeBuilder<'old>)>,
}
impl<'old> ExprFunctionSignatureRetTypeBuilder<'old> {
    pub fn ret_type(self, ret_type: ExprTypeBuilder<'old>) -> ExprFunctionSignatureBuilder<'old> {
        ExprFunctionSignatureBuilder { name: self.name, self_arg: self.self_arg, ret_type: Some(ret_type), generics: Vec::new(), args: Vec::new() }
    }
    pub fn no_ret_type(self) -> ExprFunctionSignatureBuilder<'old> {
        ExprFunctionSignatureBuilder { name: self.name, self_arg: self.self_arg, ret_type: None, generics: Vec::new(), args: Vec::new() }
    }
}
#[must_use]
pub struct ExprFunctionSignatureBuilder<'old> {
    name: &'old str,
    generics: Vec<&'old str>,
    self_arg: Option<(ExprBuilderBinding<'old>, ExprTypeBuilder<'old>)>,
    /// name, typ
    args: Vec<(ExprBuilderBinding<'old>, ExprTypeBuilder<'old>)>,
    ret_type: Option<ExprTypeBuilder<'old>>,
}
impl<'old> ExprFunctionSignatureBuilder<'old> {
    pub fn push_generic(&mut self, generic: &'old str) -> &mut Self {
        self.generics.push(generic);
        self
    }
    pub fn generic(mut self, generic: &'old str) -> Self {
        self.push_generic(generic);
        self
    }
    pub fn push_arg(&mut self, binding: impl Into<ExprBuilderBinding<'old>>, typ: ExprTypeBuilder<'old>) -> &mut Self {
        self.args.push((binding.into(), typ));
        self
    }
    pub fn arg(mut self, binding: impl Into<ExprBuilderBinding<'old>>, typ: ExprTypeBuilder<'old>) -> Self {
        self.push_arg(binding, typ);
        self
    }
    pub fn body(self, body: ExprBlockBuilder<'old>) -> ExprFunctionDefinitionBuilder<'old> {
        ExprFunctionDefinitionBuilder { sig: self, body, }
    }
}
impl<'old> BuildExprPart<'old> for ExprFunctionSignatureBuilder<'old> {
    type ExprPart<'new> = ExprFunctionSignature<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprFunctionSignature::new(
            None,
            TokenFn { span: gen.next_fake_span("fn ") },
            Some(TokenIdent { ident: self.name, span: gen.next_fake_span(self.name) }),
            gen_expr_generics(gen, &self.generics),
            TokenOpenParen { span: gen.next_fake_span("(") },
            self.self_arg.as_ref().map(|(binding, _)| binding.build_expr_part(gen)),
            (self.self_arg.is_some() && !self.args.is_empty())
                .then(|| TokenComma { span: gen.next_fake_span(", ") }),
            self.self_arg.iter().chain(&self.args).map(|(binding, typ)| (
                TokenComma { span: gen.next_fake_span(", ") },
                ExprPatternTyped::new(
                    ExprPatternUntyped { binding: binding.build_expr_part(gen) },
                    TokenColon { span: gen.next_fake_span(": ") },
                    typ.build_expr_part(gen),
                )
            )).collect(),
            None,
            TokenCloseParen { span: gen.next_fake_span(")") },
            self.ret_type.clone().map(|typ| (
                TokenArrow { span: gen.next_fake_span(" -> ") },
                typ.build_expr_part(gen),
            )),
        )
    }
}
#[must_use]
pub struct ExprFunctionDefinitionBuilder<'old> {
    sig: ExprFunctionSignatureBuilder<'old>,
    body: ExprBlockBuilder<'old>,
}
impl<'old> ExprFunctionDefinitionBuilder<'old> {
    pub fn build(self) -> ExprBuilder<'old> {
        ExprBuilder { expr: Box::new(self) }
    }
}
impl<'old> BuildExprPart<'old> for ExprFunctionDefinitionBuilder<'old> {
    type ExprPart<'new> = ExprFunctionDefinition<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprFunctionDefinition::new(
            self.sig.build_expr_part(gen),
            IndexSet::new(),
            self.body.build_expr_part(gen),
        )
    }
}
impl<'old> BuildExpr<'old> for ExprFunctionDefinitionBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        gen.arena.alloc(Expr::FunctionDefinition(self.build_expr_part(gen)))
    }
}

#[must_use]
pub struct ExprStructDefinitionBuilder<'old> {
    name: &'old str,
    generics: Vec<&'old str>,
    /// name, type
    fields: Vec<(&'old str, ExprTypeBuilder<'old>)>,
}
impl<'old> ExprStructDefinitionBuilder<'old> {
    pub fn push_generic(&mut self, generic: &'old str) -> &mut Self {
        self.generics.push(generic);
        self
    }
    pub fn generic(mut self, generic: &'old str) -> Self {
        self.push_generic(generic);
        self
    }
    pub fn push_field(&mut self, name: &'old str, typ: ExprTypeBuilder<'old>) -> &mut Self {
        self.fields.push((name, typ));
        self
    }
    pub fn field(mut self, name: &'old str, typ: ExprTypeBuilder<'old>) -> Self {
        self.push_field(name, typ);
        self
    }
    pub fn build(self) -> ExprBuilder<'old> {
        ExprBuilder { expr: Box::new(self) }
    }
}
impl<'old> BuildExprPart<'old> for ExprStructDefinitionBuilder<'old> {
    type ExprPart<'new> = ExprStructDefinition<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprStructDefinition::new(
            TokenStruct { span: gen.next_fake_span("struct ") },
            TokenIdent { ident: self.name, span: gen.next_fake_span(self.name) },
            gen_expr_generics(gen, &self.generics),
            TokenOpenCurly { span: gen.next_fake_span_indent(" {\n") },
            self.fields.iter().map(|(field, typ)| (
                TokenComma { span: gen.next_fake_span(",\n") },
                (
                    TokenIdent { ident: field, span: gen.next_fake_span(field) },
                    TokenColon { span: gen.next_fake_span(": ") },
                    typ.clone().build_expr_part(gen),
                )
            )).collect(),
            TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        )
    }
}
impl<'old> BuildExpr<'old> for ExprStructDefinitionBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        gen.arena.alloc(Expr::StructDefinition(self.build_expr_part(gen)))
    }
}

#[must_use]
pub struct ExprStructInitializationBuilder<'old> {
    name: &'old str,
    /// name, value
    fields: Vec<(&'old str, ExprBuilder<'old>)>,
}
impl<'old> ExprStructInitializationBuilder<'old> {
    pub fn push_field(&mut self, name: &'old str, value: ExprBuilder<'old>) -> &mut Self {
        self.fields.push((name, value));
        self
    }
    pub fn field(mut self, name: &'old str, value: ExprBuilder<'old>) -> Self {
        self.push_field(name, value);
        self
    }
    pub fn build(self) -> ExprBuilder<'old> {
        ExprBuilder { expr: Box::new(self) }
    }
}
impl<'old> BuildExprPart<'old> for ExprStructInitializationBuilder<'old> {
    type ExprPart<'new> = ExprStructInitialization<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprStructInitialization::new(
            TokenIdent { ident: self.name, span: gen.next_fake_span(self.name) },
            TokenOpenCurly { span: gen.next_fake_span_indent(" {\n") },
            self.fields.iter().map(|(field, value)| (
                TokenComma { span: gen.next_fake_span(",\n") },
                (
                    TokenIdent { ident: field, span: gen.next_fake_span(field) },
                    TokenColon { span: gen.next_fake_span(": ") },
                    value.build_expr(gen),
                )
            )).collect(),
            TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        )
    }
}
impl<'old> BuildExpr<'old> for ExprStructInitializationBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        gen.arena.alloc(Expr::StructInitialization(self.build_expr_part(gen)))
    }
}

#[derive(Clone)]
#[must_use]
pub struct ExprTypeBuilder<'old> {
    inner: ExprTypeBuilderInner<'old>,
}
#[derive(Clone)]
enum ExprTypeBuilderInner<'old> {
    /// wrap an original ExprType to ensure start and end of any use belong to our file
    Parenthesized(ExprType<'old, 'old>),
    String,
    Int,
    Float,
    Bool,
    Unit,
    // struct, enum, typedef, ...
    /// name, generics
    UserType(&'old str, Vec<ExprTypeBuilder<'old>>),
    // TODO: Generic(Generic<'i>),
    // TODO: Function(Box<ExprFunctionType<'a, 'i>>),
    Never,
    Any,
}
impl<'old> ExprTypeBuilder<'old> {
    fn name(&self) -> &'old str {
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
}
impl<'old> BuildExprPart<'old> for ExprTypeBuilder<'old> {
    type ExprPart<'new> = ExprType<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        match &self.inner {
            ExprTypeBuilderInner::Parenthesized(typ) => ExprType::Parenthesized(ExprTypeParenthesized::new(
                TokenOpenParen { span: gen.next_fake_span("(") },
                Box::new(typ.clone()),
                TokenCloseParen { span: gen.next_fake_span(")") },
            )),
            ExprTypeBuilderInner::String => ExprType::String(TokenStringType { span: gen.next_fake_span("string") }),
            ExprTypeBuilderInner::Int => ExprType::Int(TokenIntType { span: gen.next_fake_span("int") }),
            ExprTypeBuilderInner::Float => ExprType::Float(TokenFloatType { span: gen.next_fake_span("float") }),
            ExprTypeBuilderInner::Bool => ExprType::Bool(TokenBoolType { span: gen.next_fake_span("bool") }),
            ExprTypeBuilderInner::Unit => ExprType::Unit(ExprTypeUnit::new(
                TokenOpenParen { span: gen.next_fake_span("(") },
                TokenCloseParen { span: gen.next_fake_span(")") },
            )),
            ExprTypeBuilderInner::UserType(name, generics) => ExprType::UserType(ExprTypeUserType::new(
                TokenIdent { ident: name, span: gen.next_fake_span(name) },
                Some((
                    TokenLessThan { span: gen.next_fake_span("<") },
                    Box::new(generics.into_iter().map(|generic| (
                        TokenComma { span: gen.next_fake_span(", ") },
                        generic.build_expr_part(gen),
                    )).collect()),
                    TokenGreaterThan { span: gen.next_fake_span(">") },
                ))
            )),
            ExprTypeBuilderInner::Never => ExprType::Never(TokenBang { span: gen.next_fake_span("!") }),
            ExprTypeBuilderInner::Any => ExprType::Any(gen.next_fake_span("\"any\"")),
        }
    }
}
#[must_use]
pub struct ExprUserTypeBuilder<'old> {
    name: &'old str,
    generics: Vec<ExprTypeBuilder<'old>>,
}
impl<'old> ExprUserTypeBuilder<'old> {
    pub fn generic(mut self, typ: ExprTypeBuilder<'old>) -> Self {
        self.generics.push(typ);
        self
    }
    pub fn build(self) -> ExprTypeBuilder<'old> {
        ExprTypeBuilder { inner: ExprTypeBuilderInner::UserType(self.name, self.generics) }
    }
}

#[must_use]
pub struct ExprImplBlockBuilder<'old> {
    target: &'old str,
    // TODO: generics
    functions: Vec<ExprFunctionDefinitionBuilder<'old>>
}
impl<'old> ExprImplBlockBuilder<'old> {
    pub fn push_function(&mut self, function: ExprFunctionDefinitionBuilder<'old>) -> &mut Self {
        if let Some((_, typ)) = &function.sig.self_arg {
            assert_eq!(typ.name(), self.target);
            // TODO: check self-type generics
        }
        self.functions.push(function);
        self
    }
    pub fn function(mut self, function: ExprFunctionDefinitionBuilder<'old>) -> Self {
        self.push_function(function);
        self
    }
    pub fn build(self) -> ExprBuilder<'old> {
        ExprBuilder { expr: Box::new(self) }
    }
}

impl<'old> BuildExprPart<'old> for ExprImplBlockBuilder<'old> {
    type ExprPart<'new> = ExprImplBlock<'new, 'new> where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        ExprImplBlock::new(
            TokenImpl { span: gen.next_fake_span("impl ") },
            TokenIdent { ident: self.target, span: gen.next_fake_span(self.target) },
            // TODO: generics
            None,
            TokenOpenCurly { span: gen.next_fake_span_indent(" {\n") },
            self.functions.iter()
                .map(|fun| fun.build_expr_part(gen))
                .collect(),
            TokenCloseCurly { span: gen.next_fake_span_unindent("\n}") },
        )
    }
}
impl<'old> BuildExpr<'old> for ExprImplBlockBuilder<'old> {
    fn build_expr<'new>(&self, gen: &'_ ExprGen<'new>) -> &'new Expr<'new, 'new> where 'old: 'new {
        gen.arena.alloc(Expr::ImplBlock(self.build_expr_part(gen)))
    }
}


// helpers

fn gen_expr_generics<'old: 'new, 'new>(gen: &ExprGen<'new>, generics: &[&'old str]) -> Option<ExprGenerics<'old, 'old>> {
    if generics.is_empty() {
        return None;
    }
    Some(ExprGenerics::new(
        TokenLessThan { span: gen.next_fake_span("<") },
        Some(generics.into_iter().map(|generic| {
            let ident = TokenIdent { ident: generic, span: gen.next_fake_span(generic) };
            (
                TokenComma { span: gen.next_fake_span(", ") },
                Generic { def_ident: ident, ident },
            )
        }).collect()),
        TokenGreaterThan { span: gen.next_fake_span(">") },
    ))
}

#[derive(Clone)]
pub enum ExprLiteralBuilder {
    Unit,
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
}
impl<'old> BuildExprPart<'old> for ExprLiteralBuilder {
    type ExprPart<'new> = ExprLiteral where 'old: 'new;
    fn build_expr_part<'new>(&self, gen: &ExprGen<'new>) -> Self::ExprPart<'new> where 'old: 'new {
        match self {
            ExprLiteralBuilder::Unit => ExprLiteral::Unit(ExprUnit::new(
                TokenOpenParen { span: gen.next_fake_span("(") },
                TokenCloseParen { span: gen.next_fake_span(")") },
            )),
            &ExprLiteralBuilder::Integer(value) => ExprLiteral::Integer(ExprInteger { int: TokenInteger { span: gen.next_fake_span(&format!("{value:?}")), value, radix: Radix::Dec } }),
            &ExprLiteralBuilder::Float(value) => ExprLiteral::Float(ExprFloat { float: TokenFloat { span: gen.next_fake_span(&format!("{value:?}")), value, radix: Radix::Dec }, }),
            &ExprLiteralBuilder::Bool(value) => ExprLiteral::Bool(ExprBool { b: TokenBool { span: gen.next_fake_span(&format!("{value:?}")), value } }),
            ExprLiteralBuilder::String(string) => ExprLiteral::String(ExprString { string: TokenDqString { span: gen.next_fake_span(&format!("{string:?}")), string: string.clone() } })
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
