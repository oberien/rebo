use diagnostic::{Diagnostics, Span};
use crate::common::MetaInfo;
use crate::parser::Expr;

/// A type variable.
///
/// `a + b` has 3 TypeVars: `a`, `b` and `a + b`.
/// The source-code span is uniquely identifying a TypeVar.
/// For variables, the binding-span (i.e. creation span) is used.
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TypeVar {
    span: Span,
}
impl TypeVar {
    pub fn new(span: Span) -> TypeVar {
        TypeVar { span }
    }
}

pub fn typeck(diagnostics: &Diagnostics, meta_info: &mut MetaInfo<'_, '_>, exprs: &[&Expr<'_, '_>]) {

}