use std::fmt::{Display, Formatter};
use rebo::common::SpanWithId;
use crate::common::Spanned;

// `let x = a + b` has 14 unique SpanIds:
// * ExprBinding-span
// * `x`:
//    * Binding-span
//    * Binding-TokenIdent-span
// * `=`: TokenAssign-span
// * `a + b`:
//     * ExprAdd-span
//     * `a`
//        * ExprVariable-span
//        * TokenIdent-span
//        * Binding-span
//        * Binding-TokenIdent-span
//     * `+`: TokenPlus-span
//     * `a`
//        * ExprVariable-span
//        * TokenIdent-span
//        * Binding-span
//        * Binding-TokenIdent-span
//
// But it only creates 5 new TypeVars:
// * ExprBinding-span
// * `x` Binding-span
// * `a + b` ExprAdd-span
// * `a` ExprVariable-span (Binding-span-TypeVar was already created in the `let a = ...` expr)
// * `b` ExprVariable-span (Binding-span-TypeVar was already created in the `let b = ...` expr)

/// A type variable.
///
/// `a + b` has 3 TypeVars: `a`, `b` and `a + b`.
/// The source-code span-id is uniquely identifying a TypeVar.
/// For variables, the binding-span-id (i.e. creation span) is used.
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TypeVar {
    span_with_id: SpanWithId,
}
impl TypeVar {
    pub fn from_spanned(spanned: impl Spanned) -> TypeVar {
        TypeVar { span_with_id: spanned.span_with_id() }
    }
}
impl Display for TypeVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let span = self.span_with_id.diagnostics_span();
        write!(f, "{}<{}:{}-{}>", self.span_with_id.id(), span.file, span.start, span.end)
    }
}
impl Spanned for TypeVar {
    fn span_with_id(&self) -> SpanWithId {
        self.span_with_id
    }
}
