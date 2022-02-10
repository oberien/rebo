use std::borrow::Borrow;

use diagnostic::{Diagnostics, Span};
use log::Level;

use graph::Graph;

use crate::common::MetaInfo;
use crate::parser::Expr;

mod graph;
pub mod types;

/// A type variable.
///
/// `a + b` has 3 TypeVars: `a`, `b` and `a + b`.
/// The source-code span is uniquely identifying a TypeVar.
/// For variables, the binding-span (i.e. creation span) is used.
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TypeVar {
    pub span: Span,
}
impl TypeVar {
    pub fn new(span: Span) -> TypeVar {
        TypeVar { span }
    }
}
impl Borrow<Span> for TypeVar {
    fn borrow(&self) -> &Span {
        &self.span
    }
}

pub fn typeck<'a, 'i>(diagnostics: &'i Diagnostics, meta_info: &mut MetaInfo<'a, 'i>, exprs: &[&'a Expr<'a, 'i>]) {
    let mut graph = Graph::create(diagnostics, meta_info, exprs);
    if Level::Trace <= log::max_level() {
        graph.dot();
    }
    graph.solve(meta_info);
    if Level::Trace <= log::max_level() {
        graph.dot();
    }
    graph.check(diagnostics, meta_info);
}
