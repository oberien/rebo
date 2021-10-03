use std::borrow::Borrow;

use diagnostic::{Diagnostics, Span};
use log::Level;

use crate::common::MetaInfo;
use crate::parser::Expr;

mod graph;
mod create_graph;
mod solver;
mod checker;
pub mod types;

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
impl Borrow<Span> for TypeVar {
    fn borrow(&self) -> &Span {
        &self.span
    }
}

pub fn typeck(diagnostics: &Diagnostics, meta_info: &mut MetaInfo<'_, '_>, exprs: &[&Expr<'_, '_>]) {
    let mut graph = create_graph::create_graph(diagnostics, meta_info, exprs);
    if Level::Trace <= log::max_level() {
        graph.dot(diagnostics);
    }
    solver::solve(&mut graph, meta_info);
    if Level::Trace <= log::max_level() {
        graph.dot(diagnostics);
    }
    checker::check(diagnostics, &graph, meta_info);
}
