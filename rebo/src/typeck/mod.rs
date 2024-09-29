use diagnostic::Diagnostics;
use log::Level;

use graph::Graph;

use crate::common::MetaInfo;
use crate::parser::Expr;
use crate::ErrorCode;

mod graph;
mod type_var;
pub mod types;

pub use type_var::TypeVar;

pub fn typeck<'i>(diagnostics: &'i Diagnostics<ErrorCode>, meta_info: &mut MetaInfo<'i>, exprs: &[&'i Expr<'i>]) -> (String, String) {
    let mut graph = Graph::create(diagnostics, meta_info, exprs);
    if log_enabled!(Level::Trace) {
        graph.xdot();
    }
    let before = graph.to_string();
    graph.solve(meta_info);
    let after = graph.to_string();
    if log_enabled!(Level::Trace) {
        graph.xdot();
    }
    graph.check(diagnostics, meta_info);
    (before, after)
}
