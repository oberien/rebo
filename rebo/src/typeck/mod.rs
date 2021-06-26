use std::collections::HashMap;

use diagnostic::{Diagnostics, Span};

use crate::parser::{Expr, Binding};
use crate::common::{Type, SpecificType, PreTypeInfo};

mod constraints;
mod solver;
mod checker;

use constraints::ConstraintCreator;
use solver::ConstraintSolver;
use checker::Checker;

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

pub enum Constraint {
    Type(TypeVar, Type),
    Eq(TypeVar, TypeVar),
}

pub struct Typechecker<'i> {
    diagnostics: &'i Diagnostics,
    pre_info: &'i mut PreTypeInfo<'i>,
}

impl<'i> Typechecker<'i> {
    pub fn new(diagnostics: &'i Diagnostics, pre_info: &'i mut PreTypeInfo<'i>) -> Typechecker<'i> {
        Typechecker {
            diagnostics,
            pre_info,
        }
    }

    // Step 1: Create constraint set and collect function types
    // Step 2: Solve constraint set and print unification errors
    // Step 3: Check resolved types
    pub fn typeck(&mut self, exprs: &Vec<&Expr<'_, 'i>>) {
        let cc = ConstraintCreator::new(self.pre_info);
        let (constraints, restrictions) = cc.get_constraints(exprs);
        let stringified = constraints.iter()
            .map(|c| match c {
                Constraint::Type(var, typ) => format!("`{}` = {}", self.diagnostics.resolve_span(var.span), typ),
                Constraint::Eq(a, b) => format!("`{}` = `{}`", self.diagnostics.resolve_span(a.span), self.diagnostics.resolve_span(b.span)),
            }).collect::<Vec<_>>();
        trace!("got constraints: {:#?}", stringified);

        let cs = ConstraintSolver::new(&self.diagnostics);
        let solved = cs.solve(constraints);
        let stringified = solved.iter()
            .map(|(var, typ)| format!("`{}`: {}", self.diagnostics.resolve_span(var.span), typ))
            .collect::<Vec<_>>();
        trace!("got solve: {:#?}", stringified);

        let c = Checker::new(&self.diagnostics, solved, restrictions);
        c.check();
    }
}

