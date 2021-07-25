use diagnostic::{Diagnostics, Span};

use crate::parser::Expr;
use crate::common::{Type, PreInfo};

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

pub struct Constraint {
    pub span: Span,
    pub typ: ConstraintTyp,
}
pub enum ConstraintTyp {
    Type(TypeVar, Type),
    Eq(TypeVar, TypeVar),
    /// initial-struct-type-var, field-names, field-type-var
    FieldAccess(TypeVar, Vec<String>, TypeVar),
}
impl Constraint {
    pub fn new(span: Span, typ: ConstraintTyp) -> Constraint {
        Constraint { span, typ }
    }
}

pub struct Typechecker<'a, 'b, 'i> {
    diagnostics: &'b Diagnostics,
    pre_info: &'b mut PreInfo<'a, 'i>,
}

impl<'a, 'b, 'i> Typechecker<'a, 'b, 'i> {
    pub fn new(diagnostics: &'b Diagnostics, pre_info: &'b mut PreInfo<'a, 'i>) -> Typechecker<'a, 'b, 'i> {
        Typechecker {
            diagnostics,
            pre_info,
        }
    }

    // Step 1: Create constraint set
    // Step 2: Solve constraint set and print unification errors
    // Step 3: Check resolved types
    pub fn typeck(&mut self, exprs: &[&Expr<'a, 'i>]) {
        let cc = ConstraintCreator::new(self.diagnostics, self.pre_info);
        let (constraints, restrictions) = cc.get_constraints(exprs);
        let stringified = constraints.iter()
            .map(|c| match c {
                Constraint { typ: ConstraintTyp::Type(var, typ), .. } => format!("`{}` = {}", self.diagnostics.resolve_span(var.span), typ),
                Constraint { typ: ConstraintTyp::Eq(a, b), .. } => format!("`{}` = `{}`", self.diagnostics.resolve_span(a.span), self.diagnostics.resolve_span(b.span)),
                Constraint { typ: ConstraintTyp::FieldAccess(variable, _fields, field_type_var), .. } => format!("`{}` -> `{}`", self.diagnostics.resolve_span(variable.span), self.diagnostics.resolve_span(field_type_var.span)),
            }).collect::<Vec<_>>();
        debug!("got constraints: {:#?}", stringified);

        let cs = ConstraintSolver::new(self.diagnostics, self.pre_info);
        let solved = cs.solve(constraints);
        let stringified = solved.iter()
            .map(|(var, (_span, typ))| format!("`{}`: {}", self.diagnostics.resolve_span(var.span), typ))
            .collect::<Vec<_>>();
        debug!("got solve: {:#?}", stringified);

        let c = Checker::new(&self.diagnostics, solved, restrictions);
        c.check();
    }
}

