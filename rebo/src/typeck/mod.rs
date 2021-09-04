use diagnostic::{Diagnostics, Span};

use crate::parser::Expr;
use crate::common::{Type, PreInfo, UnificationError};

mod constraints;
mod solver;
mod checker;

use constraints::ConstraintCreator;
use solver::ConstraintSolver;
use checker::Checker;
use std::collections::BTreeMap;
use petgraph::graphmap::GraphMap;
use itertools::Either;
use petgraph::Directed;

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
    /// left ⊆ right ∨ right ⊆ left, i.e. left and right are equal or one is a subtype of the other
    Similar(TypeVar, TypeVar),
    /// left ⊆ right, i.e. left is a subtype of or equal to right
    SubEq(TypeVar, TypeVar),
    /// initial-struct-type-var, field-names, field-type-var
    FieldAccess(TypeVar, Vec<String>, TypeVar),
}
impl Constraint {
    pub fn new(span: Span, typ: ConstraintTyp) -> Constraint {
        Constraint { span, typ }
    }
}

enum MyConstraint {
    Eq,
    SubEq,
    FieldAccess(Vec<String>),
}

pub struct Constraints<'b> {
    diagnostics: &'b Diagnostics,
    types: BTreeMap<TypeVar, (Span, Type)>,
    graph: GraphMap<TypeVar, (Span, MyConstraint), Directed>,
}

impl<'b> Constraints<'b> {
    pub fn new(diagnostics: &'b Diagnostics) -> Self {
        Constraints {
            diagnostics,
            types: BTreeMap::new(),
            graph: GraphMap::new(),
        }
    }
    fn add_inital_type(&mut self, var: TypeVar, origin: Span, typ: Type) {
        match self.types.insert(var, (origin, typ)) {
            Some((origin2, typ2)) => unreachable!("inserted initial type twice for {:?} ({:?}, {:?}; previously {:?}, {:?})", var, origin, typ, origin2, typ2),
            None => (),
        }
    }
    fn add_constraint(&mut self, from: TypeVar, to: TypeVar, origin: Span, constraint: MyConstraint) {
        self.graph.add_edge(from, to, (origin, constraint));
    }
}

pub struct Typechecker<'a, 'b, 'i> {
    diagnostics: &'b Diagnostics,
    meta_info: &'b mut PreInfo<'a, 'i>,
}

impl<'a, 'b, 'i> Typechecker<'a, 'b, 'i> {
    pub fn new(diagnostics: &'b Diagnostics, meta_info: &'b mut PreInfo<'a, 'i>) -> Typechecker<'a, 'b, 'i> {
        Typechecker {
            diagnostics,
            meta_info,
        }
    }

    // Step 1: Create constraint set
    // Step 2: Solve constraint set and print unification errors
    // Step 3: Check resolved types
    pub fn typeck(&mut self, exprs: &[&Expr<'a, 'i>]) {
        let cc = ConstraintCreator::new(self.diagnostics, self.meta_info);
        let (constraints, restrictions) = cc.get_constraints(exprs);
        let stringified = constraints.iter()
            .map(|c| match c {
                Constraint { typ: ConstraintTyp::Type(var, typ), .. } => format!("`{}` = {}", self.diagnostics.resolve_span(var.span), typ),
                Constraint { typ: ConstraintTyp::Similar(a, b), .. } => format!("`{}` ≈ `{}`", self.diagnostics.resolve_span(a.span), self.diagnostics.resolve_span(b.span)),
                Constraint { typ: ConstraintTyp::SubEq(a, b), .. } => format!("`{}` ⊆ `{}`", self.diagnostics.resolve_span(a.span), self.diagnostics.resolve_span(b.span)),
                Constraint { typ: ConstraintTyp::FieldAccess(variable, _fields, field_type_var), .. } => format!("`{}` -> `{}`", self.diagnostics.resolve_span(variable.span), self.diagnostics.resolve_span(field_type_var.span)),
            }).collect::<Vec<_>>();
        debug!("got constraints: {:#?}", stringified);

        let cs = ConstraintSolver::new(self.diagnostics, self.meta_info);
        let solved = cs.solve(constraints);
        let stringified = solved.iter()
            .map(|(var, (_span, typ))| format!("`{}`: {}", self.diagnostics.resolve_span(var.span), typ))
            .collect::<Vec<_>>();
        debug!("got solve: {:#?}", stringified);

        let c = Checker::new(&self.diagnostics, solved, restrictions);
        c.check(exprs);
    }
}
