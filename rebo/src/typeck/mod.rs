use std::collections::HashMap;

use crate::diagnostics::{Diagnostics, Span};
use crate::parser::{Expr, Binding};
use crate::common::Type;

mod constraints;
mod solver;
mod checker;

use constraints::ConstraintCreator;
use solver::ConstraintSolver;
use checker::Checker;

#[derive(Debug, Clone)]
pub struct BindingTypes<'i> {
    pub types: HashMap<Binding<'i>, (Type, Span)>,
}

impl<'i> BindingTypes<'i> {
    pub fn new() -> BindingTypes<'i> {
        BindingTypes {
            types: HashMap::new(),
        }
    }

    pub fn get(&self, binding: Binding<'i>) -> Option<&(Type, Span)> {
        self.types.get(&binding)
    }
    pub fn insert(&mut self, binding: Binding<'i>, typ: Type, span: Span) {
        self.types.insert(binding, (typ, span));
    }
}

pub struct Typechecker<'i> {
    diagnostics: &'i Diagnostics,
    binding_types: &'i mut BindingTypes<'i>,
}

impl<'i> Typechecker<'i> {
    pub fn new(diagnostics: &'i Diagnostics, binding_types: &'i mut BindingTypes<'i>) -> Typechecker<'i> {
        Typechecker {
            diagnostics,
            binding_types,
        }
    }

    // Step 1: Create constraint set and collect function types
    // Step 2: Solve constraint set and print unification errors
    // Step 3: Check math operations and function calls, printing type errors
    pub fn typeck(&mut self, exprs: &Vec<&Expr<'_, 'i>>) {
        let cc = ConstraintCreator::new(self.binding_types);
        let constraints = cc.get_constraints(exprs);
        trace!("got constraints: {:#?}", constraints.iter().map(ToString::to_string).collect::<Vec<_>>());
        let cs = ConstraintSolver::new(&self.diagnostics, self.binding_types);
        cs.solve(constraints);
        let c = Checker::new(&self.diagnostics, self.binding_types);
        c.check(exprs);
    }
}

