use std::collections::{HashMap, BTreeMap, VecDeque};

use diagnostic::Diagnostics;

use crate::error_codes::ErrorCode;
use crate::typeck::{TypeVar, Constraint};
use crate::common::Type;
use itertools::Either;

pub struct ConstraintSolver<'i> {
    diagnostics: &'i Diagnostics,
    dependents: HashMap<TypeVar, Vec<TypeVar>>,
    worklist: VecDeque<(TypeVar, Type)>,
}

impl<'i> ConstraintSolver<'i> {
    pub fn new(diagnostics: &'i Diagnostics) -> Self {
        ConstraintSolver {
            diagnostics,
            dependents: HashMap::new(),
            worklist: VecDeque::new(),
        }
    }

    pub fn solve(mut self, constraints: Vec<Constraint>) -> BTreeMap<TypeVar, Type> {
        for constraint in constraints {
            match constraint {
                Constraint::Type(var, typ) => self.worklist.push_back((var, typ)),
                Constraint::Eq(a, b) => {
                    self.dependents.entry(a).or_default().push(b);
                    self.dependents.entry(b).or_default().push(a);
                },
            }
        }

        let mut solved: BTreeMap<_, Type> = BTreeMap::new();

        while let Some((var, typ)) = self.worklist.pop_front() {
            match solved.get(&var) {
                Some(typ_before) => match typ_before.try_unify(&typ) {
                    Ok(Either::Left(_)) => (),
                    Ok(Either::Right(t)) => { solved.insert(var, t.clone()); },
                    Err(()) => self.diagnostics.error(ErrorCode::TypeConflict)
                        .with_info_label(var.span, format!("inferred type is `{}`", typ_before))
                        .with_error_label(var.span, format!("tried to assign type `{}`", typ))
                        .emit(),
                }
                _ => { solved.insert(var, typ.clone()); },
            }

            if typ.is_specific() {
                for dependent in self.dependents.remove(&var).into_iter().flatten() {
                    self.worklist.push_back((dependent, typ.clone()));
                }
            }
        }

        // check that no dependents are left
        for (binding, deps) in self.dependents {
            let mut diag = self.diagnostics.error(ErrorCode::UnableToInferType)
                .with_error_label(binding.span, "can't infer type for this binding");
            for var in deps {
                diag = diag.with_info_label(var.span, "cause: can't infer type for this expression");
            }
        }
        solved
    }
}
