use std::collections::{HashMap, BTreeMap, VecDeque};

use diagnostic::{Diagnostics, Span};

use crate::error_codes::ErrorCode;
use crate::typeck::{TypeVar, Constraint, ConstraintTyp};
use crate::common::Type;
use itertools::Either;

pub struct ConstraintSolver<'i> {
    diagnostics: &'i Diagnostics,
    dependents: HashMap<TypeVar, Vec<(Span, TypeVar)>>,
    worklist: VecDeque<WorklistItem>,
}
struct WorklistItem {
    solved: TypeVar,
    origin: Span,
    typ: Type,
}

impl<'i> ConstraintSolver<'i> {
    pub fn new(diagnostics: &'i Diagnostics) -> Self {
        ConstraintSolver {
            diagnostics,
            dependents: HashMap::new(),
            worklist: VecDeque::new(),
        }
    }

    pub fn solve(mut self, constraints: Vec<Constraint>) -> BTreeMap<TypeVar, (Span, Type)> {
        for constraint in constraints {
            match constraint {
                Constraint { span, typ: ConstraintTyp::Type(var, typ) } => self.worklist.push_back(WorklistItem {solved: var, origin: span, typ }),
                Constraint { span, typ: ConstraintTyp::Eq(a, b) } => {
                    self.dependents.entry(a).or_default().push((span, b));
                    self.dependents.entry(b).or_default().push((span, a));
                },
            }
        }

        let mut solved: BTreeMap<_, (Span, Type)> = BTreeMap::new();

        while let Some(WorklistItem { solved: var, origin, typ }) = self.worklist.pop_front() {
            match solved.get(&var) {
                Some((span_before, typ_before)) => match typ_before.try_unify(&typ) {
                    Ok(Either::Left(_)) => (),
                    Ok(Either::Right(t)) => { solved.insert(var, (origin, t.clone())); },
                    Err(()) => {
                        let variable = self.diagnostics.resolve_span(var.span);
                        self.diagnostics.error(ErrorCode::TypeConflict)
                            .with_info_label(*span_before, format!("inferred type for `{}` is `{}`", variable, typ_before))
                            .with_error_label(origin, format!("tried to assign type `{}`", typ))
                            .emit()
                    },
                }
                _ => { solved.insert(var, (origin, typ.clone())); },
            }

            if typ.is_specific() {
                for (span, dependent) in self.dependents.remove(&var).into_iter().flatten() {
                    self.worklist.push_back(WorklistItem { solved: dependent, origin: span, typ: typ.clone() });
                }
            }
        }

        // check that no dependents are left
        for (binding, deps) in self.dependents {
            let mut diag = self.diagnostics.error(ErrorCode::UnableToInferType)
                .with_error_label(binding.span, "can't infer type for this binding");
            for (_span, var) in deps {
                diag = diag.with_info_label(var.span, "cause: can't infer type for this expression");
            }
            diag.emit();
        }
        solved
    }
}
