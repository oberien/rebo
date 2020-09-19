use std::collections::{HashMap, VecDeque};

use crate::diagnostics::{Diagnostics, Span, ErrorCode};
use crate::typeck::{BindingTypes, Type, FunctionType};
use crate::typeck::constraints::Constraint;
use crate::parser::Binding;

enum Dependency<'i> {
    Binding(Binding<'i>, Span),
    RetOf(Binding<'i>, Span),
}

pub struct ConstraintSolver<'a, 'i> {
    diagnostics: &'i Diagnostics<'i>,
    binding_types: &'a mut BindingTypes<'i>,
    dependents: HashMap<Binding<'i>, Vec<Dependency<'i>>>,
    worklist: VecDeque<Binding<'i>>,
}

impl<'a, 'i> ConstraintSolver<'a, 'i> {
    pub fn new(diagnostics: &'i Diagnostics<'i>, binding_types: &'a mut BindingTypes<'i>) -> Self {
        ConstraintSolver {
            diagnostics,
            binding_types,
            dependents: HashMap::new(),
            worklist: VecDeque::new(),
        }
    }

    fn add_dependent(&mut self, binding: Binding<'i>, dependency: Dependency<'i>) {
        self.dependents.entry(binding).or_default().push(dependency)
    }
    fn set_type(&mut self, binding: Binding<'i>, typ: Type, span: Span) {
        match self.binding_types.get(binding) {
            Some((typ_before, span_before)) if *typ_before != typ => {
                self.diagnostics.error(ErrorCode::TypeConflict)
                    .with_info_label(*span_before, format!("inferred type is `{}`", typ_before))
                    .with_error_label(span, format!("tried to assign type `{}`", typ))
                    .emit()
            },
            Some(_) => (),
            None => self.binding_types.insert(binding, typ, span),
        }
    }

    pub fn solve(mut self, constraints: Vec<Constraint<'i>>) {
        for binding in self.binding_types.types.keys().copied() {
            self.worklist.push_back(binding);
        }
        for constraint in constraints {
            match constraint {
                Constraint::Type(binding, typ, span) => {
                    self.worklist.push_back(binding);
                    self.set_type(binding, typ, span);
                },
                Constraint::Eq(a, b, span) => {
                    self.add_dependent(a, Dependency::Binding(b, span));
                    self.add_dependent(b, Dependency::Binding(a, span));
                },
                Constraint::RetOf(a, fun, span) => {
                    self.add_dependent(fun, Dependency::RetOf(a, span));
                }
            }
        }

        while let Some(workitem) = self.worklist.pop_front() {
            self.apply_backwards(workitem);
        }
        // check that everything is solved
        for (binding, deps) in self.dependents {
            let mut diag = self.diagnostics.error(ErrorCode::UnableToInferType)
                .with_error_label(binding.span, "can't infer type for this binding");
            for dep in deps {
                let span = match dep {
                    Dependency::Binding(_, span) => span,
                    Dependency::RetOf(_, span) => span,
                };
                diag = diag.with_info_label(span, "cause: can't infer type for this expression");
            }
        }
    }

    /// Applies a known type to all of its dependents
    fn apply_backwards(&mut self, binding: Binding<'i>) {
        let (typ, typ_span) = self.binding_types.get(binding).unwrap().clone();
        let deps = match self.dependents.remove(&binding) {
            Some(deps) => deps,
            None => return,
        };
        let ret_type = match &typ {
            Type::Function(f) => Some(f.ret.clone()),
            _ => None,
        };
        for dep in deps {
            match dep {
                Dependency::Binding(b, span) => {
                    self.worklist.push_back(b);
                    self.set_type(b, typ.clone(), span);
                },
                Dependency::RetOf(b, span) => {
                    self.worklist.push_back(b);
                    self.set_type(b, ret_type.clone().unwrap(), span);
                },
            }
        }
    }
}
