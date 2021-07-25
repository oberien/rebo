use std::collections::{HashMap, BTreeMap, VecDeque};

use diagnostic::{Diagnostics, Span};

use crate::error_codes::ErrorCode;
use crate::typeck::{TypeVar, Constraint, ConstraintTyp};
use crate::common::{Type, PreInfo, SpecificType};
use itertools::{Either, Itertools};

pub struct ConstraintSolver<'a, 'i> {
    diagnostics: &'i Diagnostics,
    pre_info: &'a PreInfo<'a, 'i>,
    dependants: HashMap<TypeVar, Vec<(Span, Dependant)>>,
    worklist: VecDeque<WorklistItem>,
}
#[derive(Debug)]
enum Dependant {
    Eq(TypeVar),
    /// field-names, field-type-var
    FieldAccess(Vec<String>, TypeVar),
}
#[derive(Debug)]
struct WorklistItem {
    solved: TypeVar,
    origin: Span,
    typ: Type,
}

impl<'a, 'i> ConstraintSolver<'a, 'i> {
    pub fn new(diagnostics: &'i Diagnostics, pre_info: &'a PreInfo<'a, 'i>) -> Self {
        ConstraintSolver {
            diagnostics,
            pre_info,
            dependants: HashMap::new(),
            worklist: VecDeque::new(),
        }
    }

    pub fn solve(mut self, constraints: Vec<Constraint>) -> BTreeMap<TypeVar, (Span, Type)> {
        for constraint in constraints {
            match constraint {
                Constraint { span, typ: ConstraintTyp::Type(var, typ) } => {
                    trace!("add `{}` = `{}` to worklist", self.diagnostics.resolve_span(var.span), typ);
                    self.worklist.push_back(WorklistItem {solved: var, origin: span, typ })
                },
                Constraint { span, typ: ConstraintTyp::Eq(a, b) } => {
                    self.dependants.entry(a).or_default().push((span, Dependant::Eq(b)));
                    self.dependants.entry(b).or_default().push((span, Dependant::Eq(a)));
                },
                Constraint { span, typ: ConstraintTyp::FieldAccess(struct_var, field_names, field_var) } => {
                    self.dependants.entry(struct_var).or_default().push((span, Dependant::FieldAccess(field_names, field_var)));
                }
            }
        }
        trace!("dependants: {:#?}", self.dependants.iter().map(|(binding, deps)|
            format!("{}: {}", self.diagnostics.resolve_span(binding.span), deps.iter().map(|(_, d)| match d {
                Dependant::FieldAccess(_, var) => self.diagnostics.resolve_span(var.span),
                Dependant::Eq(var) => self.diagnostics.resolve_span(var.span),
            }).join(", "))
        ).collect::<Vec<_>>());

        let mut solved: BTreeMap<_, (Span, Type)> = BTreeMap::new();

        while let Some(WorklistItem { solved: var, origin, typ }) = self.worklist.pop_front() {
            trace!("applying `{}` = `{}` (worklist: [{}])", self.diagnostics.resolve_span(var.span), typ, self.worklist.iter()
                .map(|wi| format!("`{}` = {}", self.diagnostics.resolve_span(wi.solved.span), wi.typ))
                .join(", ")
            );
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

            if !typ.is_specific() {
                continue;
            }
            'dep: for (span, dependant) in self.dependants.remove(&var).into_iter().flatten() {
                match dependant {
                    Dependant::Eq(dependant) => {
                        trace!("add `{}` = `{}`", self.diagnostics.resolve_span(dependant.span), typ);
                        self.worklist.push_back(WorklistItem { solved: dependant, origin: span, typ: typ.clone() })
                    },
                    Dependant::FieldAccess(field_names, field_var) => {
                        let mut typ = typ.clone();
                        for field_name in field_names {
                            // resolve struct as we have a field access
                            let struct_name = match &typ {
                                Type::Specific(SpecificType::Struct(name)) => name,
                                _ => {
                                    self.diagnostics.error(ErrorCode::NonStructFieldAccess)
                                        .with_error_label(span, format!("`{}` is of type `{}`, which is not a struct", self.diagnostics.resolve_span(var.span), typ))
                                        .emit();
                                    continue 'dep;
                                }
                            };

                            // resolve accessed field
                            let struct_typ = &self.pre_info.structs[struct_name.as_str()].0;
                            let field_typ = struct_typ.fields.iter()
                                .filter(|(name, _typ)| *name == field_name)
                                .map(|(_name, typ)| Type::Specific(typ.clone()))
                                .next();
                            typ = match field_typ {
                                Some(typ) => typ,
                                None => {
                                    self.diagnostics.error(ErrorCode::UnknownFieldAccess)
                                        .with_error_label(span, format!("tried to access non-existent field `{}` of `struct {}`", field_name, struct_name))
                                        .emit();
                                    continue 'dep;
                                }
                            };
                        }
                        trace!("got `{}` = `{}`", self.diagnostics.resolve_span(field_var.span), typ);
                        self.worklist.push_back(WorklistItem {
                            solved: field_var,
                            origin: span,
                            typ,
                        })
                    }
                }
            }
        }

        // check that no dependents are left
        for (binding, deps) in self.dependants {
            let mut diag = self.diagnostics.error(ErrorCode::UnableToInferType)
                .with_error_label(binding.span, "can't infer type for this binding");
            for (_span, dependant) in deps {
                let var = match dependant {
                    Dependant::FieldAccess(_, field_type_var) => field_type_var,
                    Dependant::Eq(var) => var,
                };
                diag = diag.with_info_label(var.span, "thus: can't infer type for this expression");
            }
            diag.emit();
        }
        solved
    }
}
