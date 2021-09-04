use std::collections::{HashMap, BTreeMap, VecDeque};

use diagnostic::{Diagnostics, Span};

use crate::error_codes::ErrorCode;
use crate::typeck::{TypeVar, Constraint, ConstraintTyp};
use crate::common::{Type, PreInfo, SpecificType, UnificationError};
use itertools::{Either, Itertools};

pub struct ConstraintSolver<'a, 'i> {
    diagnostics: &'i Diagnostics,
    meta_info: &'a PreInfo<'a, 'i>,
    dependants: HashMap<TypeVar, Vec<(Span, Dependant)>>,
    worklist: VecDeque<WorklistItem>,
    solved: BTreeMap<TypeVar, (Span, Type)>,
}
#[derive(Debug)]
enum Dependant {
    /// self ≈ dependant
    Similar(TypeVar),
    /// self ⊆ dependant
    Sub(TypeVar),
    /// self ⊇ dependant
    Super(TypeVar),
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
    pub fn new(diagnostics: &'i Diagnostics, meta_info: &'a PreInfo<'a, 'i>) -> Self {
        ConstraintSolver {
            diagnostics,
            meta_info,
            dependants: HashMap::new(),
            worklist: VecDeque::new(),
            solved: BTreeMap::new(),
        }
    }

    pub fn solve(mut self, constraints: Vec<Constraint>) -> BTreeMap<TypeVar, (Span, Type)> {
        for constraint in constraints {
            match constraint {
                Constraint { span, typ: ConstraintTyp::Type(var, typ) } => {
                    trace!("add `{}` = `{}` to worklist", self.diagnostics.resolve_span(var.span), typ);
                    self.add_solved(var, span, typ.clone(), Type::try_unify_subeq);
                    self.worklist.push_back(WorklistItem {solved: var, origin: span, typ });
                },
                Constraint { span, typ: ConstraintTyp::Similar(a, b) } => {
                    self.dependants.entry(a).or_default().push((span, Dependant::Similar(b)));
                    self.dependants.entry(b).or_default().push((span, Dependant::Similar(a)));
                },
                Constraint { span, typ: ConstraintTyp::SubEq(a, b) } => {
                    self.dependants.entry(a).or_default().push((span, Dependant::Sub(b)));
                    self.dependants.entry(b).or_default().push((span, Dependant::Super(a)));
                },
                Constraint { span, typ: ConstraintTyp::FieldAccess(struct_var, field_names, field_var) } => {
                    self.dependants.entry(struct_var).or_default().push((span, Dependant::FieldAccess(field_names, field_var)));
                }
            }
        }
        trace!("dependants: {:#?}", self.dependants.iter().map(|(binding, deps)|
            format!("{}: {}", self.diagnostics.resolve_span(binding.span), deps.iter().map(|(_, d)| match d {
                Dependant::FieldAccess(_, var) => format!(".{}", self.diagnostics.resolve_span(var.span)),
                Dependant::Similar(var) => format!("≈{}", self.diagnostics.resolve_span(var.span)),
                Dependant::Sub(var) => format!("⊆{}", self.diagnostics.resolve_span(var.span)),
                Dependant::Super(var) => format!("⊇{}", self.diagnostics.resolve_span(var.span)),
            }).join(", "))
        ).collect::<Vec<_>>());


        while let Some(WorklistItem { solved: var, origin, typ }) = self.worklist.pop_front() {
            trace!("applying `{}` = `{}`", self.diagnostics.resolve_span(var.span), typ);
            // trace!("applying `{}` = `{}` (worklist: [{}])", self.diagnostics.resolve_span(var.span), typ, self.worklist.iter()
            //     .map(|wi| format!("`{}` = {}", self.diagnostics.resolve_span(wi.solved.span), wi.typ))
            //     .join(", ")
            // );

            if !typ.is_specific() && typ != Type::Bottom {
                continue;
            }
            'dep: for (span, dependant) in self.dependants.remove(&var).into_iter().flatten() {
                let (type_var, typ, unification_function): (_, _, for<'x, 'y> fn(&'x Type, &'y Type) -> _) = match dependant {
                    Dependant::Similar(dependant) => {
                        trace!("add `{}` ≈ `{}`", self.diagnostics.resolve_span(dependant.span), typ);
                        (dependant, typ.clone(), Type::try_unify_similar)
                    },
                    Dependant::Sub(dependant) => {
                        trace!("add `{}` ⊇ `{}`", self.diagnostics.resolve_span(dependant.span), typ);
                        (dependant, typ.clone(), Type::try_unify_subeq)
                    },
                    Dependant::Super(dependant) => {
                        trace!("add `{}` ⊆ `{}`", self.diagnostics.resolve_span(dependant.span), typ);
                        (dependant, typ.clone(), Type::try_unify_supereq)
                    },
                    Dependant::FieldAccess(field_names, field_var) => {
                        let mut typ = typ.clone();
                        for field_name in field_names {
                            // resolve struct as we have a field access
                            let struct_name = match &typ {
                                Type::Specific(SpecificType::Struct(_mut, name)) => name,
                                _ => continue 'dep,
                            };

                            // resolve accessed field
                            let struct_typ = &self.meta_info.structs[struct_name.as_str()].0;
                            let field_typ = struct_typ.fields.iter()
                                .filter(|(name, _typ)| *name == field_name)
                                .map(|(_name, typ)| Type::Specific(typ.clone()))
                                .next();
                            typ = match field_typ {
                                Some(typ) => typ,
                                None => continue 'dep,
                            };
                        }
                        trace!("got `{}` = `{}`", self.diagnostics.resolve_span(field_var.span), typ);
                        (field_var, typ, Type::try_unify_similar)
                    }
                };
                self.add_solved(type_var, origin, typ.clone(), unification_function);
                self.worklist.push_back(WorklistItem {
                    solved: type_var,
                    origin: span,
                    typ: typ.clone(),
                });
            }
        }

        // check that no dependents are left
        for (binding, deps) in self.dependants {
            let mut diag = self.diagnostics.error(ErrorCode::UnableToInferType)
                .with_error_label(binding.span, "can't infer type for this binding");
            for (_span, dependant) in deps {
                let var = match dependant {
                    Dependant::FieldAccess(_, field_type_var) => field_type_var,
                    Dependant::Similar(var) => var,
                    Dependant::Sub(var) => var,
                    Dependant::Super(var) => var,
                };
                diag = diag.with_info_label(var.span, "thus: can't infer type for this expression");
            }
            diag.emit();
        }
        self.solved
    }

    fn add_solved<F>(&mut self, var: TypeVar, origin: Span, typ: Type, unification_function: F)
    where
        F: Fn(&Type, &Type) -> Result<Either<Type, Type>, UnificationError>,
    {
        match self.solved.get(&var) {
            Some((span_before, typ_before)) => match unification_function(typ_before, &typ) {
                Ok(Either::Left(_)) => (),
                Ok(Either::Right(t)) => { self.solved.insert(var, (origin, t.clone())); },
                Err(UnificationError::IncompatibleTypes) => {
                    let variable = self.diagnostics.resolve_span(var.span);
                    let origin_code = self.diagnostics.resolve_span(origin);
                    self.diagnostics.error(ErrorCode::TypeConflict)
                        .with_info_label(*span_before, format!("inferred type for `{}` is `{}`", variable, typ_before))
                        .with_info_label(origin, format!("but inferred type for `{}` is `{}`", origin_code, typ))
                        .with_error_label(var.span, format!("incompatible types"))
                        .emit()
                },
                Err(UnificationError::IncompatibleMutability) => {
                    self.diagnostics.error(ErrorCode::ImmutableAssign)
                        .with_error_label(origin, format!("tried to assign a value here"))
                        .with_info_label(*span_before, format!("inferred immutability here"))
                        .emit()
                },
            }
            _ => { self.solved.insert(var, (origin, typ.clone())); },
        }
    }
}
