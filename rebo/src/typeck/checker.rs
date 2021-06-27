use diagnostic::Diagnostics;
use crate::error_codes::ErrorCode;
use crate::typeck::TypeVar;
use crate::common::{SpecificType, Type};
use std::collections::BTreeMap;
use itertools::Itertools;

pub struct Checker<'i> {
    diagnostics: &'i Diagnostics,
    solved: BTreeMap<TypeVar, Type>,
    restrictions: Vec<(TypeVar, Vec<SpecificType>)>,
}

impl<'i> Checker<'i> {
    pub fn new(diagnostics: &'i Diagnostics, solved: BTreeMap<TypeVar, Type>, restrictions: Vec<(TypeVar, Vec<SpecificType>)>) -> Self {
        Checker { diagnostics, solved, restrictions }
    }
    pub fn check(self) {
        for (var, restrictions) in self.restrictions {
            let solved = &self.solved[&var];
            if !restrictions.iter().any(|r| solved.try_unify(&Type::Specific(r.clone())).is_ok()) {
                self.diagnostics.error(ErrorCode::TypeConflict)
                    .with_error_label(var.span, format!("inferred type is {}", solved))
                    .with_info_label(var.span, format!("but expected one of {}", restrictions.iter().join(", ")))
                    .emit();
            }
        }
    }
}

