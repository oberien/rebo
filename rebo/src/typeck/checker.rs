use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::typeck::TypeVar;
use crate::common::{SpecificType, Type};
use std::collections::BTreeMap;
use itertools::Itertools;

pub struct Checker<'i> {
    diagnostics: &'i Diagnostics,
    solved: BTreeMap<TypeVar, (Span, Type)>,
    restrictions: Vec<(TypeVar, Vec<SpecificType>)>,
}

impl<'i> Checker<'i> {
    pub fn new(diagnostics: &'i Diagnostics, solved: BTreeMap<TypeVar, (Span, Type)>, restrictions: Vec<(TypeVar, Vec<SpecificType>)>) -> Self {
        Checker { diagnostics, solved, restrictions }
    }
    pub fn check(self) {
        for (var, restrictions) in self.restrictions {
            let (span, solved) = &self.solved[&var];
            if !restrictions.iter().any(|r| solved.try_unify(&Type::Specific(r.clone())).is_ok()) {
                let one_of = if restrictions.len() == 1 { "" } else { "one of " };
                self.diagnostics.error(ErrorCode::TypeConflict)
                    .with_error_label(*span, format!("inferred type is {}", solved))
                    .with_info_label(var.span, format!("but expected {}`{}`", one_of, restrictions.iter().join("`, `")))
                    .emit();
            }
        }
    }
}

