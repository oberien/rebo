use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{BlockStack, MetaInfo};
use crate::common::Spanned;
use crate::parser::ExprIfElse;
use crate::typeck::TypeVar;
use crate::error_codes::ErrorCode;
use crate::typeck::types::{SpecificType, Type};

pub struct IfElseReturnValue;

impl Visitor for IfElseReturnValue {
    fn visit_if_else(&self, diagnostics: &Diagnostics<ErrorCode>, meta_info: &MetaInfo, _: &BlockStack<'_, ()>, ifelse: &ExprIfElse) {
        let block_type_vars: Vec<_> = ifelse.iter_branches()
            .map(|(_cond, block)| TypeVar::from_spanned(block))
            .collect();
        if block_type_vars.iter().any(|var| meta_info.types[var] != Type::Specific(SpecificType::Unit)) {
            if ifelse.els.is_none() {
                diagnostics.error(ErrorCode::MissingElse)
                    .with_error_label(ifelse.diagnostics_span(), "missing else-branch in this if")
                    .with_note("if the `if` should return a value, all branches must return a value")
                    .emit();
            }

            return;
            // TODO: unsure if these are needed later on; they are covered by type-checker errors already
            #[allow(unreachable_code)]
            for (_cond, block) in ifelse.iter_branches() {
                // if returns value but block is empty
                if block.body.exprs.is_empty() {
                    diagnostics.error(ErrorCode::MissingBranchBody)
                        .with_error_label(block.diagnostics_span(), "this branch body is expected to evaluate to a value")
                        .with_info_label(ifelse.diagnostics_span(), "all branches in this if must evaluate to a value")
                        .with_note("if not all branches are terminated with a `;` an if-expression evalutes to a value")
                        .emit();
                    // if returns value but block is terminated
                }
                if block.body.terminated_with_semicolon {
                    diagnostics.error(ErrorCode::MissingBranchValue)
                        .with_error_label(block.diagnostics_span(), "this block doesn't evaluate to a value")
                        .with_info_label(ifelse.diagnostics_span(), "all branches in this if must evaluate to a value")
                        .with_note("if not all branches are terminated with a `;` an if-expression evalutes to a value")
                        .emit();
                }
            }
        }
    }
}