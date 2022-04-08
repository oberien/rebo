use crate::lints::visitor::Visitor;
use diagnostic::Diagnostics;
use crate::common::{MetaInfo, BlockStack, BlockType};
use crate::error_codes::ErrorCode;
use crate::parser::{ExprBreak, Spanned, ExprContinue, ExprReturn};

pub struct BreakContinueReturn;

impl Visitor for BreakContinueReturn {
    fn visit_break(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, block_stack: &BlockStack<'_, '_, ()>, br: &ExprBreak) {
        let ExprBreak { break_token: _, label, expr } = br;

        match (block_stack.get_loop_like(label.as_ref()), label) {
            (None, Some(label)) => {
                diagnostics.error(ErrorCode::BreakLabelNotFound)
                    .with_error_label(label.span(), "unknown break label")
                    .emit();
                return
            }
            (None, None) => {
                diagnostics.error(ErrorCode::BreakOutsideOfLoopLike)
                    .with_error_label(br.span(), "this break is not inside a loop")
                    .emit();
                return
            }
            _ => (),
        }

        if let Some(expr) = expr {
            match block_stack.get_loop_like(label.as_ref()) {
                Some((BlockType::Loop(_), _))  => (),
                Some(_) => diagnostics.error(ErrorCode::BreakValueInNonLoop)
                    .with_error_label(expr.span(), "break with value can only be used inside `loop`")
                    .emit(),
                None => unreachable!(),
            }
        }
    }

    fn visit_continue(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, block_stack: &BlockStack<'_, '_, ()>, cont: &ExprContinue) {
        let ExprContinue { continue_token: _, label} = cont;

        match (block_stack.get_loop_like(label.as_ref()), label) {
            (None, Some(label)) => {
                diagnostics.error(ErrorCode::ContinueLabelNotFound)
                    .with_error_label(label.span(), "unknown continue label")
                    .emit();
                return
            }
            (None, None) => {
                diagnostics.error(ErrorCode::ContinueOutsideOfLoopLike)
                    .with_error_label(cont.span(), "this continue is not inside a loop")
                    .emit();
                return
            }
            _ => (),
        }
    }

    fn visit_return(&self, diagnostics: &Diagnostics<ErrorCode>, _: &MetaInfo, block_stack: &BlockStack<'_, '_, ()>, ret: &ExprReturn) {
        match block_stack.get_function() {
            None => {
                diagnostics.error(ErrorCode::ReturnOutsideOfFunction)
                    .with_error_label(ret.span(), "this return is not inside a function")
                    .emit();
                return
            }
            _ => (),
        }
    }
}
