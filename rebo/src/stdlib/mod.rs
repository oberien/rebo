use crate::common::{Value, MetaInfo, FuzzyFloat};
use crate as rebo;
use itertools::Itertools;
use diagnostic::Diagnostics;
use crate::typeck::types::{Type, SpecificType};
use crate::error_codes::ErrorCode;
use crate::parser::Expr;
use typed_arena::Arena;
use crate::ExecError;

mod list;
mod option;
mod result;
mod map;

bitflags::bitflags! {
    pub struct Stdlib: u64 {
        const PRINT = 0x1;
        const ASSERT = 0x2;
        const PANIC = 0x4;
    }
}

pub fn add_to_meta_info<'a, 'i>(stdlib: Stdlib, diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    if stdlib.contains(Stdlib::PRINT) {
        meta_info.add_external_function(diagnostics, print);
    }

    meta_info.add_external_function(diagnostics, add_one);

    meta_info.add_external_function(diagnostics, int_to_float);
    meta_info.add_external_function(diagnostics, float_to_int);
    meta_info.add_external_function(diagnostics, bool_to_int);

    if stdlib.contains(Stdlib::ASSERT) {
        meta_info.add_external_function(diagnostics, assert);
    }
    if stdlib.contains(Stdlib::PANIC) {
        meta_info.add_external_function(diagnostics, panic);
    }

    meta_info.add_external_type::<Option<Value>>(arena, diagnostics);
    meta_info.add_external_type::<Result<Value, Value>>(arena, diagnostics);
    list::add_list(diagnostics, arena, meta_info);
    map::add_map(diagnostics, arena, meta_info);
}

#[rebo::function(raw("print"))]
fn print(..: _) {
    let joined = args.join(", ");
    println!("{}", joined);
}

#[rebo::function("add_one")]
fn add_one(a: i64) -> i64 {
    a + 1
}

// type conversions
#[rebo::function("int::to_float")]
fn int_to_float(this: i64) -> FuzzyFloat {
    FuzzyFloat(this as f64)
}
#[rebo::function("bool::to_int")]
fn bool_to_int(this: bool) -> i64 {
    this as i64
}
#[rebo::function("float::to_int")]
fn float_to_int(this: FuzzyFloat) -> i64 {
    this.0 as i64
}

#[rebo::function(raw("assert", return Type::Specific(SpecificType::Unit)))]
fn assert(condition: bool) -> Result<Value, ExecError> {
    if !condition {
        vm.diagnostics().error(ErrorCode::AssertionFailed)
            .with_error_label(expr_span, "this assertion failed")
            .emit();
        Err(ExecError::Panic)
    } else {
        Ok(Value::Unit)
    }
}
#[rebo::function(raw("panic", return Type::Top))]
fn panic(message: String) -> Result<Value, ExecError> {
    vm.diagnostics().error(ErrorCode::Panic)
        .with_error_label(expr_span, message)
        .emit();
    Err(ExecError::Panic)
}
