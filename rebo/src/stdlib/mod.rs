use crate::common::{Value, MetaInfo, FuzzyFloat};
use crate as rebo;
use itertools::Itertools;
use diagnostic::Diagnostics;
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
        meta_info.add_external_function(arena, diagnostics, print);
    }

    meta_info.add_external_function(arena, diagnostics, add_one);

    meta_info.add_external_function(arena, diagnostics, int_to_float);
    meta_info.add_external_function(arena, diagnostics, float_to_int);
    meta_info.add_external_function(arena, diagnostics, bool_to_int);
    meta_info.add_external_function(arena, diagnostics, string_slice);
    meta_info.add_external_function(arena, diagnostics, string_from_char);
    meta_info.add_external_function(arena, diagnostics, string_to_lowercase);
    meta_info.add_external_function(arena, diagnostics, string_to_uppercase);

    if stdlib.contains(Stdlib::ASSERT) {
        meta_info.add_external_function(arena, diagnostics, assert);
    }
    if stdlib.contains(Stdlib::PANIC) {
        meta_info.add_external_function(arena, diagnostics, panic);
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

#[rebo::function(raw("assert"))]
fn assert(condition: bool) {
    if !condition {
        vm.diagnostics().error(ErrorCode::AssertionFailed)
            .with_error_label(expr_span, "this assertion failed")
            .emit();
        return Err(ExecError::Panic);
    }
}
#[rebo::function(raw("panic"))]
fn panic(message: String) -> ! {
    vm.diagnostics().error(ErrorCode::Panic)
        .with_error_label(expr_span, message)
        .emit();
    Err(ExecError::Panic)
}
#[rebo::function(raw("string::slice"))]
fn string_slice(mut this: String, start: i64, ..: i64) -> String {
    let end = args.next().map(|val| val.expect_int("TypedVarargs is broken as fuck"));
    if args.next().is_some() {
        vm.diagnostics().error(ErrorCode::Panic)
            .with_error_label(expr_span, "string::slice must be called with one or two indices")
            .emit();
        return Err(ExecError::Panic);
    }

    let len = this.len() as i64;
    let start = if start < 0 { len + start } else { start };
    let start = start.max(0) as usize;
    let start = start.min(this.len());

    let end = end.map(|end| {
        let end = if end < 0 { len + end } else { end };
        let end = end.max(0) as usize;
        end.min(this.len())
    }).unwrap_or(this.len());

    this.truncate(end);
    this.drain(..start);
    this
}
#[rebo::function("string::from_char")]
fn string_from_char(chr: u8) -> String {
    String::from(chr as char)
}
#[rebo::function("string::to_lowercase")]
fn string_to_lowercase(this: String) -> String {
    this.to_lowercase()
}
#[rebo::function("string::to_uppercase")]
fn string_to_uppercase(this: String) -> String {
    this.to_uppercase()
}
