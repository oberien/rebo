use std::io::ErrorKind;
use crate::common::{Value, MetaInfo, FuzzyFloat, DisplayValue};
use crate as rebo;
use itertools::Itertools;
use diagnostic::{DiagnosticBuilder, Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::parser::Expr;
use typed_arena::Arena;
use crate::ExecError;
use rand_chacha::ChaCha12Rng;
use rand::{Rng, SeedableRng, seq::SliceRandom};
use std::sync::Mutex;
use regex::Regex;
use unicode_segmentation::UnicodeSegmentation;
use rebo::VmContext;
use crate::util::{self, ResolveFileError, TryParseNumberResult};
use crate::RequiredReboFunctionStruct;

mod list;
mod option;
mod result;
mod map;
mod set;

pub use list::List;
pub use map::Map;

bitflags::bitflags! {
    pub struct Stdlib: u64 {
        const PRINT = 0x1;
        const ASSERT = 0x2;
        const PANIC = 0x4;
        const ASSERT_EQ = 0x8;
    }
}
mod required {
    use rebo::{FromValue, IntoValue};

    #[rebo::required_rebo_functions]
    extern "rebo" {
        pub fn print(..: _);
        pub fn assert(condition: bool);
        pub fn panic(message: String) -> !;
        pub fn assert_eq<T: IntoValue>(left: T, right: T);
    }
 }

pub fn add_to_meta_info<'a, 'i>(stdlib: Stdlib, diagnostics: &'i Diagnostics<ErrorCode>, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    meta_info.add_external_function(arena, diagnostics, clone);

    meta_info.add_external_type::<FileError>(arena, diagnostics);

    if stdlib.contains(Stdlib::PRINT) {
        meta_info.add_external_function(arena, diagnostics, print);
    }
    meta_info.add_required_rebo_function(RequiredReboFunctionStruct::from_required_rebo_function::<required::print>(), diagnostics);

    meta_info.add_external_function(arena, diagnostics, add_one);

    meta_info.add_external_function(arena, diagnostics, int_to_float);
    meta_info.add_external_function(arena, diagnostics, float_to_int);
    meta_info.add_external_function(arena, diagnostics, bool_to_int);
    meta_info.add_external_function(arena, diagnostics, float_min);
    meta_info.add_external_function(arena, diagnostics, float_max);
    meta_info.add_external_function(arena, diagnostics, float_floor);
    meta_info.add_external_function(arena, diagnostics, float_round);
    meta_info.add_external_function(arena, diagnostics, float_ceil);
    meta_info.add_external_function(arena, diagnostics, float_sqrt);
    meta_info.add_external_function(arena, diagnostics, float_to_bits);
    meta_info.add_external_function(arena, diagnostics, float_from_bits);
    meta_info.add_external_function(arena, diagnostics, int_min);
    meta_info.add_external_function(arena, diagnostics, int_max);
    meta_info.add_external_function(arena, diagnostics, int_min_value);
    meta_info.add_external_function(arena, diagnostics, int_max_value);
    meta_info.add_external_function(arena, diagnostics, int_abs);
    meta_info.add_external_function(arena, diagnostics, string_slice);
    meta_info.add_external_function(arena, diagnostics, string_len_utf8);
    meta_info.add_external_function(arena, diagnostics, string_len_utf16);
    meta_info.add_external_function(arena, diagnostics, string_len_utf32);
    meta_info.add_external_function(arena, diagnostics, string_len_grapheme_clusters);
    meta_info.add_external_function(arena, diagnostics, string_len_legacy_grapheme_clusters);
    meta_info.add_external_function(arena, diagnostics, string_from_char);
    meta_info.add_external_function(arena, diagnostics, string_trim);
    meta_info.add_external_function(arena, diagnostics, string_trim_start);
    meta_info.add_external_function(arena, diagnostics, string_trim_end);
    meta_info.add_external_function(arena, diagnostics, string_to_lowercase);
    meta_info.add_external_function(arena, diagnostics, string_to_uppercase);
    meta_info.add_external_function(arena, diagnostics, string_starts_with);
    meta_info.add_external_function(arena, diagnostics, string_ends_with);
    meta_info.add_external_function(arena, diagnostics, string_parse_int);
    meta_info.add_external_function(arena, diagnostics, string_parse_float);
    meta_info.add_external_function(arena, diagnostics, string_split);
    meta_info.add_external_function(arena, diagnostics, string_find_matches);
    meta_info.add_external_function(arena, diagnostics, string_captures);
    meta_info.add_external_function(arena, diagnostics, string_sorted);
    meta_info.add_external_function(arena, diagnostics, string_contains);
    meta_info.add_external_function(arena, diagnostics, string_replace);
    meta_info.add_external_function(arena, diagnostics, current_time_millis);
    meta_info.add_external_function(arena, diagnostics, rng_set_random_seed);
    meta_info.add_external_function(arena, diagnostics, rng_set_seed);
    meta_info.add_external_function(arena, diagnostics, rng_gen_int_range);
    meta_info.add_external_function(arena, diagnostics, file_read_to_string);

    if stdlib.contains(Stdlib::ASSERT) {
        meta_info.add_external_function(arena, diagnostics, assert);
    }
    meta_info.add_required_rebo_function(RequiredReboFunctionStruct::from_required_rebo_function::<required::assert>(), diagnostics);
    if stdlib.contains(Stdlib::ASSERT_EQ) {
        meta_info.add_external_function(arena, diagnostics, assert_eq);
    }
    meta_info.add_required_rebo_function(RequiredReboFunctionStruct::from_required_rebo_function::<required::assert_eq>(), diagnostics);
    if stdlib.contains(Stdlib::PANIC) {
        meta_info.add_external_function(arena, diagnostics, panic);
    }
    meta_info.add_required_rebo_function(RequiredReboFunctionStruct::from_required_rebo_function::<required::panic>(), diagnostics);

    meta_info.add_external_type::<Option<Value>>(arena, diagnostics);
    meta_info.add_external_type::<Result<Value, Value>>(arena, diagnostics);
    list::add_list(diagnostics, arena, meta_info);
    map::add_map(diagnostics, arena, meta_info);
    set::add_set(diagnostics, arena, meta_info);
}

#[rebo::function(raw("print"))]
fn print(..: _) {
    let joined = args.as_slice().iter().map(DisplayValue).join(", ");
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

// float helper functions
#[rebo::function(raw("float::min"))]
fn float_min(this: FuzzyFloat, ..: FuzzyFloat) -> FuzzyFloat {
    let floats = std::iter::once(this).chain(args.map(|val| val.expect_float("TypedVarargs is broken")));
    floats.min().unwrap()
}
#[rebo::function(raw("float::max"))]
fn float_max(this: FuzzyFloat, ..: FuzzyFloat) -> FuzzyFloat {
    let floats = std::iter::once(this).chain(args.map(|val| val.expect_float("TypedVarargs is broken")));
    floats.max().unwrap()
}
#[rebo::function("float::floor")]
fn float_floor(this: f64, decimals: u8) -> f64 {
    let factor = 10f64.powi(decimals as i32);
    (this * factor).floor() / factor
}
#[rebo::function("float::ceil")]
fn float_ceil(this: f64, decimals: u8) -> f64 {
    let factor = 10f64.powi(decimals as i32);
    (this * factor).ceil() / factor
}
#[rebo::function("float::round")]
fn float_round(this: f64, decimals: u8) -> f64 {
    let factor = 10f64.powi(decimals as i32);
    (this * factor).round() / factor
}
#[rebo::function("float::sqrt")]
fn float_sqrt(this: f64) -> f64 {
    this.sqrt()
}
#[rebo::function("float::to_bits")]
fn float_to_bits(this: f64) -> i64 {
    this.to_bits() as i64
}
#[rebo::function("float::from_bits")]
fn float_from_bits(bits: i64) -> f64 {
    f64::from_bits(bits as u64)
}

// int helper functions
#[rebo::function(raw("int::min"))]
fn int_min(this: i64, ..: i64) -> i64 {
    let ints = std::iter::once(this).chain(args.map(|val| val.expect_int("TypedVarargs is broken")));
    ints.min().unwrap()
}
#[rebo::function(raw("int::max"))]
fn int_max(this: i64, ..: i64) -> i64 {
    let ints = std::iter::once(this).chain(args.map(|val| val.expect_int("TypedVarargs is broken")));
    ints.max().unwrap()
}
#[rebo::function("int::min_value")]
fn int_min_value() -> i64 {
    i64::MIN
}
#[rebo::function("int::max_value")]
fn int_max_value() -> i64 {
    i64::MAX
}
#[rebo::function("int::abs")]
fn int_abs(this: i64) -> i64 {
    this.abs()
}

#[rebo::function(raw("assert"))]
fn assert(condition: bool) {
    if !condition {
        let diag = vm.diagnostics().error(ErrorCode::AssertionFailed)
            .with_error_label(expr_span, "this assertion failed");
        emit_stacktrace(vm, diag).emit();
        return Err(ExecError::Panic);
    }
}
#[rebo::function(raw("assert_eq"))]
fn assert_eq<T>(left: T, right: T) {
    if left != right {
        let diag = vm.diagnostics().error(ErrorCode::AssertionFailed)
            .with_error_label(expr_span, format!("this assertion failed with `{}` != `{}`", DisplayValue(&left), DisplayValue(&right)));
        emit_stacktrace(vm, diag).emit();
        return Err(ExecError::Panic);
    }
}
#[rebo::function(raw("panic"))]
fn panic(message: String) -> ! {
    let diag = vm.diagnostics().error(ErrorCode::Panic)
        .with_error_label(expr_span, message);
    emit_stacktrace(vm, diag).emit();
    Err(ExecError::Panic)
}

fn emit_stacktrace<'i>(vm: &VmContext, mut diag: DiagnosticBuilder<'i, ErrorCode>) -> DiagnosticBuilder<'i, ErrorCode> {
    let mut callstack = vm.callstack();
    // last call is the rust function itself
    callstack.pop().expect("emit_stacktrace called without being in a rust-functiontcall");
    for (i, span) in callstack.into_iter().enumerate() {
        diag = diag.with_info_label(span, format!("{}. in this function call", i+1));
    }
    diag
}

trait Sliceable: Sized {
    fn len(&self) -> usize;
    fn remove_start(&mut self, num: usize);
    fn remove_end(&mut self, num: usize);
    fn name() -> &'static str;
    fn slice<'a, 'i>(mut self, vm: &VmContext<'a, '_, '_, 'i>, expr_span: Span, start: i64, mut args: impl Iterator<Item = Value>) -> Result<Self, ExecError<'a, 'i>> {
        let end = args.next().map(|val| val.expect_int("TypedVarargs is broken as fuck"));
        if args.next().is_some() {
            vm.diagnostics().error(ErrorCode::Panic)
                .with_error_label(expr_span, format!("{}::slice must be called with one or two indices", Self::name()))
                .emit();
            return Err(ExecError::Panic);
        }

        let len = self.len();
        let start = if start < 0 { len as i64 + start } else { start };
        let start = start.max(0) as usize;
        let start = start.min(len);

        let end = end.map(|end| {
            let end = if end < 0 { len as i64 + end } else { end };
            let end = end.max(0) as usize;
            end.min(len)
        }).unwrap_or(len);

        self.remove_start(start);
        self.remove_end(len - end);
        Ok(self)
    }
}
impl Sliceable for String {
    fn len(&self) -> usize { self.chars().count() }
    fn remove_start(&mut self, num: usize) {
        if num == 0 {
            return;
        }
        let index = self.char_indices().nth(num).map(|(i, _)| i).unwrap();
        self.drain(..index);
    }
    fn remove_end(&mut self, num: usize) {
        if num == 0 {
            return;
        }
        let index = self.char_indices().nth_back(num-1).map(|(i, _)| i).unwrap();
        self.truncate(index)
    }
    fn name() -> &'static str { "string" }
}
#[rebo::function(raw("string::slice"))]
fn string_slice(this: String, start: i64, ..: i64) -> String {
    this.slice(vm, expr_span, start, args)?
}
#[rebo::function(raw("string::len_utf8"))]
fn string_len_utf8(this: String) -> usize {
    this.len()
}
#[rebo::function(raw("string::len_utf16"))]
fn string_len_utf16(this: String) -> usize {
    this.encode_utf16().count()
}
#[rebo::function(raw("string::len_utf32"))]
fn string_len_utf32(this: String) -> usize {
    this.chars().count()
}
#[rebo::function(raw("string::len_grapheme_clusters"))]
fn string_len_grapheme_clusters(this: String) -> usize {
    this.graphemes(true).count()
}
#[rebo::function(raw("string::len_legacy_grapheme_clusters"))]
fn string_len_legacy_grapheme_clusters(this: String) -> usize {
    this.graphemes(false).count()
}
#[rebo::function("string::from_char")]
fn string_from_char(chr: u8) -> String {
    String::from(chr as char)
}
#[rebo::function("string::trim")]
fn string_trim(this: String) -> String {
    this.trim().to_string()
}
#[rebo::function("string::trim_start")]
fn string_trim_start(this: String) -> String {
    this.trim_start().to_string()
}
#[rebo::function("string::trim_end")]
fn string_trim_end(this: String) -> String {
    this.trim_end().to_string()
}
#[rebo::function("string::to_lowercase")]
fn string_to_lowercase(this: String) -> String {
    this.to_lowercase()
}
#[rebo::function("string::to_uppercase")]
fn string_to_uppercase(this: String) -> String {
    this.to_uppercase()
}
#[rebo::function("string::starts_with")]
fn string_starts_with(this: String, other: String) -> bool {
    this.starts_with(&other)
}
#[rebo::function("string::ends_with")]
fn string_ends_with(this: String, other: String) -> bool {
    this.ends_with(&other)
}
#[rebo::function("string::parse_int")]
fn string_parse_int(this: String) -> Result<i64, ()> {
    match util::try_parse_number(&this) {
        TryParseNumberResult::Int(i, _, _) => Ok(i),
        TryParseNumberResult::Float(_, _, _) => Err(()),
        TryParseNumberResult::Error(_, _, _) => Err(()),
    }
}
#[rebo::function("string::parse_float")]
fn string_parse_float(this: String) -> Result<f64, ()> {
    match util::try_parse_number(&this) {
        TryParseNumberResult::Int(i, _, _) => Ok(i as f64),
        TryParseNumberResult::Float(f, _, _) => Ok(f),
        TryParseNumberResult::Error(_, _, _) => Err(()),
    }
}

fn compile_regex<'a, 'i>(regex: String, vm: &VmContext<'a, '_, '_, 'i>, expr_span: Span) -> Result<Regex, ExecError<'a, 'i>> {
    match Regex::new(&regex) {
        Ok(regex) => Ok(regex),
        Err(regex::Error::Syntax(msg)) => {
            vm.diagnostics().error(ErrorCode::InvalidRegex)
                .with_error_label(expr_span, format!("syntax error: {}", msg))
                .emit();
            Err(ExecError::Panic)
        }
        Err(_) => {
            vm.diagnostics().error(ErrorCode::InvalidRegex)
                .with_error_label(expr_span, "invalid regex")
                .emit();
            Err(ExecError::Panic)
        }
    }
}
#[rebo::function(raw("string::split"))]
fn string_split(this: String, regex: String) -> List<String> {
    let regex = compile_regex(regex, vm, expr_span)?;
    List::new(regex.split(&this).map(|s| s.to_owned()))
}
#[rebo::function(raw("string::find_matches"))]
fn string_find_matches(this: String, regex: String) -> List<String> {
    let regex = compile_regex(regex, vm, expr_span)?;
    List::new(regex.find_iter(&this).map(|m| m.as_str().to_string()))
}
#[rebo::function(raw("string::captures"))]
fn string_captures(this: String, regex: String) -> Option<List<Option<String>>> {
    let regex = compile_regex(regex, vm, expr_span)?;
    regex.captures(&this).map(|c| List::new(c.iter().map(|m| m.map(|m| m.as_str().to_string()))))
}
#[rebo::function(raw("string::sorted"))]
fn string_sorted(this: String) -> String {
    itertools::sorted(this.chars()).collect()
}
#[rebo::function(raw("string::contains"))]
fn string_contains(this: String, regex: String) -> bool {
    let regex = compile_regex(regex, vm, expr_span)?;
    regex.is_match(&this)
}
#[rebo::function(raw("string::replace"))]
fn string_replace(this: String, regex: String, with: String) -> String {
    let regex = compile_regex(regex, vm, expr_span)?;
    regex.replace_all(&this, with).into_owned()
}

#[rebo::function("current_time_millis")]
fn current_time_millis() -> u64 {
    std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as u64
}

// RNG
lazy_static::lazy_static! {
    static ref RNG: Mutex<ChaCha12Rng> = Mutex::new(ChaCha12Rng::seed_from_u64(0));
}
#[rebo::function("Rng::set_random_seed")]
fn rng_set_random_seed() -> i64 {
    let seed: i64 = rand::random();
    *RNG.lock().unwrap() = ChaCha12Rng::seed_from_u64(seed as u64);
    seed
}
#[rebo::function("Rng::set_seed")]
fn rng_set_seed(seed: i64) {
    *RNG.lock().unwrap() = ChaCha12Rng::seed_from_u64(seed as u64);
}
#[rebo::function("Rng::gen_int_range")]
fn rng_gen_int_range(from: i64, to: i64) -> i64 {
    RNG.lock().unwrap().gen_range(from..to)
}
#[rebo::function("Rng::shuffle")]
fn rng_shuffle<T>(list: List<T>) {
    list.arc.list.lock().borrow_mut().shuffle(&mut *RNG.lock().unwrap())
}

// File
#[derive(rebo::ExternalType)]
enum FileError {
    NotFound,
    AccessError,
}
#[rebo::function(raw("File::read_to_string"))]
fn file_read_to_string(name: String) -> Result<String, FileError> {
    (|| {
        let path = match util::try_resolve_file(vm.include_directory(), name) {
            Ok(path) => path,
            Err(ResolveFileError::Canonicalize(path, e)) => {
                vm.diagnostics().error(ErrorCode::FileError)
                    .with_error_label(expr_span, format!("error canonicalizing `{}`", path.display()))
                    .with_error_label(expr_span, e.to_string())
                    .emit();
                return Err(ExecError::Panic);
            }
            Err(ResolveFileError::StartsWith(path)) => {
                vm.diagnostics().error(ErrorCode::FileError)
                    .with_error_label(expr_span, "the file is not in the include directory")
                    .with_info_label(expr_span, format!("this file resolved to {}", path.display()))
                    .with_error_label(expr_span, format!("included files must be in {}", vm.include_directory().unwrap_path().display()))
                    .emit();
                return Err(ExecError::Panic);
            }
        };
        match std::fs::read_to_string(path) {
            Ok(content) => Ok(Ok(content)),
            Err(e) if e.kind() == ErrorKind::NotFound => Ok(Err(FileError::NotFound)),
            Err(_) => Ok(Err(FileError::AccessError)),
        }
    })()?
}

#[rebo::function("__internal_clone_")]
fn clone<T>(t: T) -> T {
    use crate::common::DeepCopy;
    t.deep_copy()
}
#[cfg(test)]
mod test {
    use crate::ReturnValue;
    use crate::tests::test;

    #[test]
    fn test_float_rounding() {
        test(r#"
            let val = 3.1454545494545454628;
            assert(float::floor(val, 0) == 3.);
            assert(float::ceil(val, 0) == 4.);
            assert(float::round(val, 0) == 3.);

            assert(float::floor(val, 2) == 3.14);
            assert(float::ceil(val, 2) == 3.15);
            assert(float::round(val, 2) == 3.15);

            assert(float::floor(val, 9) == 3.145454549);
            assert(float::ceil(val, 9) == 3.14545455);
            assert(float::round(val, 9) == 3.145454549);

            assert(float::round(0.5, 0) == 1.);
            assert(float::round(0.99999, 0) == 1.);
            assert(float::round(0.99999, 2) == 1.);
            assert(float::round(0.00001, 0) == 0.);
            assert(float::round(1.0, 0) == 1.);
            assert(float::round(1.0, 1) == 1.0);
            assert(float::round(0.0, 0) == 0.);
            assert(float::round(0.0, 1) == 0.0);
        "#, ReturnValue::Ok)
    }
}