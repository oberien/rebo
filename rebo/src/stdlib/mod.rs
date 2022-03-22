use std::io::ErrorKind;
use std::num::IntErrorKind;
use crate::common::{Value, MetaInfo, FuzzyFloat, DisplayValue};
use crate as rebo;
use itertools::Itertools;
use diagnostic::{Diagnostics, Span};
use crate::error_codes::ErrorCode;
use crate::parser::Expr;
use typed_arena::Arena;
use crate::{ExecError, util};
use rand_chacha::ChaCha12Rng;
use rand::{Rng, SeedableRng, seq::SliceRandom};
use std::sync::Mutex;
use regex::Regex;
use unicode_segmentation::UnicodeSegmentation;
use rebo::VmContext;
use crate::util::ResolveFileError;

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
    }
}

pub fn add_to_meta_info<'a, 'i>(stdlib: Stdlib, diagnostics: &'i Diagnostics, arena: &'a Arena<Expr<'a, 'i>>, meta_info: &mut MetaInfo<'a, 'i>) {
    meta_info.add_external_type::<ParseIntError>(arena, diagnostics);
    meta_info.add_external_type::<FileError>(arena, diagnostics);

    if stdlib.contains(Stdlib::PRINT) {
        meta_info.add_external_function(arena, diagnostics, print);
    }

    meta_info.add_external_function(arena, diagnostics, add_one);

    meta_info.add_external_function(arena, diagnostics, int_to_float);
    meta_info.add_external_function(arena, diagnostics, float_to_int);
    meta_info.add_external_function(arena, diagnostics, bool_to_int);
    meta_info.add_external_function(arena, diagnostics, float_min);
    meta_info.add_external_function(arena, diagnostics, float_max);
    meta_info.add_external_function(arena, diagnostics, float_round);
    meta_info.add_external_function(arena, diagnostics, float_sqrt);
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
    meta_info.add_external_function(arena, diagnostics, string_split);
    meta_info.add_external_function(arena, diagnostics, string_find_matches);
    meta_info.add_external_function(arena, diagnostics, string_sorted);
    meta_info.add_external_function(arena, diagnostics, string_contains);
    meta_info.add_external_function(arena, diagnostics, current_time_millis);
    meta_info.add_external_function(arena, diagnostics, rng_set_random_seed);
    meta_info.add_external_function(arena, diagnostics, rng_set_seed);
    meta_info.add_external_function(arena, diagnostics, rng_gen_int_range);
    meta_info.add_external_function(arena, diagnostics, file_read_to_string);

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
    set::add_set(diagnostics, arena, meta_info);
}

#[rebo::function(raw("print"))]
fn print(..: _) {
    let joined = args.as_slice().into_iter().map(|arg| DisplayValue(arg)).join(", ");
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
#[rebo::function("float::min")]
fn float_min(this: f64, other: f64) -> f64 {
    this.min(other)
}
#[rebo::function("float::max")]
fn float_max(this: f64, other: f64) -> f64 {
    this.max(other)
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

// int helper functions
#[rebo::function("int::min")]
fn int_min(this: i64, other: i64) -> i64 {
    this.min(other)
}
#[rebo::function("int::max")]
fn int_max(this: i64, other: i64) -> i64 {
    this.max(other)
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
trait Sliceable: Sized {
    fn len(&self) -> usize;
    fn truncate(&mut self, new_len: usize);
    fn remove_start(&mut self, until: usize);
    fn name() -> &'static str;
    fn slice<'a, 'i>(mut self, vm: &VmContext<'a, '_, '_, 'i>, expr_span: Span, start: i64, mut args: impl Iterator<Item = Value>) -> Result<Self, ExecError<'a, 'i>> {
        let end = args.next().map(|val| val.expect_int("TypedVarargs is broken as fuck"));
        if args.next().is_some() {
            vm.diagnostics().error(ErrorCode::Panic)
                .with_error_label(expr_span, format!("{}::slice must be called with one or two indices", Self::name()))
                .emit();
            return Err(ExecError::Panic);
        }

        let len = self.len() as i64;
        let start = if start < 0 { len + start } else { start };
        let start = start.max(0) as usize;
        let start = start.min(self.len());

        let end = end.map(|end| {
            let end = if end < 0 { len + end } else { end };
            let end = end.max(0) as usize;
            end.min(self.len())
        }).unwrap_or(self.len());

        self.truncate(end);
        self.remove_start(start);
        Ok(self)
    }
}
impl Sliceable for String {
    fn len(&self) -> usize { self.len() }
    fn truncate(&mut self, new_len: usize) { self.truncate(new_len) }
    fn remove_start(&mut self, until: usize) { self.drain(..until); }
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
#[derive(rebo::ExternalType)]
enum ParseIntError {
    Empty,
    TooLarge,
    TooSmall,
    InvalidDigit,
}
#[rebo::function("string::parse_int")]
fn string_parse_int(this: String) -> Result<u64, ParseIntError> {
    match this.parse::<u64>() {
        Ok(res) => Ok(res),
        Err(e) => match e.kind() {
            IntErrorKind::Empty => Err(ParseIntError::Empty),
            IntErrorKind::InvalidDigit => Err(ParseIntError::InvalidDigit),
            IntErrorKind::PosOverflow => Err(ParseIntError::TooLarge),
            IntErrorKind::NegOverflow => Err(ParseIntError::TooSmall),
            _ => unreachable!("unknown error during u64::from_str: {:?}", e),
        }
    }
}

fn compile_regex<'a, 'i>(regex: String, vm: &VmContext<'a, '_, '_, 'i>, expr_span: Span) -> Result<Regex, ExecError<'a, 'i>> {
    match Regex::new(&regex) {
        Ok(regex) => Ok(regex),
        Err(regex::Error::Syntax(msg)) => {
            vm.diagnostics().error(ErrorCode::InvalidRegex)
                .with_error_label(expr_span, format!("syntax error: {}", msg))
                .emit();
            return Err(ExecError::Panic);
        }
        Err(_) => {
            vm.diagnostics().error(ErrorCode::InvalidRegex)
                .with_error_label(expr_span, "invalid regex")
                .emit();
            return Err(ExecError::Panic);
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
#[rebo::function(raw("string::sorted"))]
fn string_sorted(this: String) -> String {
    itertools::sorted(this.chars()).collect()
}
#[rebo::function(raw("string::contains"))]
fn string_contains(this: String, regex: String) -> bool {
    let regex = compile_regex(regex, vm, expr_span)?;
    regex.is_match(&this)
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
    RNG.lock().unwrap().gen_range(from as u64..to as u64) as i64
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
                    .with_error_label(expr_span, "the file in not in the include directory")
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
