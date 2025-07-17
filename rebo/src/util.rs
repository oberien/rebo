use std::fmt;
use std::path::{Path, PathBuf};
use rt_format::argument::ArgumentSource;
use crate::{Value, IncludeDirectory};
use crate::lexer::Radix;

pub struct PadFmt<T: fmt::Write> {
    f: T,
    on_newline: bool,
}

impl<T: fmt::Write> PadFmt<T> {
    pub fn new(f: T) -> Self {
        PadFmt {
            f,
            on_newline: true,
        }
    }
}

impl<T: fmt::Write> fmt::Write for PadFmt<T> {
    // modified from stdlib
    fn write_str(&mut self, mut s: &str) -> fmt::Result {
        while !s.is_empty() {
            if self.on_newline {
                self.f.write_str("    ")?;
            }

            let split = match s.find('\n') {
                Some(pos) => {
                    self.on_newline = true;
                    pos + 1
                }
                None => {
                    self.on_newline = false;
                    s.len()
                }
            };
            self.f.write_str(&s[..split])?;
            s = &s[split..];
        }
        Ok(())
    }
}

pub fn similar_name<'i, T: AsRef<str> + 'i + ?Sized>(ident: &str, others: impl IntoIterator<Item = &'i T>) -> Option<&'i str> {
    others.into_iter()
        .map(|s| (strsim::levenshtein(ident, s.as_ref()), s.as_ref()))
        // .filter(|&(dist, _)| dist <= 3)
        .min_by_key(|&(dist, _)| dist)
        .map(|(_, s)| s)
}

pub enum ResolveFileError {
    Canonicalize(PathBuf, std::io::Error),
    StartsWith(PathBuf),
}
pub fn try_resolve_file<P: AsRef<Path>>(include_directory: &IncludeDirectory, file: P) -> Result<PathBuf, ResolveFileError> {
    let include_directory = match include_directory {
        IncludeDirectory::Everywhere => return Ok(file.as_ref().to_owned()),
        IncludeDirectory::Path(path) => path,
    };
    let path = include_directory.join(file.as_ref());
    let path = match path.canonicalize() {
        Ok(path) => path,
        Err(e) => {
            return Err(ResolveFileError::Canonicalize(path, e));
        }
    };
    // we all love UNC
    let include_directory = match include_directory.canonicalize() {
        Ok(include_directory) => include_directory,
        Err(e) => {
            return Err(ResolveFileError::Canonicalize(include_directory.clone(), e));
        }
    };
    if !path.starts_with(&include_directory) {
        return Err(ResolveFileError::StartsWith(path));
    }
    Ok(path)
}

pub struct NoValues;
impl ArgumentSource<Value> for NoValues {
    fn next_argument(&mut self) -> Option<&Value> { None }
    fn lookup_argument_by_index(&self, _: usize) -> Option<&Value> { None }
    fn lookup_argument_by_name(&self, _: &str) -> Option<&Value> { None }
}

#[derive(Debug, PartialEq)]
pub enum TryParseNumberResult {
    /// number, radix, end-index
    Int(i64, Radix, usize),
    /// number, radix, end-index
    Float(f64, Radix, usize),
    /// error, radix, end-index
    Error(lexical::Error, Radix, usize),
}

pub fn try_parse_number(s: &str) -> TryParseNumberResult {
    let mut index = 0;
    let mut radix = Radix::Dec;
    let mut factor_int = 1;
    let mut factor_float = 1.;
    if s.starts_with('-') {
        index += 1;
        factor_int = -1;
        factor_float = -1.;
    }
    if s.starts_with("0b") {
        index += 2;
        radix = Radix::Bin;
    } else if s.starts_with("0x") {
        index += 2;
        radix = Radix::Hex;
    }
    let number_end = index + s[index..].chars()
        .take_while(|&c| c.is_alphanumeric() || c == '.')
        .map(char::len_utf8)
        .sum::<usize>();
    let int = lexical::parse_radix::<i64, _>(&s[index..number_end], radix.to_u8());
    let float = lexical::parse_radix::<f64, _>(&s[index..number_end], radix.to_u8());

    match (int, float) {
        (Ok(i), _) => TryParseNumberResult::Int(i * factor_int, radix, number_end),
        (_, Ok(f)) => TryParseNumberResult::Float(f * factor_float, radix, number_end),
        (_, Err(e)) => TryParseNumberResult::Error(e, radix, number_end),
    }
}

pub enum TryParseDqstringResult {
    /// dqstring content, end (index after closeing dq)
    String(String, usize),
    DoesntStartWithDq,
    /// end of passed string
    Unterminated(usize),
}
pub fn try_parse_dqstring(s: &str) -> TryParseDqstringResult {
    if s.chars().next() != Some('"') {
        return TryParseDqstringResult::DoesntStartWithDq;
    }
    let mut res = String::new();
    let mut index = 1;
    loop {
        if let Some('"') = s[index..].chars().next() {
            index += 1;
            break;
        }
        match lex_string_char(s, index, false) {
            None => return TryParseDqstringResult::Unterminated(index),
            Some((c, idx)) => {
                res.push(c);
                index = idx;
            }
        }
    };
    TryParseDqstringResult::String(res, index)
}
fn lex_string_char(s: &str, index: usize, escaped: bool) -> Option<(char, usize)> {
    let next = s[index..].chars().next()?;

    let c = match next {
        '\\' if !escaped => return lex_string_char(s, index + 1, true),
        c if escaped => match lex_escaped_char(c) {
            EscapedResult::ControlChar(c, _) | EscapedResult::Escaped(c) => c,
        },
        c => c,
    };
    Some((c, index + c.len_utf8()))
}

pub enum EscapedResult {
    ControlChar(char, &'static str),
    Escaped(char),
}
pub fn lex_escaped_char(c: char) -> EscapedResult {
    match c {
        'n' => EscapedResult::ControlChar('\n', "\n"),
        'r' => EscapedResult::ControlChar('\r', "\r"),
        't' => EscapedResult::ControlChar('\t', "\t"),
        c => EscapedResult::Escaped(c),
    }
}

macro_rules! assert_matches {
    ($scrutinee:expr, $expected:pat) => {{
        let scrutinee = $scrutinee;
        match scrutinee {
            $expected => (),
            _ => panic!("assertion failed: {scrutinee:?} does not match {}", stringify!($expected))
        }
    }};
}

#[cfg(test)]
mod test {
    use super::*;
    use super::TryParseNumberResult::*;

    #[test]
    fn test_try_parse_number() {
        assert_eq!(try_parse_number("1337"), Int(1337, Radix::Dec, 4));
        assert_eq!(try_parse_number("0x1337"), Int(0x1337, Radix::Hex, 6));
        assert_eq!(try_parse_number("0b1001"), Int(0b1001, Radix::Bin, 6));
        assert_eq!(try_parse_number("1.5"), Float(1.5, Radix::Dec, 3));
        assert_matches!(try_parse_number("."), Error(_, Radix::Dec, 1));
        assert_matches!(try_parse_number("..."), Error(_, Radix::Dec, 3));
        assert_eq!(try_parse_number(".5"), Float(0.5, Radix::Dec, 2));
        assert_eq!(try_parse_number("5."), Float(5., Radix::Dec, 2));
        assert_eq!(try_parse_number("0x10."), Float(16., Radix::Hex, 5));
        assert_eq!(try_parse_number("0x10."), Float(16., Radix::Hex, 5));
        
        assert_matches!(try_parse_number("10.a"), Error(_, _, 4));
        assert_eq!(try_parse_number("10. "), Float(10., Radix::Dec, 3));
    }
}
