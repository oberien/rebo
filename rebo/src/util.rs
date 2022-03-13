use std::fmt;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use rt_format::argument::ArgumentSource;
use crate::{Value, IncludeDirectory};

/// Workaround for <https://github.com/rust-lang/rust/issues/89940>
#[derive(Debug, Clone, Ord, PartialOrd, Eq, Hash)]
pub enum CowVec<'a, T> {
    Borrowed(&'a [T]),
    Owned(Vec<T>),
}
impl<'a, T: Clone> CowVec<'a, T> {
    pub fn to_owned(&self) -> Vec<T> {
        self.deref().to_vec()
    }
}
impl<'a, T> Deref for CowVec<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        match self {
            CowVec::Borrowed(b) => b,
            CowVec::Owned(v) => v,
        }
    }
}
impl<'a, 'b, T> IntoIterator for &'b CowVec<'a, T> {
    type Item = &'b T;
    type IntoIter = std::slice::Iter<'b, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.deref().iter()
    }
}
impl<'a, T: PartialEq> PartialEq for CowVec<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        let left = match self {
            CowVec::Owned(vec) => vec.as_slice(),
            CowVec::Borrowed(slice) => slice,
        };
        let right = match other {
            CowVec::Owned(vec) => vec.as_slice(),
            CowVec::Borrowed(slice) => slice,
        };
        left == right
    }
}

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
    let mut path_str = path.to_str().unwrap();
    if path_str.starts_with("\\\\?\\") {
        path_str = &path_str[4..];
    }
    if !path_str.starts_with(include_directory.to_str().unwrap()) {
        return Err(ResolveFileError::StartsWith(PathBuf::from(path_str)));
    }
    Ok(path)
}

pub struct NoValues;
impl ArgumentSource<Value> for NoValues {
    fn next_argument(&mut self) -> Option<&Value> { None }
    fn lookup_argument_by_index(&self, _: usize) -> Option<&Value> { None }
    fn lookup_argument_by_name(&self, _: &str) -> Option<&Value> { None }
}
