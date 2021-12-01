use std::fmt;
use std::ops::Deref;
use std::path::{Path, PathBuf};

/// Workaround for <https://github.com/rust-lang/rust/issues/89940>
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
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
pub fn try_resolve_file<P: AsRef<Path>, P2: AsRef<Path>>(include_directory: P, file: P2) -> Result<PathBuf, ResolveFileError> {
    let path = include_directory.as_ref().join(file.as_ref());
    let path = match path.canonicalize() {
        Ok(path) => path,
        Err(e) => {
            return Err(ResolveFileError::Canonicalize(path, e));
        }
    };
    if !path.starts_with(include_directory.as_ref()) {
        return Err(ResolveFileError::StartsWith(path));
    }
    Ok(path)
}
