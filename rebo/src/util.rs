use std::fmt;

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

pub fn similar_name<'i>(ident: &str, others: impl IntoIterator<Item = &'i str>) -> Option<&'i str> {
    others.into_iter()
        .map(|s| (strsim::levenshtein(ident, s), s))
        // .filter(|&(dist, _)| dist <= 3)
        .min_by_key(|&(dist, _)| dist)
        .map(|(_, s)| s)
}
