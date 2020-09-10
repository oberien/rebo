use crate::diagnostics::FileId;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: FileId, start: usize, end: usize) -> Span {
        Span { file, start, end }
    }
}