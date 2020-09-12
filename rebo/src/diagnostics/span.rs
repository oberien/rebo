use crate::diagnostics::FileId;
use crate::EXTERNAL_SOURCE;

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
    pub fn external() -> Span {
        Span {
            file: FileId(0),
            start: 0,
            end: EXTERNAL_SOURCE.len(),
        }
    }
}