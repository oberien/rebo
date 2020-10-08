use crate::diagnostics::FileId;
use crate::EXTERNAL_SOURCE;
use typed_arena::Index;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
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
            file: FileId(Index::from_raw_parts(0, 0)),
            start: 0,
            end: EXTERNAL_SOURCE.len(),
        }
    }
}