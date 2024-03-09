use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::BitOr;
use std::sync::atomic::{AtomicU32, Ordering};
use diagnostic::{FileId, Span};

pub trait Spanned {
    fn span_with_id(&self) -> SpanWithId;
    fn span_(&self) -> Span {
        self.span_with_id().span()
    }
    fn span_id(&self) -> SpanId {
        self.span_with_id().id()
    }
}

impl<'a, T: Spanned> Spanned for &'a T {
    fn span_with_id(&self) -> SpanWithId {
        <T as Spanned>::span_with_id(self)
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SpanId(u32);
impl Display for SpanId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SpanWithId {
    id: SpanId,
    span: Span,
}
impl SpanWithId {
    /// Create a new Span with a new unique SpanId
    pub fn new(file: FileId, start: usize, end: usize) -> SpanWithId {
        static NEXT_SPAN_ID: AtomicU32 = AtomicU32::new(0);
        let id = NEXT_SPAN_ID.fetch_add(1, Ordering::SeqCst);
        // why do we even have this check?!
        if id == u32::MAX {
            panic!("binding id overflow");
        }
        SpanWithId { id: SpanId(id), span: Span::new(file, start, end) }
    }
    /// Create a new SpanWithId with a unique SpanId for just the start position of this Span.
    pub fn start_span(self) -> SpanWithId {
        SpanWithId::new(self.span.file, self.span.start, self.span.start)
    }
    /// Create a new SpanWithId with a unique SpanId for just the end position of this Span.
    pub fn end_span(self) -> SpanWithId {
        SpanWithId::new(self.span.file, self.span.end, self.span.end)
    }
    pub fn id(self) -> SpanId {
        self.id
    }
    pub fn span(self) -> Span {
        self.span
    }
}
impl From<Span> for SpanWithId {
    fn from(span: Span) -> Self {
        SpanWithId::new(span.file, span.start, span.end)
    }
}

// SpanWithId | value
impl BitOr<SpanWithId> for SpanWithId {
    type Output = SpanWithId;
    fn bitor(self, rhs: SpanWithId) -> Self::Output {
        assert_eq!(self.span.file, rhs.span.file);
        SpanWithId::new(self.span.file, self.span.start, rhs.span.end)
    }
}
impl BitOr<Span> for SpanWithId {
    type Output = SpanWithId;
    fn bitor(self, rhs: Span) -> Self::Output {
        assert_eq!(self.span.file, rhs.file);
        SpanWithId::new(self.span.file, self.span.start, rhs.end)
    }
}
impl BitOr<usize> for SpanWithId {
    type Output = SpanWithId;
    fn bitor(self, rhs: usize) -> Self::Output {
        SpanWithId::new(self.span.file, self.span.start, rhs)
    }
}
impl<T> BitOr<Option<T>> for SpanWithId
    where SpanWithId: BitOr<T, Output = SpanWithId>
{
    type Output = SpanWithId;
    fn bitor(self, rhs: Option<T>) -> Self::Output {
        match rhs {
            Some(rhs) => self | rhs,
            None => self,
        }
    }
}

// value | SpanWithId
impl BitOr<SpanWithId> for Span {
    type Output = SpanWithId;
    fn bitor(self, rhs: SpanWithId) -> Self::Output {
        assert_eq!(self.file, rhs.span.file);
        SpanWithId::new(self.file, self.start, rhs.span.end)
    }
}
impl BitOr<SpanWithId> for usize {
    type Output = SpanWithId;
    fn bitor(self, rhs: SpanWithId) -> Self::Output {
        SpanWithId::new(rhs.span.file, self, rhs.span.end)
    }
}
impl<T> BitOr<SpanWithId> for Option<T>
    where T: BitOr<SpanWithId, Output = SpanWithId>
{
    type Output = SpanWithId;
    fn bitor(self, rhs: SpanWithId) -> Self::Output {
        match self {
            Some(val) => val | rhs,
            None => rhs,
        }
    }
}
