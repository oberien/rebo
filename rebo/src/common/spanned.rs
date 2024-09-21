use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::BitOr;
use std::sync::atomic::{AtomicU32, Ordering};
use diagnostic::{FileId, Span};

pub trait Spanned {
    fn span_with_id(&self) -> SpanWithId;
    fn diagnostics_span(&self) -> Span {
        self.span_with_id().diagnostics_span()
    }
    fn file_id(&self) -> FileId {
        self.diagnostics_span().file
    }
    fn start(&self) -> usize {
        self.diagnostics_span().start
    }
    fn end(&self) -> usize {
        self.diagnostics_span().end
    }
}

impl<'a, T: Spanned> Spanned for &'a T {
    fn span_with_id(&self) -> SpanWithId {
        <T as Spanned>::span_with_id(self)
    }
}
impl Spanned for SpanWithId {
    fn span_with_id(&self) -> SpanWithId {
        *self
    }
}

/// ID uniquely identifying a span (and thus (sub-)expression).
///
/// `a + b` has 4 SpanIds: `a`, `+`, `b`, `a + b`
///
/// A span may exist multiple times for different reasons. For example when
/// generating code from existing code (e.g. for generators), a source-code span
/// may be used for multiple generated expressions. As Spans are used for identifying
/// `TypeId`s and other analyses, each usage of a span must be unique.
/// This is where the SpanId comes in. Even if a span is used for 2 different expressions,
/// they'll have different SpanIds, making each expression have a unique SpanId.
///
/// Neither SpanIds nor Spans aren't directly used within the rebo compiler.
/// Instead, `SpanWithId` is used. The constructor of is private and only used
/// within `SpanWithId::new`.
// Historically, types were inferred by their spans in the code. Take the following code:
// ```rust
//           1111111111122222
// 01234567890123456789012345
// list.get(1).unwrap().value
// ```
// In the type-checker there are the following types:
// * 0-4 `list`: must be a struct with a `get` function
// * 5-8 `get`: must be a function taking the `list` as first argument and an  int as second argument
// * 9-10 `1`: must be an int; must be the same as the second argument of `get`
// * 5-10 `get(1)`: must be a struct with an `unwrap` function
// * 0-10 `list.get(1)`: must be the same as `5-10`
// * 12-18 `unwrap`: must be a function taking `list.get(1)` as only argument
// * 12-20 `unwrap()`: must be a struct with a field called `value`
// * 0-20 `list.get(1).unwrap()` must be the same as `12-20`
// * 21-26 `value`: can be of any type depending on its usage
// * 0-26: must be the same as `21-26`
//
// However, there's a problem with this approach when it comes to the source-code transform
// performed by generators.
// ```rust
// let mut a = Foo { x: 5 };
// print(a.x);
// ```
// That code needs to be transformed to this:
// ```rust
// self.a = Option::Some(Foo { x: 5 });
// print(self.a.unwrap().x);
// ```
// The problem is the transformation from `a.x` to `self.a.unwrap().x`.
// There aren't enough unique spans in `a.x` to represent all required spans in `self.a.unwrap().x`.
// All of the following expressions require a unique span for typeck to work correctly:
// `self`, `a`, `self.a`, `unwrap`, `unwrap()`, `self.a.unwrap()`, `x` , `self.a.unwrap().x`.
// There are 5 unique spans (i.e. not combinations of other spans):
// `self`, `a`, `unwrap`, `unwrap()` and `x`.
// However, `a.x` only has 0 through 3, so I can only get the unique spans `0-1`, `1-2` and `2-3`.
// That's not enough for the 5 unique spans that are required.
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SpanId(u32);

impl Display for SpanId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A span used within the rebo compiler.
///
/// The raw `Span` type is only used with the `diagnostics` library.
/// The `SpanId` uniquely identifies the span, even when there are two expressions
/// sharing the same span (e.g. in autogenerated code).
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
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
    pub fn diagnostics_span(self) -> Span {
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
