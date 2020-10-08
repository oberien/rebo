#![allow(dead_code)]
use std::fmt;
use std::cell::RefCell;
use std::ops::Range;

use codespan_reporting::files::{SimpleFile, Files, Location};
use codespan_reporting::diagnostic::{Severity, Diagnostic, Label, LabelStyle};
use codespan_reporting::term::{self, Config, termcolor::{StandardStream, ColorChoice}};
use typed_arena::{ImmutableArena, Index};

mod span;
mod error_codes;

pub use span::Span;
pub use error_codes::ErrorCode;

// codespan_reporting::SimpleFiles requires `&mut self` when adding a file,
// making it impossible to have an immutable interface to put a string in it
// and return an immutable &str.
// Thus, here we implement the same thing, but using an Arena instead of a Vec, to allow
// an immutable interface.
struct EvenSimplerFiles {
    files: ImmutableArena<SimpleFile<String, String>>,
}
impl EvenSimplerFiles {
    pub fn new() -> Self {
        EvenSimplerFiles {
            files: ImmutableArena::new(),
        }
    }
    pub fn add(&self, name: String, source: String) -> Index {
        self.files.alloc(SimpleFile::new(name, source))
    }
    pub fn get(&self, idx: Index) -> &SimpleFile<String, String> {
        self.files.get(idx)
    }
}
impl<'a> Files<'a> for EvenSimplerFiles {
    type FileId = Index;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Option<Self::Name> {
        Some(&self.files.get(id).name())
    }

    fn source(&'a self, id: Self::FileId) -> Option<Self::Source> {
        Some(&self.files.get(id).source())
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Option<usize> {
        self.files.get(id).line_index((), byte_index)
    }

    fn line_number(&'a self, id: Self::FileId, line_index: usize) -> Option<usize> {
        self.files.get(id).line_number((), line_index)
    }

    fn column_number(&'a self, id: Self::FileId, line_index: usize, byte_index: usize) -> Option<usize> {
        self.files.get(id).column_number((), line_index, byte_index)
    }

    fn location(&'a self, id: Self::FileId, byte_index: usize) -> Option<Location> {
        self.files.get(id).location((), byte_index)
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Option<Range<usize>> {
        self.files.get(id).line_range((), line_index)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct FileId(Index);

pub struct Diagnostics {
    files: EvenSimplerFiles,
    stderr: StandardStream,
    config: Config,
    error_printed: RefCell<bool>,
}
impl fmt::Debug for Diagnostics {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Diagnostics")
            .field("source_arena", &"&'i Arena<String>")
            .field("files", &"RefCell<SimpleFiles<String, &'i str>>")
            .field("stderr", &"StandardStream")
            .field("config", &self.config)
            .finish()
    }
}

impl Diagnostics {
    pub fn new() -> Diagnostics {
        Diagnostics {
            files: EvenSimplerFiles::new(),
            stderr: StandardStream::stderr(ColorChoice::Auto),
            config: Config::default(),
            error_printed: RefCell::new(false),
        }
    }

    pub fn add_file(&self, name: String, source: String) -> (FileId, &str) {
        let idx = self.files.add(name, source);
        let source = self.files.get(idx).source();
        (FileId(idx), source)
    }

    pub fn error_printed(&self) -> bool {
        *self.error_printed.borrow()
    }

    fn diagnostic(&self, severity: Severity, code: ErrorCode, message: String) -> DiagnosticBuilder<'_> {
        DiagnosticBuilder {
            files: &self.files,
            stderr: &self.stderr,
            config: &self.config,
            severity,
            code,
            message,
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }
    pub fn bug<S: Into<String>>(&self, code: ErrorCode, message: S) -> DiagnosticBuilder<'_> {
        *self.error_printed.borrow_mut() = true;
        let mut diag =
            Some(self.diagnostic(Severity::Bug, code, message.into()).with_note("please report this"));
        backtrace::trace(|frame| {
            let ip = frame.ip();
            backtrace::resolve(ip, |symbol| {
                diag = Some(diag.take().unwrap().with_note(format!(
                    "in file {:?} name {:?} line {:?} address {:?}",
                    symbol.filename(),
                    symbol.name(),
                    symbol.lineno(),
                    symbol.addr()
                )));
            });
            true
        });
        diag.unwrap()
    }

    pub fn error(&self, code: ErrorCode) -> DiagnosticBuilder<'_> {
        *self.error_printed.borrow_mut() = true;
        self.diagnostic(Severity::Error, code, code.message().to_string())
    }

    pub fn warning(&self, code: ErrorCode) -> DiagnosticBuilder<'_> {
        self.diagnostic(Severity::Warning, code, code.message().to_string())
    }

    pub fn note(&self, code: ErrorCode) -> DiagnosticBuilder<'_> {
        self.diagnostic(Severity::Note, code, code.message().to_string())
    }

    pub fn help(&self, code: ErrorCode) -> DiagnosticBuilder<'_> {
        self.diagnostic(Severity::Help, code, code.message().to_string())
    }
}

#[must_use = "call `emit` to emit the diagnostic"]
pub struct DiagnosticBuilder<'d> {
    files: &'d EvenSimplerFiles,
    stderr: &'d StandardStream,
    config: &'d Config,

    severity: Severity,
    code: ErrorCode,
    message: String,
    labels: Vec<Label<Index>>,
    notes: Vec<String>,
}

impl<'d> DiagnosticBuilder<'d> {
    pub fn emit(self) {
        let Self { files, stderr, config, severity, message, code, labels, notes } = self;
        let diagnostic = Diagnostic { severity, message, code: Some(code.code_str().to_string()), labels, notes };

        let mut stderr = stderr.lock();
        term::emit(&mut stderr, config, files, &diagnostic)
            .expect("stderr is gone???");
        // writeln!(&mut stderr).expect("stderr is gone???");
    }

    fn with_label<S: Into<String>>(mut self, style: LabelStyle, span: Span, message: S) -> Self {
        self.labels.push(Label {
            style,
            file_id: span.file.0,
            range: span.start..span.end,
            message: message.into(),
        });
        self
    }

    pub fn with_error_label<S: Into<String>>(self, span: Span, message: S) -> Self {
        self.with_label(LabelStyle::Primary, span, message)
    }

    pub fn with_info_label<S: Into<String>>(self, span: Span, message: S) -> Self {
        self.with_label(LabelStyle::Secondary, span, message)
    }

    pub fn with_note<S: Into<String>>(mut self, message: S) -> Self {
        self.notes.push(message.into());
        self
    }
}

