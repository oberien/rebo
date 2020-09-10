use std::fmt;
use std::cell::RefCell;
use std::io::Write;

use codespan_reporting::files::SimpleFiles;
use codespan_reporting::diagnostic::{Severity, Diagnostic, Label, LabelStyle};
use codespan_reporting::term::{self, Config, termcolor::{StandardStream, ColorChoice}};
use typed_arena::Arena;

mod span;
mod error_codes;

pub use span::Span;
pub use error_codes::ErrorCode;

#[derive(Debug, Clone, Copy)]
pub struct FileId(usize);

pub struct Diagnostics<'a> {
    source_arena: &'a Arena<String>,
    files: RefCell<SimpleFiles<String, &'a str>>,
    stderr: StandardStream,
    config: Config,
}
impl<'a> fmt::Debug for Diagnostics<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Diagnostics")
            .field("source_arena", &"&'a Arena<String>")
            .field("files", &"RefCell<SimpleFiles<String, &'a str>>")
            .field("stderr", &"StandardStream")
            .field("config", &self.config)
            .finish()
    }
}

impl<'a> Diagnostics<'a> {
    pub fn new(source_arena: &'a Arena<String>) -> Diagnostics<'a> {
        Diagnostics {
            source_arena,
            files: RefCell::new(SimpleFiles::new()),
            stderr: StandardStream::stderr(ColorChoice::Auto),
            config: Config::default(),
        }
    }

    pub fn add_file(&self, name: String, source: String) -> (FileId, &'a str) {
        let source = self.source_arena.alloc(source);
        (FileId(self.files.borrow_mut().add(name, source)), source)
    }

    fn diagnostic(&self, severity: Severity, code: ErrorCode, message: String) -> DiagnosticBuilder<'a, '_> {
        DiagnosticBuilder {
            files: &self.files,
            stderr: &self.stderr,
            config: &self.config,
            diagnostics: Vec::new(),
            severity,
            code,
            message,
            labels: Vec::new(),
        }
    }
    pub fn bug<S: Into<String>>(&self, code: ErrorCode, message: S) -> DiagnosticBuilder<'a, '_> {
        let mut diag =
            Some(self.diagnostic(Severity::Bug, code, message.into()).note("please report this"));
        backtrace::trace(|frame| {
            let ip = frame.ip();
            backtrace::resolve(ip, |symbol| {
                diag = Some(diag.take().unwrap().note(format!(
                    "in heradoc file {:?} name {:?} line {:?} address {:?}",
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

    pub fn error(&self, code: ErrorCode) -> DiagnosticBuilder<'a, '_> {
        self.diagnostic(Severity::Error, code, code.message().to_string())
    }

    pub fn warning(&self, code: ErrorCode) -> DiagnosticBuilder<'a, '_> {
        self.diagnostic(Severity::Warning, code, code.message().to_string())
    }

    pub fn note(&self, code: ErrorCode) -> DiagnosticBuilder<'a, '_> {
        self.diagnostic(Severity::Note, code, code.message().to_string())
    }

    pub fn help(&self, code: ErrorCode) -> DiagnosticBuilder<'a, '_> {
        self.diagnostic(Severity::Help, code, code.message().to_string())
    }
}

#[must_use = "call `emit` to emit the diagnostic"]
pub struct DiagnosticBuilder<'a, 'd> {
    files: &'d RefCell<SimpleFiles<String, &'a str>>,
    stderr: &'d StandardStream,
    config: &'d Config,
    diagnostics: Vec<Diagnostic<usize>>,

    severity: Severity,
    code: ErrorCode,
    message: String,
    labels: Vec<Label<usize>>,
}

impl<'a: 'b, 'b> DiagnosticBuilder<'a, 'b> {
    pub fn emit(self) {
        let Self { files, stderr, config, mut diagnostics, severity, message, code, labels } = self;
        diagnostics.push(Diagnostic { severity, message, code: Some(code.code_str().to_string()), labels, notes: Vec::new() });

        let mut stderr = stderr.lock();
        for diagnostic in diagnostics {
            term::emit(&mut stderr, config, &*files.borrow(), &diagnostic)
                .expect("stderr is gone???");
        }
        writeln!(&mut stderr).expect("stderr is gone???");
    }

    fn diagnostic(self, new_severity: Severity, new_message: String) -> Self {
        let Self { files, stderr, config, mut diagnostics, severity, message, code, labels } = self;
        diagnostics.push(Diagnostic { severity, message, code: Some(code.code_str().to_string()), labels, notes: Vec::new() });

        Self {
            files,
            config,
            stderr,
            diagnostics,
            severity: new_severity,
            message: new_message,
            code,
            labels: Vec::new(),
        }
    }

    pub fn bug<S: Into<String>>(self, message: S) -> Self {
        self.diagnostic(Severity::Bug, message.into())
    }

    pub fn error<S: Into<String>>(self, message: S) -> Self {
        self.diagnostic(Severity::Error, message.into())
    }

    pub fn warning<S: Into<String>>(self, message: S) -> Self {
        self.diagnostic(Severity::Warning, message.into())
    }

    pub fn note<S: Into<String>>(self, message: S) -> Self {
        self.diagnostic(Severity::Note, message.into())
    }

    pub fn help<S: Into<String>>(self, message: S) -> Self {
        self.diagnostic(Severity::Help, message.into())
    }

    fn with_section<S: Into<String>>(
        mut self, style: LabelStyle, span: Span, message: S,
    ) -> Self {
        self.labels.push(Label {
            style,
            file_id: span.file.0,
            range: span.start..span.end,
            message: message.into(),
        });
        self
    }

    /// message can be empty
    pub fn with_error_label<S: Into<String>>(self, span: Span, message: S) -> Self {
        self.with_section(LabelStyle::Primary, span, message)
    }

    /// message can be empty
    pub fn with_info_label<S: Into<String>>(self, span: Span, message: S) -> Self {
        self.with_section(LabelStyle::Secondary, span, message)
    }
}

