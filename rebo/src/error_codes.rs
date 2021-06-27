#[derive(Debug, Clone, Copy)]
pub enum ErrorCode {
    // InternalCompilerError,
    MissingSemicolon,
    UnterminatedString,
    UnexpectedEof,
    InvalidNumber,
    InvalidExpression,
    UnknownIdentifier,
    ImmutableAssign,
    InvalidLetBinding,
    IncompleteLetBinding,
    TypeConflict,
    UnableToInferType,
    NotAFunction,
    UnclosedBlock,
    InvalidNumberOfArguments,
    UnclosedParen,
    UnclosedBlockComment,
    EmptyFunctionBody,
    DuplicateGlobal,
}
use ErrorCode::*;

impl diagnostic::ErrorCode for ErrorCode {
    fn code(&self) -> String {
        match self {
            // InternalCompilerError => "0000",
            MissingSemicolon => "0001",
            UnterminatedString => "0002",
            UnexpectedEof => "0003",
            InvalidNumber => "0004",
            InvalidExpression => "0005",
            UnknownIdentifier => "0006",
            ImmutableAssign => "0007",
            InvalidLetBinding => "0008",
            IncompleteLetBinding => "0009",
            TypeConflict => "0011",
            UnableToInferType => "0012",
            NotAFunction => "0014",
            UnclosedBlock => "0015",
            InvalidNumberOfArguments => "0016",
            UnclosedParen => "0017",
            UnclosedBlockComment => "0018",
            EmptyFunctionBody => "0019",
            DuplicateGlobal => "0020",
        }.to_string()
    }

    fn message(&self) -> String {
        match self {
            // InternalCompilerError => "internal compiler error (please report this)",
            MissingSemicolon => "missing semicolon",
            UnterminatedString => "unterminated double quote string",
            UnexpectedEof => "unexpected end of file",
            InvalidNumber => "invalid number",
            InvalidExpression => "invalid expression",
            UnknownIdentifier => "unknown identifier",
            ImmutableAssign => "assignment to immutable variable",
            InvalidLetBinding => "invalid let binding",
            IncompleteLetBinding => "incomplete let binding",
            TypeConflict => "type conflict",
            UnableToInferType => "unable to infer type",
            NotAFunction => "tried to call a non-function",
            UnclosedBlock => "unclosed block, missing `}`",
            InvalidNumberOfArguments => "invalid number of arguments",
            UnclosedParen => "unclosed parenthesis",
            UnclosedBlockComment => "unclosed block comment",
            EmptyFunctionBody => "empty function body",
            DuplicateGlobal => "conflicting duplicate global definition",
        }.to_string()
    }
}
