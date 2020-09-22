#[derive(Debug, Clone, Copy)]
pub enum ErrorCode {
    InternalCompilerError,
    MissingSemicolon,
    UnterminatedString,
    UnexpectedEof,
    InvalidNumber,
    InvalidExpression,
    UnknownIdentifier,
    ImmutableAssign,
    InvalidLetBinding,
    IncompleteLetBinding,
    IncompatibleMathTypes,
    TypeConflict,
    UnableToInferType,
    InvalidArgumentType,
    NotAFunction,
    UnclosedBlock,
    InvalidNumberOfArguments,
    UnclosedParen,
    UnclosedBlockComment,
}
use ErrorCode::*;

impl ErrorCode {
    pub fn code_str(&self) -> &'static str {
        match self {
            InternalCompilerError => "0000",
            MissingSemicolon => "0001",
            UnterminatedString => "0002",
            UnexpectedEof => "0003",
            InvalidNumber => "0004",
            InvalidExpression => "0005",
            UnknownIdentifier => "0006",
            ImmutableAssign => "0007",
            InvalidLetBinding => "0008",
            IncompleteLetBinding => "0009",
            IncompatibleMathTypes => "0010",
            TypeConflict => "0011",
            UnableToInferType => "0012",
            InvalidArgumentType => "0013",
            NotAFunction => "0014",
            UnclosedBlock => "0015",
            InvalidNumberOfArguments => "0016",
            UnclosedParen => "0017",
            UnclosedBlockComment => "0018",
        }
    }

    pub fn message(&self) -> &'static str {
        match self {
            InternalCompilerError => "internal compiler error (please report this)",
            MissingSemicolon => "missing semicolon",
            UnterminatedString => "unterminated double quote string",
            UnexpectedEof => "unexpected end of file",
            InvalidNumber => "invalid number",
            InvalidExpression => "invalid expression",
            UnknownIdentifier => "unknown identifier",
            ImmutableAssign => "assignment to immutable variable",
            InvalidLetBinding => "invalid let binding",
            IncompleteLetBinding => "incomplete let binding",
            IncompatibleMathTypes => "incompatible types for math operation",
            TypeConflict => "type conflict",
            UnableToInferType => "unable to infer type",
            InvalidArgumentType => "invalid argument type",
            NotAFunction => "tried to call a non-function",
            UnclosedBlock => "unclosed block, missing `}`",
            InvalidNumberOfArguments => "invalid number of arguments",
            UnclosedParen => "unclosed parenthesis",
            UnclosedBlockComment => "unclosed block coment",
        }
    }
}
