#[derive(Debug, Clone, Copy)]
pub enum ErrorCode {
    MissingSemicolon,
    UnterminatedString,
    UnexpectedEof,
    InvalidNumber,
    InvalidExpression,
    UnknownIdentifier,
    ImmutableAssign,
    InvalidLetBinding,
    IncompleteLetBinding,
}
use ErrorCode::*;

impl ErrorCode {
    pub fn code_str(&self) -> &'static str {
        match self {
            MissingSemicolon => "0001",
            UnterminatedString => "0002",
            UnexpectedEof => "0003",
            InvalidNumber => "0004",
            InvalidExpression => "0005",
            UnknownIdentifier => "0006",
            ImmutableAssign => "0007",
            InvalidLetBinding => "0008",
            IncompleteLetBinding => "0009",
        }
    }

    pub fn message(&self) -> &'static str {
        match self {
            MissingSemicolon => "missing semicolon",
            UnterminatedString => "unterminated double quote string",
            UnexpectedEof => "unexpected end of file",
            InvalidNumber => "invalid number",
            InvalidExpression => "invalid expression",
            UnknownIdentifier => "unknown identifier",
            ImmutableAssign => "assignment to immutable variable",
            InvalidLetBinding => "invalid let binding",
            IncompleteLetBinding => "incomplete let binding",
        }
    }
}
