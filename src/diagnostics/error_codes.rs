#[derive(Debug, Clone, Copy)]
pub enum ErrorCode {
    MissingSemicolon,
    UnterminatedString,
    UnexpectedEof,
    InvalidNumber,
}
use ErrorCode::*;

impl ErrorCode {
    pub fn code_str(&self) -> &'static str {
        match self {
            MissingSemicolon => "0001",
            UnterminatedString => "0002",
            UnexpectedEof => "0003",
            InvalidNumber => "0004",
        }
    }

    pub fn message(&self) -> &'static str {
        match self {
            MissingSemicolon => "missing semicolon",
            UnterminatedString => "unterminated double quote string",
            UnexpectedEof => "unexpected end of file",
            InvalidNumber => "invalid number",
        }
    }
}
