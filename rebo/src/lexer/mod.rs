use std::collections::VecDeque;

use diagnostic::{Span, FileId, Diagnostics};
use crate::error_codes::ErrorCode;

mod token;

pub use token::*;

#[derive(Debug)]
pub enum Error {
    /// The current token is handled by the current function, but it was incorrect in an
    /// unrecoverable way and a diagnostic was emitted. Abort lexing.
    Abort,
}

#[derive(Debug)]
pub enum MaybeToken<'i> {
    /// The current token was correctly parsed by the current function. Continue parsing after
    /// the end of the span of the contained token.
    Token(Token<'i>),
    /// The current token is not a token handled by the current function. Try the next one.
    Backtrack,
    /// The current token is handles by the current function, but there is an error. A diagnostic
    /// was printed and we could recover from it. Continue lexing from the contained offset.
    Diagnostic(usize),
}

pub fn lex<'i>(diagnostics: &'i Diagnostics, file: FileId) -> Result<Tokens<'i>, Error> {
    let source = diagnostics.get_file(file);
    lex_in(diagnostics, file, 0, source.len())
}

pub fn lex_in<'i>(diagnostics: &'i Diagnostics, file: FileId, from: usize, to: usize) -> Result<Tokens<'i>, Error> {
    trace!("lex");
    let source = &diagnostics.get_file(file)[..to];
    let mut index = from;
    let mut res = VecDeque::new();
    loop {
        match lex_next(diagnostics, file, source, index)? {
            token @ Token::Eof(_) => {
                res.push_back(token);
                return Ok(Tokens::new(file, res))
            }
            token => {
                index = token.span().end;
                res.push_back(token);
            }
        }
    }
}

fn lex_next<'i>(diagnostics: &Diagnostics, file: FileId, s: &'i str, index: usize) -> Result<Token<'i>, Error> {
    trace!("lex_next: {}", index);
    // skip preceding whitespace
    let index = match skip_whitespace(s, index) {
        Some(index) => index,
        None => return Ok(Token::Eof(TokenEof { span: Span::new(file, index, index) })),
    };

    if s[index..].is_empty() {
        return Ok(Token::Eof(TokenEof { span: Span::new(file, index, index) }));
    }

    let functions = [
        try_lex_token,
        try_lex_number,
        try_lex_bool,
        try_lex_ident,
    ];

    for f in &functions {
        match f(diagnostics, file, s, index)? {
            MaybeToken::Token(token) => {
                trace!("lexed {:?} as {:?}", &s[token.span().start..token.span().end], token);
                return Ok(token)
            },
            MaybeToken::Backtrack => continue,
            MaybeToken::Diagnostic(cont) => return lex_next(diagnostics, file, s, cont),
        }
    }

    diagnostics.error(ErrorCode::UnexpectedEof)
        .with_error_label(Span::new(file, index, s.len()), "this expression is not complete")
        .emit();
    Err(Error::Abort)
}

fn skip_whitespace(s: &str, mut index: usize) -> Option<usize> {
    loop {
        match s[index..].chars().next() {
            None => return None,
            Some(c) if c.is_whitespace() => index += 1,
            Some(_) => break Some(index),
        }
    }
}

fn try_lex_token<'i>(diagnostics: &Diagnostics, file: FileId, s: &'i str, index: usize) -> Result<MaybeToken<'i>, Error> {
    trace!("try_lex_token: {}", index);
    if s[index..].starts_with("let") {
        return Ok(MaybeToken::Token(Token::Let(TokenLet { span: Span::new(file, index, index+3) })));
    }
    if s[index..].starts_with("mut") {
        return Ok(MaybeToken::Token(Token::Mut(TokenMut { span: Span::new(file, index, index+3) })));
    }
    if s[index..].starts_with("fn") {
        return Ok(MaybeToken::Token(Token::Fn(TokenFn { span: Span::new(file, index, index+2) })));
    }
    if s[index..].starts_with("struct") {
        return Ok(MaybeToken::Token(Token::Struct(TokenStruct { span: Span::new(file, index, index+6) })));
    }
    if s[index..].starts_with("if") {
        return Ok(MaybeToken::Token(Token::If(TokenIf { span: Span::new(file, index, index+2) })));
    }
    if s[index..].starts_with("while") {
        return Ok(MaybeToken::Token(Token::While(TokenWhile { span: Span::new(file, index, index+5) })));
    }
    if s[index..].starts_with("else") {
        return Ok(MaybeToken::Token(Token::Else(TokenElse { span: Span::new(file, index, index+4) })));
    }
    if s[index..].starts_with("string") {
        return Ok(MaybeToken::Token(Token::StringType(TokenStringType { span: Span::new(file, index, index+6) })));
    }
    if s[index..].starts_with("int") {
        return Ok(MaybeToken::Token(Token::IntType(TokenIntType { span: Span::new(file, index, index+3) })));
    }
    if s[index..].starts_with("float") {
        return Ok(MaybeToken::Token(Token::FloatType(TokenFloatType { span: Span::new(file, index, index+5) })));
    }
    if s[index..].starts_with("bool") {
        return Ok(MaybeToken::Token(Token::BoolType(TokenBoolType { span: Span::new(file, index, index+4) })));
    }
    let char = s[index..].chars().next().unwrap();
    let span = Span::new(file, index, index + char.len_utf8());
    let char2 = s[index+char.len_utf8()..].chars().next();
    let span2 = Span::new(file, index, index + char.len_utf8() + char2.map(|c| c.len_utf8()).unwrap_or_default());
    match char {
        '(' => Ok(MaybeToken::Token(Token::OpenParen(TokenOpenParen { span }))),
        ')' => Ok(MaybeToken::Token(Token::CloseParen(TokenCloseParen { span }))),
        '{' => Ok(MaybeToken::Token(Token::OpenCurly(TokenOpenCurly { span }))),
        '}' => Ok(MaybeToken::Token(Token::CloseCurly(TokenCloseCurly { span }))),
        ';' => Ok(MaybeToken::Token(Token::Semicolon(TokenSemicolon { span }))),
        ':' => Ok(MaybeToken::Token(Token::Colon(TokenColon { span }))),
        '+' => Ok(MaybeToken::Token(Token::Plus(TokenPlus { span }))),
        '-' => match char2 {
            Some('>') => Ok(MaybeToken::Token(Token::Arrow(TokenArrow { span: span2 }))),
            _ => Ok(MaybeToken::Token(Token::Minus(TokenMinus { span }))),
        }
        '*' => Ok(MaybeToken::Token(Token::Star(TokenStar { span }))),
        '/' => match char2 {
            Some('/') => Ok(MaybeToken::Token(lex_line_comment(diagnostics, file, s, index))),
            Some('*') => Ok(MaybeToken::Token(lex_block_comment(diagnostics, file, s, index))),
            _ => Ok(MaybeToken::Token(Token::Slash(TokenSlash { span }))),
        }
        ',' => Ok(MaybeToken::Token(Token::Comma(TokenComma { span }))),
        '=' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::Equals(TokenEquals { span: span2 }))),
            _ => Ok(MaybeToken::Token(Token::Assign(TokenAssign { span }))),
        }
        '>' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::GreaterEquals(TokenGreaterEquals { span: span2 }))),
            _ => Ok(MaybeToken::Token(Token::GreaterThan(TokenGreaterThan { span }))),
        }
        '<' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::LessEquals(TokenLessEquals { span: span2 }))),
            _ => Ok(MaybeToken::Token(Token::LessThan(TokenLessThan { span }))),
        }
        '!' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::NotEquals(TokenNotEquals { span: span2 }))),
            _ => Ok(MaybeToken::Token(Token::Bang(TokenBang { span }))),
        }
        '&' => match char2 {
            Some('&') => Ok(MaybeToken::Token(Token::DoubleAmp(TokenDoubleAmp { span: span2 }))),
            _ => Ok(MaybeToken::Token(Token::Amp(TokenAmp { span }))),
        }
        '|' => match char2 {
            Some('|') => Ok(MaybeToken::Token(Token::DoublePipe(TokenDoublePipe { span: span2 }))),
            _ => Ok(MaybeToken::Token(Token::Pipe(TokenPipe { span }))),
        }
        '.' => Ok(MaybeToken::Token(Token::Dot(TokenDot { span }))),
        '"' => Ok(MaybeToken::Token(lex_double_quoted_string(diagnostics, file, s, index)?)),
        _ => Ok(MaybeToken::Backtrack),
    }
}

fn try_lex_number<'i>(diagnostics: &Diagnostics, file: FileId, s: &'i str, mut index: usize) -> Result<MaybeToken<'i>, Error> {
    trace!("try_lex_number: {}", index);
    let start = index;
    let mut radix = Radix::Dec;
    if s[index..].starts_with("0b") {
        index += 2;
        radix = Radix::Bin;
    } else if s[index..].starts_with("0x") {
        index += 2;
        radix = Radix::Hex;
    }
    let number_end = index + s[index..].chars()
        .take_while(|&c| c.is_alphanumeric() || c == '.')
        .map(char::len_utf8)
        .sum::<usize>();
    let int = lexical::parse_radix::<i64, _>(&s[index..number_end], radix.to_u8());
    let float = lexical::parse_radix::<f64, _>(&s[index..number_end], radix.to_u8());

    match (int, float) {
        (Ok(i), _) => Ok(MaybeToken::Token(Token::Integer(TokenInteger {
            span: Span::new(file, start, number_end),
            value: i,
            radix,
        }))),
        (_, Ok(f)) => Ok(MaybeToken::Token(Token::Float(TokenFloat {
            span: Span::new(file, start, number_end),
            value: f,
            radix,
        }))),
        (Err(e), _) if e.index == 0 && radix == Radix::Dec => Ok(MaybeToken::Backtrack),
        (Err(e), _) => {
            diagnostics.error(ErrorCode::InvalidNumber)
                .with_error_label(Span::new(file, start, number_end), format!("{:?}", e.code))
                .emit();
            Ok(MaybeToken::Diagnostic(number_end))
        }
    }
}

fn try_lex_bool<'i>(_diagnostics: &Diagnostics, file: FileId, s: &'i str, index: usize) -> Result<MaybeToken<'i>, Error> {
    trace!("try_lex_bool: {}", index);
    if s[index..].starts_with("true") {
        Ok(MaybeToken::Token(Token::Bool(TokenBool {
            span: Span::new(file, index, index + 4),
            value: true,
        })))
    } else if s[index..].starts_with("false") {
        Ok(MaybeToken::Token(Token::Bool(TokenBool {
            span: Span::new(file, index, index + 5),
            value: false,
        })))
    } else {
        Ok(MaybeToken::Backtrack)
    }
}

fn try_lex_ident<'i>(_diagnostics: &Diagnostics, file: FileId, s: &'i str, mut index: usize) -> Result<MaybeToken<'i>, Error> {
    trace!("try_lex_ident: {}", index);
    let start = index;
    // first character
    let first = s[index..].chars().next().unwrap();
    match first {
        'a'..='z' | 'A'..='Z' | '_' => index += first.len_utf8(),
        _ => return Ok(MaybeToken::Backtrack),
    }
    loop {
        match s[index..].chars().next() {
            Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') => index += 1,
            Some(_) | None => return Ok(MaybeToken::Token(Token::Ident(TokenIdent {
                span: Span::new(file, start, index),
                ident: &s[start..index]
            }))),
        }
    }
}

fn lex_line_comment<'i>(_diagnostics: &Diagnostics, file: FileId, s: &'i str, index: usize) -> Token<'i> {
    trace!("lex_line_comment: {}", index);
    assert_eq!(s[index..].chars().next(), Some('/'));
    assert_eq!(s[index+1..].chars().next(), Some('/'));
    let mut end = index + 2;
    loop {
        match s[end..].chars().next() {
            Some('\n') => {
                end += 1;
                break
            },
            Some(_) => end += 1,
            None => break,
        }
    }
    Token::LineComment(TokenLineComment {
        span: Span::new(file, index, end),
        comment: &s[index..end],
    })
}
fn lex_block_comment<'i>(diagnostics: &Diagnostics, file: FileId, s: &'i str, index: usize) -> Token<'i> {
    trace!("lex_block_comment: {}", index);
    assert_eq!(s[index..].chars().next(), Some('/'));
    assert_eq!(s[index+1..].chars().next(), Some('*'));
    let warn = |start, end| {
        diagnostics.warning(ErrorCode::UnclosedBlockComment)
            .with_info_label(Span::new(file, start, end), "")
            .with_info_label(Span::new(file, end, end), "try inserting `*/` here")
            .emit();
        Token::BlockComment(TokenBlockComment {
            span: Span::new(file, start, end),
            comment: &s[start..end],
        })
    };
    let mut end = index + 2;
    let mut depth = 0;
    loop {
        let c1 = match s[end..].chars().next() {
            Some(c) => c,
            None => return warn(index, end),
        };
        end += c1.len_utf8();
        let c2 = match s[end..].chars().next() {
            Some(c) => c,
            None => return warn(index, end),
        };
        match (c1, c2) {
            ('*', '/') if depth == 0 => {
                end += c2.len_utf8();
                return Token::BlockComment(TokenBlockComment {
                    span: Span::new(file, index, end),
                    comment: &s[index..end],
                });
            },
            ('*', '/') => depth -= 1,
            ('/', '*') => depth += 1,
            _ => (),
        }
    }
}

fn lex_double_quoted_string<'i>(diagnostics: &Diagnostics, file: FileId, s: &'i str, mut index: usize) -> Result<Token<'i>, Error> {
    trace!("lex_double_quoted_string: {}", index);
    assert_eq!(s[index..].chars().next(), Some('"'));
    let mut res = String::new();
    let start = index;
    index += 1;
    loop {
        if let Some('"') = s[index..].chars().next() {
            return Ok(Token::DqString(TokenDqString {
                span: Span::new(file, start, index + 1),
                string: res,
            }));
        }
        match lex_string_char(s, index, false) {
            None => {
                diagnostics.error(ErrorCode::UnterminatedString)
                    .with_error_label(Span::new(file, start, index), "this string is unterminated")
                    .with_info_label(Span::new(file, index, index), "try adding a closing `\"`")
                    .emit();
                return Err(Error::Abort)
            },
            Some((c, idx)) => {
                res.push(c);
                index = idx;
            }
        }
    }
}

fn lex_string_char(s: &str, index: usize, escaped: bool) -> Option<(char, usize)> {
    let next = s[index..].chars().next()?;

    match next {
        '\\' if !escaped => lex_string_char(s, index + 1, true),
        c => Some((c, index + c.len_utf8()))
    }
}