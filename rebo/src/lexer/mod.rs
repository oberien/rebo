use std::collections::VecDeque;

use diagnostic::{Span, FileId, Diagnostics};
use crate::error_codes::ErrorCode;

mod token;

pub use token::{Tokens, Token, TokenType, Radix};

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

pub fn lex<'i>(diagnostics: &Diagnostics, file: FileId, s: &'i str) -> Result<Tokens<'i>, Error> {
    trace!("lex");
    let mut index = 0;
    let mut res = VecDeque::new();
    loop {
        match lex_next(diagnostics, file, s, index)? {
            token @ Token { typ: TokenType::Eof, .. } => {
                res.push_back(token);
                return Ok(Tokens::new(res))
            }
            token => {
                index = token.span.end;
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
        None => return Ok(Token::new(Span::new(file, index, index), TokenType::Eof)),
    };

    if s[index..].is_empty() {
        return Ok(Token::new(Span::new(file, index, index), TokenType::Eof));
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
                trace!("lexed {:?} as {:?}", &s[token.span.start..token.span.end], token.typ);
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
        return Ok(MaybeToken::Token(Token::new(Span::new(file, index, index+3), TokenType::Let)));
    }
    if s[index..].starts_with("mut") {
        return Ok(MaybeToken::Token(Token::new(Span::new(file, index, index+3), TokenType::Mut)));
    }
    let char = s[index..].chars().next().unwrap();
    let span = Span::new(file, index, index + char.len_utf8());
    let char2 = s[index+char.len_utf8()..].chars().next();
    let span2 = Span::new(file, index, index + char.len_utf8() + char2.map(|c| c.len_utf8()).unwrap_or_default());
    match char {
        '(' => Ok(MaybeToken::Token(Token::new(span, TokenType::OpenParen))),
        ')' => Ok(MaybeToken::Token(Token::new(span, TokenType::CloseParen))),
        '{' => Ok(MaybeToken::Token(Token::new(span, TokenType::OpenCurly))),
        '}' => Ok(MaybeToken::Token(Token::new(span, TokenType::CloseCurly))),
        ';' => Ok(MaybeToken::Token(Token::new(span, TokenType::Semicolon))),
        '+' => Ok(MaybeToken::Token(Token::new(span, TokenType::Plus))),
        '-' => Ok(MaybeToken::Token(Token::new(span, TokenType::Minus))),
        '*' => Ok(MaybeToken::Token(Token::new(span, TokenType::Star))),
        '/' => match char2 {
            Some('/') => Ok(MaybeToken::Token(lex_line_comment(file, s, index))),
            Some('*') => Ok(MaybeToken::Token(lex_block_comment(diagnostics, file, s, index))),
            _ => Ok(MaybeToken::Token(Token::new(span, TokenType::Slash))),
        }
        ',' => Ok(MaybeToken::Token(Token::new(span, TokenType::Comma))),
        '=' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::new(span2, TokenType::Equals))),
            _ => Ok(MaybeToken::Token(Token::new(span, TokenType::Assign))),
        }
        '~' => match char2 {
            Some('~') => Ok(MaybeToken::Token(Token::new(span2, TokenType::FloatEquals))),
            _ => Ok(MaybeToken::Backtrack),
        }
        '>' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::new(span2, TokenType::GreaterEquals))),
            _ => Ok(MaybeToken::Token(Token::new(span, TokenType::GreaterThan))),
        }
        '<' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::new(span2, TokenType::LessEquals))),
            _ => Ok(MaybeToken::Token(Token::new(span, TokenType::LessThan))),
        }
        '!' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::new(span2, TokenType::NotEquals))),
            Some('~') => Ok(MaybeToken::Token(Token::new(span2, TokenType::FloatNotEquals))),
            _ => Ok(MaybeToken::Token(Token::new(span, TokenType::Exclamation))),
        }
        '&' => match char2 {
            Some('&') => Ok(MaybeToken::Token(Token::new(span2, TokenType::DoubleAmp))),
            _ => Ok(MaybeToken::Token(Token::new(span, TokenType::Amp))),
        }
        '|' => match char2 {
            Some('|') => Ok(MaybeToken::Token(Token::new(span2, TokenType::DoublePipe))),
            _ => Ok(MaybeToken::Token(Token::new(span, TokenType::Pipe))),
        }
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
        (Ok(i), _) => Ok(MaybeToken::Token(Token::new(Span::new(file, start, number_end), TokenType::Integer(i, radix)))),
        (_, Ok(f)) => Ok(MaybeToken::Token(Token::new(Span::new(file, start, number_end), TokenType::Float(f, radix)))),
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
        Ok(MaybeToken::Token(Token::new(Span::new(file, index, index+4), TokenType::Bool(true))))
    } else if s[index..].starts_with("false") {
        Ok(MaybeToken::Token(Token::new(Span::new(file, index, index+5), TokenType::Bool(false))))
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
            Some('a'..='z') | Some('A'..='Z') | Some('_') | Some('0'..='9') => index += 1,
            Some(_) | None => return Ok(MaybeToken::Token(Token::new(Span::new(file, start, index), TokenType::Ident(&s[start..index])))),
        }
    }
}

fn lex_line_comment<'i>(file: FileId, s: &'i str, index: usize) -> Token<'i> {
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
    Token::new(Span::new(file, index, end), TokenType::LineComment(&s[index..end]))
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
        Token::new(Span::new(file, start, end), TokenType::BlockComment(&s[start..end]))
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
                return Token::new(Span::new(file, index, end), TokenType::BlockComment(&s[index..end]))
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
            return Ok(Token::new(Span::new(file, start, index + 1), TokenType::DqString(res)));
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