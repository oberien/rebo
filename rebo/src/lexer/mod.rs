use std::collections::VecDeque;

use crate::diagnostics::{Span, FileId, Diagnostics, ErrorCode};

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

pub fn lex<'i>(diagnostics: &Diagnostics<'_>, file: FileId, s: &'i str) -> Result<Tokens<'i>, Error> {
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

fn lex_next<'i>(diagnostics: &Diagnostics<'_>, file: FileId, s: &'i str, index: usize) -> Result<Token<'i>, Error> {
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

fn try_lex_token<'i>(diagnostics: &Diagnostics<'_>, file: FileId, s: &'i str, index: usize) -> Result<MaybeToken<'i>, Error> {
    trace!("try_lex_token: {}", index);
    if s[index..].starts_with("let") {
        return Ok(MaybeToken::Token(Token::new(Span::new(file, index, index+3), TokenType::Let)));
    }
    if s[index..].starts_with("mut") {
        return Ok(MaybeToken::Token(Token::new(Span::new(file, index, index+3), TokenType::Mut)));
    }
    let char = s[index..].chars().next().unwrap();
    let span = Span::new(file, index, index + char.len_utf8());
    match char {
        '(' => Ok(MaybeToken::Token(Token::new(span, TokenType::OpenParen))),
        ')' => Ok(MaybeToken::Token(Token::new(span, TokenType::CloseParen))),
        ';' => Ok(MaybeToken::Token(Token::new(span, TokenType::Semicolon))),
        '+' => Ok(MaybeToken::Token(Token::new(span, TokenType::Plus))),
        '-' => Ok(MaybeToken::Token(Token::new(span, TokenType::Minus))),
        '*' => Ok(MaybeToken::Token(Token::new(span, TokenType::Star))),
        '/' => Ok(MaybeToken::Token(Token::new(span, TokenType::Slash))),
        ',' => Ok(MaybeToken::Token(Token::new(span, TokenType::Comma))),
        '=' => match s[index+1..].chars().next() {
                Some('=') => Ok(MaybeToken::Token(Token::new(Span::new(file, index, index + 2), TokenType::Equals))),
                _ => Ok(MaybeToken::Token(Token::new(Span::new(file, index, index + 1), TokenType::Assign))),
            }
        '"' => Ok(MaybeToken::Token(lex_double_quoted_string(diagnostics, file, s, index)?)),
        _ => Ok(MaybeToken::Backtrack),
    }
}

fn try_lex_number<'i>(diagnostics: &Diagnostics<'_>, file: FileId, s: &'i str, mut index: usize) -> Result<MaybeToken<'i>, Error> {
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
        .take_while(|&c| !c.is_whitespace() && c != ';')
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

fn try_lex_ident<'i>(_diagnostics: &Diagnostics<'_>, file: FileId, s: &'i str, mut index: usize) -> Result<MaybeToken<'i>, Error> {
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

fn lex_double_quoted_string<'i>(diagnostics: &Diagnostics<'_>, file: FileId, s: &'i str, mut index: usize) -> Result<Token<'i>, Error> {
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