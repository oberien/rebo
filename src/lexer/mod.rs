use std::collections::VecDeque;

use thiserror::Error;
use lexical_core::Error as NumberError;

use crate::span::Span;

mod token;

pub use token::{Tokens, Token, TokenType};
use crate::lexer::token::Radix;

#[derive(Error, Debug)]
pub enum Error {
    #[error("unexpected EOF when parsing double quoted string from {} to {}", .0.start, .0.end)]
    DqStringEof(Span),
    #[error("unexpected EOF at {0}")]
    UnexpectedEof(usize),
    #[error("invalid character in number at {0}: {1:?}")]
    InvalidNumber(usize, NumberError),
}

pub fn lex(s: &str) -> Result<Tokens, Error> {
    trace!("lex");
    let mut index = 0;
    let mut res = VecDeque::new();
    loop {
        match lex_next(s, index)? {
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

pub fn lex_next(s: &str, index: usize) -> Result<Token, Error> {
    trace!("lex_next: {}", index);
    // skip preceding whitespace
    let index = match skip_whitespace(s, index) {
        Some(index) => index,
        None => return Ok(Token::new(Span::new(index, index), TokenType::Eof)),
    };

    if s[index..].is_empty() {
        return Ok(Token::new(Span::new(index, index), TokenType::Eof));
    }

    match try_lex_token(s, index)? {
        Some(token) => return Ok(token),
        None => ()
    }
    match try_lex_number(s, index)? {
        Some(token) => return Ok(token),
        None => ()
    }
    match try_lex_ident(s, index)? {
        Some(token) => return Ok(token),
        None => ()
    }
    Err(Error::UnexpectedEof(index))
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

fn try_lex_token(s: &str, index: usize) -> Result<Option<Token>, Error> {
    trace!("try_lex_token: {}", index);
    if s[index..].starts_with("let") {
        return Ok(Some(Token::new(Span::new(index, index+3), TokenType::Let)));
    }
    if s[index..].starts_with("mut") {
        return Ok(Some(Token::new(Span::new(index, index+3), TokenType::Mut)));
    }
    let char = s[index..].chars().next().unwrap();
    let span = Span::new(index, index + char.len_utf8());
    match char {
        '(' => Ok(Some(Token::new(span, TokenType::OpenParen))),
        ')' => Ok(Some(Token::new(span, TokenType::CloseParen))),
        ';' => Ok(Some(Token::new(span, TokenType::Semicolon))),
        '+' => Ok(Some(Token::new(span, TokenType::Plus))),
        '-' => Ok(Some(Token::new(span, TokenType::Minus))),
        '*' => Ok(Some(Token::new(span, TokenType::Star))),
        '/' => Ok(Some(Token::new(span, TokenType::Slash))),
        ',' => Ok(Some(Token::new(span, TokenType::Comma))),
        '=' => match s[index+1..].chars().next() {
                Some('=') => Ok(Some(Token::new(Span::new(index, index + 2), TokenType::Equals))),
                _ => Ok(Some(Token::new(Span::new(index, index + 1), TokenType::Assign))),
            }
        '"' => lex_double_quoted_string(s, index).map(Some),
        _ => Ok(None),
    }
}

fn try_lex_number(s: &str, mut index: usize) -> Result<Option<Token>, Error> {
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
    let float = lexical_core::parse_partial_radix::<f64>(s[index..].as_bytes(), radix.to_u8());
    let int = lexical_core::parse_partial_radix::<i64>(s[index..].as_bytes(), radix.to_u8());

    match (float, int) {
        (Ok((_f, flen)), Ok((i, ilen))) if flen == ilen =>
            Ok(Some(Token::new(Span::new(start, index + ilen), TokenType::Integer(i, radix)))),
        (Ok((_f, flen)), _) if flen == 0 => Ok(None),
        (Ok((f, flen)), _) =>
            Ok(Some(Token::new(Span::new(start, index + flen), TokenType::Float(f)))),
        (Err(_), Ok((_i, ilen))) if ilen == 0 => Ok(None),
        (Err(_), Ok((i, ilen))) =>
            Ok(Some(Token::new(Span::new(start, index + ilen), TokenType::Integer(i, radix)))),
        (Err(e1), Err(e2)) if e1.index == 0 && e2.index == 0 => Ok(None),
        (Err(e), _) => Err(Error::InvalidNumber(start, e))
    }
}

fn try_lex_ident(s: &str, mut index: usize) -> Result<Option<Token>, Error> {
    trace!("try_lex_ident: {}", index);
    let start = index;
    // first character
    let first = s[index..].chars().next().unwrap();
    match first {
        'a'..='z' | 'A'..='Z' | '_' => index += first.len_utf8(),
        _ => return Ok(None),
    }
    loop {
        match s[index..].chars().next() {
            Some('a'..='z') | Some('A'..='Z') | Some('_') | Some('0'..='9') => index += 1,
            Some(_) | None => return Ok(Some(Token::new(Span::new(start, index), TokenType::Ident(&s[start..index])))),
        }
    }
}

fn lex_double_quoted_string(s: &str, mut index: usize) -> Result<Token, Error> {
    trace!("lex_double_quoted_string: {}", index);
    assert_eq!(s[index..].chars().next(), Some('"'));
    let mut res = String::new();
    let start = index;
    index += 1;
    loop {
        if let Some('"') = s[index..].chars().next() {
            return Ok(Token::new(Span::new(start, index + 1), TokenType::DqString(res)));
        }
        match lex_string_char(s, index, false) {
            None => return Err(Error::DqStringEof(Span::new(start, index))),
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