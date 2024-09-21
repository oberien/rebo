use crate::lexer::{Error, Token, TokenEof, LexerMode};
use diagnostic::{FileId, Diagnostics, Span};
use rebo::util::{EscapedResult, TryParseDqstringResult};
use crate::common::{SpanWithId, Spanned};
use crate::error_codes::ErrorCode;
use super::token::*;
use crate::util::{self, TryParseNumberResult};

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

pub fn lex_next<'i>(diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, mut index: usize, mode: LexerMode) -> Result<Token<'i>, Error> {
    loop {
        trace!("lex_next: {}", index);
        // skip preceding whitespace
        index = match skip_whitespace(s, index) {
            Some(index) => index,
            None => return Ok(Token::Eof(TokenEof { span: SpanWithId::new(file, index, index) })),
        };

        if s[index..].is_empty() {
            return Ok(Token::Eof(TokenEof { span: SpanWithId::new(file, index, index) }));
        }

        let functions = [
            // first numbers, then tokens, so that -1 becomes a num and not neg(num)
            try_lex_number,
            try_lex_token,
            try_lex_bool,
            try_lex_ident,
        ];

        for f in &functions {
            match f(diagnostics, file, s, index)? {
                MaybeToken::Token(token) => {
                    trace!("lexed {:?} as {:?}", &s[token.diagnostics_span().start..token.diagnostics_span().end], token);
                    return Ok(token)
                },
                MaybeToken::Backtrack => continue,
                MaybeToken::Diagnostic(cont) => return lex_next(diagnostics, file, s, cont, mode),
            }
        }

        if index >= s.len() || mode == LexerMode::UnexpectedCharacterEof {
            return Err(Error::UnexpectedEof(Span::new(file, index, s.len())));
        }

        let char = s[index..].chars().next().unwrap();
        let mut diag = diagnostics.error(ErrorCode::UnexpectedCharacter)
            .with_error_label(Span::new(file, index, index), format!("unexpected character `{char}` (U+{:X})", char as u32));

        // modified from https://stackoverflow.com/a/49139933
        const IOS_SMART_PUNCTUATION: &[char] = &[
            // russian double-quote replacements
            '\u{ab}', '\u{bb}',
            // Open-quotes: http://www.fileformat.info/info/unicode/category/Pi/list.htm
            '\u{2018}', '\u{201B}', '\u{201C}', '\u{201E}', '\u{201F}',
            // Close-quotes: http://www.fileformat.info/info/unicode/category/Pf/list.htm
            '\u{2019}', '\u{201D}',
            // Primes: http://www.fileformat.info/info/unicode/category/Po/list.htm
            '\u{2032}', '\u{2033}', '\u{2035}', '\u{2036}',
            // iOS 11 also replaces dashes with em-dash and "--" with en-dash
            '\u{2014}', '\u{2013}',
        ];
        if IOS_SMART_PUNCTUATION.contains(&char) {
            diag = diag.with_note("if you're using iOS, consider disabling Smart Punctuation in iOS Settings");
        }
        diag.emit();
        index += char.len_utf8();
    }
}

pub fn skip_whitespace(s: &str, mut index: usize) -> Option<usize> {
    loop {
        match s[index..].chars().next() {
            None => return None,
            Some(c) if c.is_whitespace() => index += c.len_utf8(),
            Some(_) => break Some(index),
        }
    }
}

macro_rules! lex_kws {
    ($diagnostics:ident, $file:ident, $s:ident, $index:ident; $($kw:literal => $token_name:ident, $token:ident;)*) => {
        $(
            match try_lex_ident($diagnostics, $file, $s, $index) {
                Ok(MaybeToken::Token(Token::Ident(TokenIdent { ident, span }))) => if ident == $kw {
                    return Ok(MaybeToken::Token(Token::$token_name($token { span })));
                }
                Ok(MaybeToken::Token(token)) => unreachable!("try_lex_ident returned non-Token::Ident: {:?}", token),
                _ => (),
            }
        )*
    }
}

pub fn try_lex_token<'i>(diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, index: usize) -> Result<MaybeToken<'i>, Error> {
    trace!("try_lex_token: {}", index);
    lex_kws! {
        diagnostics, file, s, index;
        "let" => Let, TokenLet;
        "mut" => Mut, TokenMut;
        "fn" => Fn, TokenFn;
        "gen" => Gen, TokenGen;
        "yield" => Yield, TokenYield;
        "struct" => Struct, TokenStruct;
        "enum" => Enum, TokenEnum;
        "impl" => Impl, TokenImpl;
        "match" => Match, TokenMatch;
        "if" => If, TokenIf;
        "while" => While, TokenWhile;
        "for" => For, TokenFor;
        "loop" => Loop, TokenLoop;
        "break" => Break, TokenBreak;
        "continue" => Continue, TokenContinue;
        "return" => Return, TokenReturn;
        "in" => In, TokenIn;
        "static" => Static, TokenStatic;
        "include" => Include, TokenInclude;
        "else" => Else, TokenElse;
        "string" => StringType, TokenStringType;
        "int" => IntType, TokenIntType;
        "float" => FloatType, TokenFloatType;
        "bool" => BoolType, TokenBoolType;
    }
    let char = s[index..].chars().next().unwrap();
    let char_len = char.len_utf8();
    let span = SpanWithId::new(file, index, index + char.len_utf8());
    let char2 = s[index+char_len..].chars().next();
    let char2_len = char2.map(char::len_utf8).unwrap_or_default();
    let span2 = char2.map(|_| SpanWithId::new(file, index, index + char_len + char2_len));
    let char3 = s[index+char_len+char2_len..].chars().next();
    let char3_len = char3.map(char::len_utf8).unwrap_or_default();
    let span3 = char3.map(|_| SpanWithId::new(file, index, index + char_len + char2_len + char3_len));
    match char {
        '(' => Ok(MaybeToken::Token(Token::OpenParen(TokenOpenParen { span }))),
        ')' => Ok(MaybeToken::Token(Token::CloseParen(TokenCloseParen { span }))),
        '{' => Ok(MaybeToken::Token(Token::OpenCurly(TokenOpenCurly { span }))),
        '}' => Ok(MaybeToken::Token(Token::CloseCurly(TokenCloseCurly { span }))),
        ';' => Ok(MaybeToken::Token(Token::Semicolon(TokenSemicolon { span }))),
        '\'' => Ok(MaybeToken::Token(Token::Apostrophe(TokenApostrophe { span }))),
        ':' => match char2 {
            Some(':') => Ok(MaybeToken::Token(Token::DoubleColon(TokenDoubleColon { span: span2.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::Colon(TokenColon { span }))),
        }
        '+' => Ok(MaybeToken::Token(Token::Plus(TokenPlus { span }))),
        '-' => match char2 {
            Some('>') => Ok(MaybeToken::Token(Token::Arrow(TokenArrow { span: span2.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::Minus(TokenMinus { span }))),
        }
        '*' => Ok(MaybeToken::Token(Token::Star(TokenStar { span }))),
        '/' => match char2 {
            Some('/') => Ok(MaybeToken::Token(lex_line_comment(diagnostics, file, s, index))),
            Some('*') => Ok(MaybeToken::Token(lex_block_comment(diagnostics, file, s, index))),
            _ => Ok(MaybeToken::Token(Token::Slash(TokenSlash { span }))),
        }
        '%' => Ok(MaybeToken::Token(Token::Percent(TokenPercent { span }))),
        '^' => Ok(MaybeToken::Token(Token::Circumflex(TokenCircumflex { span }))),
        ',' => Ok(MaybeToken::Token(Token::Comma(TokenComma { span }))),
        '=' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::Equals(TokenEquals { span: span2.unwrap() }))),
            Some('>') => Ok(MaybeToken::Token(Token::FatArrow(TokenFatArrow { span: span2.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::Assign(TokenAssign { span }))),
        }
        '>' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::GreaterEquals(TokenGreaterEquals { span: span2.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::GreaterThan(TokenGreaterThan { span }))),
        }
        '<' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::LessEquals(TokenLessEquals { span: span2.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::LessThan(TokenLessThan { span }))),
        }
        '!' => match char2 {
            Some('=') => Ok(MaybeToken::Token(Token::NotEquals(TokenNotEquals { span: span2.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::Bang(TokenBang { span }))),
        }
        '&' => match char2 {
            Some('&') => Ok(MaybeToken::Token(Token::DoubleAmp(TokenDoubleAmp { span: span2.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::Amp(TokenAmp { span }))),
        }
        '|' => match char2 {
            Some('|') => Ok(MaybeToken::Token(Token::DoublePipe(TokenDoublePipe { span: span2.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::Pipe(TokenPipe { span }))),
        }
        '.' => match (char2, char3) {
            (Some('.'), Some('.')) => Ok(MaybeToken::Token(Token::DotDotDot(TokenDotDotDot { span: span3.unwrap() }))),
            _ => Ok(MaybeToken::Token(Token::Dot(TokenDot { span }))),
        }
        '_' => match char2 {
            // ident
            Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') => Ok(MaybeToken::Backtrack),
            _ => Ok(MaybeToken::Token(Token::Underscore(TokenUnderscore { span }))),
        }
        '"' => Ok(MaybeToken::Token(lex_double_quoted_string(diagnostics, file, s, index)?)),
        'f' => match char2 {
            Some('"') => Ok(MaybeToken::Token(lex_format_string(diagnostics, file, s, index))),
            _ => Ok(MaybeToken::Backtrack),
        }
        _ => Ok(MaybeToken::Backtrack),
    }
}

pub fn try_lex_number<'i>(diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, index: usize) -> Result<MaybeToken<'i>, Error> {
    trace!("try_lex_number: {}", index);
    match util::try_parse_number(&s[index..]) {
        TryParseNumberResult::Int(value, radix, end) => Ok(MaybeToken::Token(Token::Integer(TokenInteger {
            span: SpanWithId::new(file, index, index + end),
            value,
            radix,
        }))),
        TryParseNumberResult::Float(value, radix, end) => Ok(MaybeToken::Token(Token::Float(TokenFloat {
            span: SpanWithId::new(file, index, index + end),
            value,
            radix,
        }))),
        TryParseNumberResult::Error(e, radix, _) if e.index == 0 && radix == Radix::Dec => Ok(MaybeToken::Backtrack),
        TryParseNumberResult::Error(e, _, end) => {
            diagnostics.error(ErrorCode::InvalidNumber)
                .with_error_label(Span::new(file, index, index + end), format!("{:?}", e.code))
                .emit();
            Ok(MaybeToken::Diagnostic(index + end))
        }
    }
}

pub fn try_lex_bool<'i>(_diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, index: usize) -> Result<MaybeToken<'i>, Error> {
    trace!("try_lex_bool: {}", index);
    if s[index..].starts_with("true") {
        Ok(MaybeToken::Token(Token::Bool(TokenBool {
            span: SpanWithId::new(file, index, index + 4),
            value: true,
        })))
    } else if s[index..].starts_with("false") {
        Ok(MaybeToken::Token(Token::Bool(TokenBool {
            span: SpanWithId::new(file, index, index + 5),
            value: false,
        })))
    } else {
        Ok(MaybeToken::Backtrack)
    }
}

pub fn try_lex_ident<'i>(_diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, mut index: usize) -> Result<MaybeToken<'i>, Error> {
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
                span: SpanWithId::new(file, start, index),
                ident: &s[start..index]
            }))),
        }
    }
}

pub fn lex_line_comment<'i>(_diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, index: usize) -> Token<'i> {
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
            Some(c) => end += c.len_utf8(),
            None => break,
        }
    }
    Token::LineComment(TokenLineComment {
        span: SpanWithId::new(file, index, end),
        comment: &s[index..end],
    })
}
pub fn lex_block_comment<'i>(diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, index: usize) -> Token<'i> {
    trace!("lex_block_comment: {}", index);
    assert_eq!(s[index..].chars().next(), Some('/'));
    assert_eq!(s[index+1..].chars().next(), Some('*'));
    let warn = |start, end| {
        diagnostics.warning(ErrorCode::UnclosedBlockComment)
            .with_info_label(Span::new(file, start, end), "")
            .with_info_label(Span::new(file, end, end), "try inserting `*/` here")
            .emit();
        Token::BlockComment(TokenBlockComment {
            span: SpanWithId::new(file, start, end),
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
                    span: SpanWithId::new(file, index, end),
                    comment: &s[index..end],
                });
            },
            ('*', '/') => depth -= 1,
            ('/', '*') => depth += 1,
            _ => (),
        }
    }
}

pub fn lex_double_quoted_string<'i>(diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, index: usize) -> Result<Token<'i>, Error> {
    trace!("lex_double_quoted_string: {}", index);
    let (string, end) = match util::try_parse_dqstring(&s[index..]) {
        TryParseDqstringResult::String(string, end) => (string, index + end),
        TryParseDqstringResult::DoesntStartWithDq => unreachable!("already checked before this function was called"),
        TryParseDqstringResult::Unterminated(end) => {
            let end = index + end;
            diagnostics.error(ErrorCode::UnterminatedString)
                .with_error_label(Span::new(file, index, end), "this string is unterminated")
                .with_info_label(Span::new(file, end, end), "try adding a closing `\"`")
                .emit();
            (String::new(), end)
        },
    };
    Ok(Token::DqString(TokenDqString {
        span: SpanWithId::new(file, index, end),
        string,
    }))
}

pub fn lex_format_string<'i>(diagnostics: &Diagnostics<ErrorCode>, file: FileId, s: &'i str, mut index: usize) -> Token<'i> {
    trace!("lex_format_string: {}", index);
    assert_eq!(s[index..].chars().next(), Some('f'));
    assert_eq!(s[index+1..].chars().next(), Some('"'));
    let start = index;
    index += 2;

    #[derive(Debug, Copy, Clone)]
    enum CurrentPart {
        Str,
        Escaped,
        FmtArg,
    }

    let mut parts = Vec::new();

    let mut current = CurrentPart::Str;
    let mut part_start = index;
    let mut depth = 0;
    let mut post_index = 0;
    let mut rogue = false;

    fn make_part(s: &str, typ: CurrentPart, part_start: usize, part_end: usize) -> TokenFormatStringPart<'_> {
        match typ {
            CurrentPart::Str => TokenFormatStringPart::Str(&s[part_start..part_end]),
            CurrentPart::Escaped => TokenFormatStringPart::Escaped(&s[part_start..part_end]),
            CurrentPart::FmtArg => TokenFormatStringPart::FormatArg(&s[part_start..part_end], part_start),
        }
    }

    let err_diag = |index| {
        diagnostics.error(ErrorCode::UnterminatedFormatString)
            .with_error_label(Span::new(file, start, index), "this format string is unterminated")
            .with_info_label(Span::new(file, index, index), "try adding a closing `\"`")
            .emit();
    };

    loop {
        match (s[index..].chars().next(), current) {
            (None, _) => {
                if depth == 0 { err_diag(index) };
                rogue = true;
                break;
            },
            (Some('"'), CurrentPart::Str | CurrentPart::Escaped) => {
                post_index = 1;
                break;
            },
            (Some('\\'), _) => {
                let next = match s[index..].chars().nth(1) {
                    None => {
                        if depth == 0 { err_diag(index) };
                        rogue = true;
                        break;
                    },
                    Some(c) => c,
                };
                match util::lex_escaped_char(next) {
                    EscapedResult::ControlChar(_, char_str) => {
                        match current {
                            CurrentPart::Str | CurrentPart::Escaped => {
                                parts.push(make_part(s, current, part_start, index));
                                parts.push(TokenFormatStringPart::Str(char_str));
                                index += 2;
                                part_start = index;
                                current = CurrentPart::Str;
                            }
                            CurrentPart::FmtArg => index += 2,
                        }
                    },
                    EscapedResult::Escaped(c) => {
                        match current {
                            CurrentPart::Str | CurrentPart::Escaped => {
                                parts.push(make_part(s, current, part_start, index));
                                index += 1;
                                part_start = index;
                                current = CurrentPart::Escaped;
                            }
                            CurrentPart::FmtArg => index += 1,
                        }
                        index += c.len_utf8();
                    }
                }
            }
            (Some('{'), CurrentPart::Str) => {
                assert_eq!(depth, 0);
                depth += 1;
                parts.push(make_part(s, current, part_start, index));
                current = CurrentPart::FmtArg;
                index += 1;
                part_start = index;
            }
            (Some('{'), CurrentPart::FmtArg) => {
                depth += 1;
                index += 1;
            }
            (Some('}'), CurrentPart::FmtArg) => {
                depth -= 1;
                if depth == 0 {
                    parts.push(make_part(s, current, part_start, index));
                    current = CurrentPart::Str;
                    index += 1;
                    part_start = index;
                } else {
                    index += 1;
                }
            }
            (Some('}'), CurrentPart::Str) => {
                diagnostics.error(ErrorCode::UnescapedFormatStringCurlyParen)
                    .with_error_label(Span::new(file, index, index+1), "this paren isn't escaped")
                    .with_note("escape it with `\\}`")
                    .emit();
                index += 1;
            }
            (Some(c), _) => {
                index += c.len_utf8();
            }
        }
    }
    if depth != 0 {
        diagnostics.error(ErrorCode::UnterminatedFormatStringArg)
            .with_error_label(Span::new(file, part_start, index), "this interpolation argument isn't closed")
            .with_info_label(Span::new(file, start, index), "in this format string")
            .with_note("if you want to output a curly parenthesis, escape it like `\\{`")
            .emit();
        rogue = true;
    }
    parts.push(make_part(s, current, part_start, index));
    Token::FormatString(TokenFormatString {
        span: SpanWithId::new(file, start, index + post_index),
        parts,
        rogue,
    })
}
