use rebo::common::{SpanWithId, Spanned};
use rebo::util;
use derive_more::Display;

macro_rules! gen_tokens {
    ($(
        $name:ident$(<$lt:lifetime>)?, $tokenname:ident, $repr:literal, {$($field:ident : $field_ty:ty,)*}, display($fmt:literal $(, $fmtarg:expr)*) $(, ($($copy:ident),+))? $(, #[$comment:meta])?;
    )+) => {
        #[derive(Debug, Clone, PartialEq, Display, rebo_derive::Functions)]
        pub enum Token<'i> {
            $(
                $(#[$comment])?
                #[doc = $repr]
                $name($tokenname $(<$lt>)?),
            )+
        }
        impl<'i> Token<'i> {
            pub fn typ(&self) -> TokenType {
                match self {
                    $(
                        Token::$name($tokenname { .. }) => TokenType::$name,
                    )+
                }
            }
        }
        impl<'i> Spanned for Token<'i> {
            fn span_with_id(&self) -> SpanWithId {
                match self {
                    $(
                        Token::$name($tokenname { span, .. }) => *span,
                    )+
                }
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
        pub enum TokenType {
            $(
                $(#[$comment])? #[doc = $repr] $name,
            )+
        }
        impl TokenType {
            pub fn as_str(self) -> &'static str {
                match self {
                    $(
                        TokenType::$name => $repr,
                    )+
                }
            }
        }
        impl ::std::fmt::Display for TokenType {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(f, "{}", self.as_str())
            }
        }

        $(
            #[derive(Debug, Clone, $($($copy,)+)? PartialEq, Display)]
            #[display($fmt $(, $fmtarg)*)]
            $(#[$comment])?
            #[doc = $repr]
            pub struct $tokenname $(<$lt>)? {
                pub span: SpanWithId,
                $(
                    pub $field: $field_ty,
                )*
            }

            impl$(<$lt>)? Spanned for $tokenname$(<$lt>)? {
                fn span_with_id(&self) -> SpanWithId {
                    self.span
                }
            }
        )+
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenFormatStringPart<'i> {
    Str(&'i str),
    /// Substring that starts with an escaped character
    Escaped(&'i str),
    /// argument-str, start in file
    FormatArg(&'i str, usize),
}

fn format_parts(parts: &[TokenFormatStringPart]) -> String {
    let mut res = String::new();
    for part in parts {
        match part {
            TokenFormatStringPart::Str(s) => res.push_str(s),
            TokenFormatStringPart::Escaped(s) => res.push_str(s),
            TokenFormatStringPart::FormatArg(arg, _) => {
                res.push('{');
                res.push_str(arg);
                res.push('}');
            }
        }
    }
    res
}

gen_tokens! {
    // primitives
    Ident<'i>, TokenIdent, "ident", {ident: &'i str,}, display("{ident}"), (Copy, Eq, Hash);
    DqString, TokenDqString, "double-quoted string", {string: String,}, display("{string:?}"), (Eq, Hash);
    Integer, TokenInteger, "integer value", {value: i64, radix: Radix,}, display("{}", util::to_string_radix(*value, *radix)), (Copy, Eq, Hash);
    Float, TokenFloat, "float value", {value: f64, radix: Radix,}, display("{}", util::to_string_radix(*value, *radix)), (Copy);
    Bool, TokenBool, "bool value", {value: bool,}, display("{value}"), (Copy, Eq, Hash);
    FormatString<'i>, TokenFormatString, "format string", {parts: Vec<TokenFormatStringPart<'i>>, rogue: bool,}, display("f{:?} ", format_parts(parts)), (Eq, Hash);
    // keywords
    Let, TokenLet, "let", {}, display("let"), (Copy, Eq, Hash);
    Mut, TokenMut, "mut", {}, display("mut"), (Copy, Eq, Hash);
    Fn, TokenFn, "fn", {}, display("fn"), (Copy, Eq, Hash);
    Gen, TokenGen, "gen", {}, display("gen"), (Copy, Eq, Hash);
    Yield, TokenYield, "yield", {}, display("yield"), (Copy, Eq, Hash);
    Struct, TokenStruct, "struct", {}, display("struct"), (Copy, Eq, Hash);
    Enum, TokenEnum, "enum", {}, display("enum"), (Copy, Eq, Hash);
    Impl, TokenImpl, "impl", {}, display("impl"), (Copy, Eq, Hash);
    Match, TokenMatch, "match", {}, display("match"), (Copy, Eq, Hash);
    If, TokenIf, "if", {}, display("if"), (Copy, Eq, Hash);
    Else, TokenElse, "else", {}, display("else"), (Copy, Eq, Hash);
    While, TokenWhile, "while", {}, display("while"), (Copy, Eq, Hash);
    For, TokenFor, "for", {}, display("for"), (Copy, Eq, Hash);
    Loop, TokenLoop, "loop", {}, display("loop"), (Copy, Eq, Hash);
    Break, TokenBreak, "break", {}, display("break"), (Copy, Eq, Hash);
    Continue, TokenContinue, "continue", {}, display("continue"), (Copy, Eq, Hash);
    Return, TokenReturn, "return", {}, display("return"), (Copy, Eq, Hash);
    In, TokenIn, "in", {}, display("in"), (Copy, Eq, Hash);
    Static, TokenStatic, "static", {}, display("static"), (Copy, Eq, Hash);
    Include, TokenInclude, "include", {}, display("include"), (Copy, Eq, Hash);
    // types
    StringType, TokenStringType, "string", {}, display("string"), (Copy, Eq, Hash);
    IntType, TokenIntType, "int", {}, display("int"), (Copy, Eq, Hash);
    FloatType, TokenFloatType, "float", {}, display("float"), (Copy, Eq, Hash);
    BoolType, TokenBoolType, "bool", {}, display("bool"), (Copy, Eq, Hash);
    // symbols
    Assign, TokenAssign, "=", {}, display("="), (Copy, Eq, Hash);
    LessThan, TokenLessThan, "<", {}, display("<"), (Copy, Eq, Hash);
    LessEquals, TokenLessEquals, "<=", {}, display("<="), (Copy, Eq, Hash);
    Equals, TokenEquals, "==", {}, display("=="), (Copy, Eq, Hash);
    NotEquals, TokenNotEquals, "!=", {}, display("!="), (Copy, Eq, Hash);
    GreaterEquals, TokenGreaterEquals, ">=", {}, display(">="), (Copy, Eq, Hash);
    GreaterThan, TokenGreaterThan, ">", {}, display(">"), (Copy, Eq, Hash);
    Plus, TokenPlus, "+", {}, display("+"), (Copy, Eq, Hash);
    Minus, TokenMinus, "-", {}, display("-"), (Copy, Eq, Hash);
    Star, TokenStar, "*", {}, display("*"), (Copy, Eq, Hash);
    Slash, TokenSlash, "/", {}, display("/"), (Copy, Eq, Hash);
    Percent, TokenPercent, "%", {}, display("%"), (Copy, Eq, Hash);
    Circumflex, TokenCircumflex, "^", {}, display("^"), (Copy, Eq, Hash);
    Bang, TokenBang, "!", {}, display("!"), (Copy, Eq, Hash);
    Amp, TokenAmp, "&", {}, display("&"), (Copy, Eq, Hash);
    DoubleAmp, TokenDoubleAmp, "&&", {}, display("&&"), (Copy, Eq, Hash);
    Pipe, TokenPipe, "|", {}, display("|"), (Copy, Eq, Hash);
    DoublePipe, TokenDoublePipe, "||", {}, display("||"), (Copy, Eq, Hash);
    OpenParen, TokenOpenParen, "(", {}, display("("), (Copy, Eq, Hash);
    CloseParen, TokenCloseParen,")", {}, display(")"), (Copy, Eq, Hash);
    OpenCurly, TokenOpenCurly, "{", {}, display("{{\n"), (Copy, Eq, Hash);
    CloseCurly, TokenCloseCurly, "}", {}, display("}}\n"), (Copy, Eq, Hash);
    Comma, TokenComma, ",", {}, display(","), (Copy, Eq, Hash);
    Semicolon, TokenSemicolon, ";", {}, display(";\n"), (Copy, Eq, Hash);
    Colon, TokenColon, ":", {}, display(":"), (Copy, Eq, Hash);
    DoubleColon, TokenDoubleColon, "::", {}, display("::"), (Copy, Eq, Hash);
    Arrow, TokenArrow, "->", {}, display("->"), (Copy, Eq, Hash);
    FatArrow, TokenFatArrow, "=>", {}, display("=>"), (Copy, Eq, Hash);
    Dot, TokenDot, ".", {}, display("."), (Copy, Eq, Hash);
    DotDotDot, TokenDotDotDot, "...", {}, display("..."), (Copy, Eq, Hash);
    Underscore, TokenUnderscore, "_", {}, display("_"), (Copy, Eq, Hash);
    Apostrophe, TokenApostrophe, "'", {}, display("'"), (Copy, Eq, Hash);
    LineComment<'i>, TokenLineComment, "//", {comment: &'i str,}, display("{comment}"), (Copy, Eq, Hash), #[doc = "Line Comment including starting `//` and ending newline"];
    BlockComment<'i>, TokenBlockComment, "/* */", {comment: &'i str,}, display("{comment}"), (Copy, Eq, Hash), #[doc = "/// Block Comment including starting `/*` and ending `*/`"];
    Eof, TokenEof, "EOF", {}, display("EOF");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Radix {
    Bin,
    Dec,
    Hex,
}
