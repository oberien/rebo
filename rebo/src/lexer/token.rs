use rebo::common::{SpanWithId, Spanned};
use derive_more::Display;

macro_rules! gen_tokens {
    ($(
        $name:ident$(<$lt:lifetime>)?, $tokenname:ident, $repr:literal, {$($field:ident : $field_ty:ty,)*}, (fmt = $fmt:literal $(, $fmtarg:expr)*) $(, ($($copy:ident),+))? $(, #[$comment:meta])?;
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
            #[display(fmt = $fmt $(, $fmtarg)*)]
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
    Ident<'i>, TokenIdent, "ident", {ident: &'i str,}, (fmt = "{}", ident), (Copy, Eq, Hash);
    DqString, TokenDqString, "double-quoted string", {string: String,}, (fmt = "{:?}", string), (Eq, Hash);
    Integer, TokenInteger, "integer value", {value: i64, radix: Radix,}, (fmt = "{}", "lexical::to_string_radix(*value, radix.to_u8())"), (Copy, Eq, Hash);
    Float, TokenFloat, "float value", {value: f64, radix: Radix,}, (fmt = "{}", "lexical::to_string_radix(*value, radix.to_u8())"), (Copy);
    Bool, TokenBool, "bool value", {value: bool,}, (fmt = "{}", value), (Copy, Eq, Hash);
    FormatString<'i>, TokenFormatString, "format string", {parts: Vec<TokenFormatStringPart<'i>>, rogue: bool,}, (fmt = "f{:?} ", "format_parts(parts)"), (Eq, Hash);
    // keywords
    Let, TokenLet, "let", {}, (fmt = "let"), (Copy, Eq, Hash);
    Mut, TokenMut, "mut", {}, (fmt = "mut"), (Copy, Eq, Hash);
    Fn, TokenFn, "fn", {}, (fmt = "fn"), (Copy, Eq, Hash);
    Gen, TokenGen, "gen", {}, (fmt = "gen"), (Copy, Eq, Hash);
    Yield, TokenYield, "yield", {}, (fmt = "yield"), (Copy, Eq, Hash);
    Struct, TokenStruct, "struct", {}, (fmt = "struct"), (Copy, Eq, Hash);
    Enum, TokenEnum, "enum", {}, (fmt = "enum"), (Copy, Eq, Hash);
    Impl, TokenImpl, "impl", {}, (fmt = "impl"), (Copy, Eq, Hash);
    Match, TokenMatch, "match", {}, (fmt = "match"), (Copy, Eq, Hash);
    If, TokenIf, "if", {}, (fmt = "if"), (Copy, Eq, Hash);
    Else, TokenElse, "else", {}, (fmt = "else"), (Copy, Eq, Hash);
    While, TokenWhile, "while", {}, (fmt = "while"), (Copy, Eq, Hash);
    For, TokenFor, "for", {}, (fmt = "for"), (Copy, Eq, Hash);
    Loop, TokenLoop, "loop", {}, (fmt = "loop"), (Copy, Eq, Hash);
    Break, TokenBreak, "break", {}, (fmt = "break"), (Copy, Eq, Hash);
    Continue, TokenContinue, "continue", {}, (fmt = "continue"), (Copy, Eq, Hash);
    Return, TokenReturn, "return", {}, (fmt = "return"), (Copy, Eq, Hash);
    In, TokenIn, "in", {}, (fmt = "in"), (Copy, Eq, Hash);
    Static, TokenStatic, "static", {}, (fmt = "static"), (Copy, Eq, Hash);
    Include, TokenInclude, "include", {}, (fmt = "include"), (Copy, Eq, Hash);
    // types
    StringType, TokenStringType, "string", {}, (fmt = "string"), (Copy, Eq, Hash);
    IntType, TokenIntType, "int", {}, (fmt = "int"), (Copy, Eq, Hash);
    FloatType, TokenFloatType, "float", {}, (fmt = "float"), (Copy, Eq, Hash);
    BoolType, TokenBoolType, "bool", {}, (fmt = "bool"), (Copy, Eq, Hash);
    // symbols
    Assign, TokenAssign, "=", {}, (fmt = "="), (Copy, Eq, Hash);
    LessThan, TokenLessThan, "<", {}, (fmt = "<"), (Copy, Eq, Hash);
    LessEquals, TokenLessEquals, "<=", {}, (fmt = "<="), (Copy, Eq, Hash);
    Equals, TokenEquals, "==", {}, (fmt = "=="), (Copy, Eq, Hash);
    NotEquals, TokenNotEquals, "!=", {}, (fmt = "!="), (Copy, Eq, Hash);
    GreaterEquals, TokenGreaterEquals, ">=", {}, (fmt = ">="), (Copy, Eq, Hash);
    GreaterThan, TokenGreaterThan, ">", {}, (fmt = ">"), (Copy, Eq, Hash);
    Plus, TokenPlus, "+", {}, (fmt = "+"), (Copy, Eq, Hash);
    Minus, TokenMinus, "-", {}, (fmt = "-"), (Copy, Eq, Hash);
    Star, TokenStar, "*", {}, (fmt = "*"), (Copy, Eq, Hash);
    Slash, TokenSlash, "/", {}, (fmt = "/"), (Copy, Eq, Hash);
    Percent, TokenPercent, "%", {}, (fmt = "%"), (Copy, Eq, Hash);
    Circumflex, TokenCircumflex, "^", {}, (fmt = "^"), (Copy, Eq, Hash);
    Bang, TokenBang, "!", {}, (fmt = "!"), (Copy, Eq, Hash);
    Amp, TokenAmp, "&", {}, (fmt = "&"), (Copy, Eq, Hash);
    DoubleAmp, TokenDoubleAmp, "&&", {}, (fmt = "&&"), (Copy, Eq, Hash);
    Pipe, TokenPipe, "|", {}, (fmt = "|"), (Copy, Eq, Hash);
    DoublePipe, TokenDoublePipe, "||", {}, (fmt = "||"), (Copy, Eq, Hash);
    OpenParen, TokenOpenParen, "(", {}, (fmt = "("), (Copy, Eq, Hash);
    CloseParen, TokenCloseParen,")", {}, (fmt = ")"), (Copy, Eq, Hash);
    OpenCurly, TokenOpenCurly, "{", {}, (fmt = "{{\n"), (Copy, Eq, Hash);
    CloseCurly, TokenCloseCurly, "}", {}, (fmt = "}}\n"), (Copy, Eq, Hash);
    Comma, TokenComma, ",", {}, (fmt = ","), (Copy, Eq, Hash);
    Semicolon, TokenSemicolon, ";", {}, (fmt = ";\n"), (Copy, Eq, Hash);
    Colon, TokenColon, ":", {}, (fmt = ":"), (Copy, Eq, Hash);
    DoubleColon, TokenDoubleColon, "::", {}, (fmt = "::"), (Copy, Eq, Hash);
    Arrow, TokenArrow, "->", {}, (fmt = "->"), (Copy, Eq, Hash);
    FatArrow, TokenFatArrow, "=>", {}, (fmt = "=>"), (Copy, Eq, Hash);
    Dot, TokenDot, ".", {}, (fmt = "."), (Copy, Eq, Hash);
    DotDotDot, TokenDotDotDot, "...", {}, (fmt = "..."), (Copy, Eq, Hash);
    Underscore, TokenUnderscore, "_", {}, (fmt = "_"), (Copy, Eq, Hash);
    Apostrophe, TokenApostrophe, "'", {}, (fmt = "'"), (Copy, Eq, Hash);
    LineComment<'i>, TokenLineComment, "//", {comment: &'i str,}, (fmt = "{}", comment), (Copy, Eq, Hash), #[doc = "Line Comment including starting `//` and ending newline"];
    BlockComment<'i>, TokenBlockComment, "/* */", {comment: &'i str,}, (fmt = "{}", comment), (Copy, Eq, Hash), #[doc = "/// Block Comment including starting `/*` and ending `*/`"];
    Eof, TokenEof, "EOF", {}, (fmt = "EOF");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Radix {
    Bin,
    Dec,
    Hex,
}

impl Radix {
    pub fn to_u8(self) -> u8 {
        match self {
            Radix::Bin => 2,
            Radix::Dec => 10,
            Radix::Hex => 16,
        }
    }
}
