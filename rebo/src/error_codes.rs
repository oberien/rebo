#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub enum ErrorCode {
    // InternalCompilerError,
    MissingSemicolon,
    UnterminatedString,
    UnexpectedEof,
    InvalidNumber,
    InvalidExpression,
    UnknownIdentifier,
    ImmutableAssign,
    // InvalidLetBinding,
    // IncompleteLetBinding,
    TypeConflict,
    UnableToInferType,
    UnknownFunction,
    // UnclosedBlock,
    InvalidNumberOfArguments,
    // UnclosedParen,
    UnclosedBlockComment,
    EmptyFunctionBody,
    DuplicateGlobal,
    // InvalidFunctionArgument,
    // TypeNotFound,
    RecursiveStruct,
    UnknownStruct,
    UnknownFieldInit,
    MissingField,
    NonStructFieldAccess,
    UnknownFieldAccess,
    UnnecessaryIfConditionParenthesis,
    MissingElse,
    MissingBranchValue,
    MissingBranchBody,
    UnterminatedFormatString,
    UnterminatedFormatStringArg,
    UnescapedFormatStringCurlyParen,
    InvalidFormatString,
    EmptyMatch,
    UnreachableMatchArm,
    FloatMatch,
    MatchNoCatchall,
    StructMatch,
    NonExhaustiveMatch,
    EmptyImplBlock,
    UnknownImplBlockTarget,
    UnknownStructFieldType,
    UnnecessaryWhileConditionParenthesis,
    UnnecessaryMatchTargetParenthesis,
    UnknownType,
    UnknownEnum,
    UnknownEnumVariant,
    InvalidNumberOfEnumVariantFields,
    DuplicateEnumVariant,
    DuplicateStructField,
    SelfBinding,
    UnknownMethod,
    NotAMethod,
    DuplicateGeneric,
    MismatchedGeneric,
    MissingGeneric,
    TooManyGenerics,
    ListStructInitialization,
    ErrorReadingIncludedFile,
    RequiredReboFunctionUnavailable,
    RequiredReboFunctionDiffers,
    FunctionMatch,
    InvalidVarargs,
    MissingFunctionName,
    InvalidFormatStringSpecifier,
    AccessOfUnknown,
    BreakLabelNotFound,
    BreakOutsideOfLoopLike,
    BreakValueInNonLoop,
    ContinueLabelNotFound,
    ContinueOutsideOfLoopLike,
    ReturnOutsideOfFunction,
    UnexpectedCharacter,
    NonVariableAssign,
    NamedFunctionCapture,
    ClosureCapturesMutablePrimitive,

    Panic,
    AssertionFailed,
    InvalidRegex,
    FileError,
}
use ErrorCode::*;

impl diagnostic::ErrorCode for ErrorCode {
    fn code(&self) -> String {
        match self {
            // InternalCompilerError => "0000",
            MissingSemicolon => "0001",
            UnterminatedString => "0002",
            UnexpectedEof => "0003",
            InvalidNumber => "0004",
            InvalidExpression => "0005",
            UnknownIdentifier => "0006",
            ImmutableAssign => "0007",
            // InvalidLetBinding => "0008",
            // IncompleteLetBinding => "0009",
            TypeConflict => "0011",
            UnableToInferType => "0012",
            UnknownFunction => "0014",
            // UnclosedBlock => "0015",
            InvalidNumberOfArguments => "0016",
            // UnclosedParen => "0017",
            UnclosedBlockComment => "0018",
            EmptyFunctionBody => "0019",
            DuplicateGlobal => "0020",
            // InvalidFunctionArgument => "0021",
            // TypeNotFound => "0022",
            RecursiveStruct => "0023",
            UnknownStruct => "0024",
            UnknownFieldInit => "0025",
            MissingField => "0026",
            NonStructFieldAccess => "0027",
            UnknownFieldAccess => "0028",
            UnnecessaryIfConditionParenthesis => "0029",
            MissingElse => "0030",
            MissingBranchValue => "0031",
            MissingBranchBody => "0032",
            UnterminatedFormatString => "0033",
            UnterminatedFormatStringArg => "0034",
            UnescapedFormatStringCurlyParen => "0035",
            InvalidFormatString => "0036",
            EmptyMatch => "0037",
            UnreachableMatchArm => "0038",
            FloatMatch => "0039",
            MatchNoCatchall => "0040",
            StructMatch => "0041",
            NonExhaustiveMatch => "0042",
            EmptyImplBlock => "0043",
            UnknownImplBlockTarget => "0044",
            UnknownStructFieldType => "0045",
            UnnecessaryWhileConditionParenthesis => "0046",
            UnnecessaryMatchTargetParenthesis => "0047",
            UnknownType => "0048",
            UnknownEnum => "0049",
            UnknownEnumVariant => "0050",
            InvalidNumberOfEnumVariantFields => "0051",
            DuplicateEnumVariant => "0052",
            DuplicateStructField => "0053",
            SelfBinding => "0054",
            UnknownMethod => "0055",
            NotAMethod => "0056",
            DuplicateGeneric => "0057",
            MismatchedGeneric => "0058",
            MissingGeneric => "0059",
            TooManyGenerics => "0060",
            ListStructInitialization => "0061",
            ErrorReadingIncludedFile => "0062",
            RequiredReboFunctionUnavailable => "0063",
            RequiredReboFunctionDiffers => "0064",
            FunctionMatch => "0065",
            InvalidVarargs => "0066",
            MissingFunctionName => "0067",
            InvalidFormatStringSpecifier => "0068",
            AccessOfUnknown => "0069",
            BreakLabelNotFound => "0070",
            BreakOutsideOfLoopLike => "0071",
            BreakValueInNonLoop => "0072",
            ContinueLabelNotFound => "0073",
            ContinueOutsideOfLoopLike => "0074",
            ReturnOutsideOfFunction => "0075",
            UnexpectedCharacter => "0076",
            NonVariableAssign => "0077",
            NamedFunctionCapture => "0078",
            ClosureCapturesMutablePrimitive => "0079",

            Panic => "9999",
            AssertionFailed => "9998",
            InvalidRegex => "9997",
            FileError => "9996",
        }.to_string()
    }

    fn message(&self) -> String {
        match self {
            // InternalCompilerError => "internal compiler error (please report this)",
            MissingSemicolon => "missing semicolon",
            UnterminatedString => "unterminated double quote string",
            UnexpectedEof => "unexpected end of file",
            InvalidNumber => "invalid number",
            InvalidExpression => "invalid expression",
            UnknownIdentifier => "unknown identifier",
            ImmutableAssign => "assignment to immutable variable",
            // InvalidLetBinding => "invalid let binding",
            // IncompleteLetBinding => "incomplete let binding",
            TypeConflict => "type conflict",
            UnableToInferType => "unable to infer type",
            UnknownFunction => "unknown function",
            // UnclosedBlock => "unclosed block, missing `}`",
            InvalidNumberOfArguments => "invalid number of arguments",
            // UnclosedParen => "unclosed parenthesis",
            UnclosedBlockComment => "unclosed block comment",
            EmptyFunctionBody => "empty function body",
            DuplicateGlobal => "conflicting duplicate global definition",
            // InvalidFunctionArgument => "invalid function argument",
            // TypeNotFound => "type not found",
            RecursiveStruct => "recursive struct",
            UnknownStruct => "unknown struct",
            UnknownFieldInit => "unknown struct field in initializer",
            MissingField => "missing struct field",
            NonStructFieldAccess => "tried accessing field of a non-struct",
            UnknownFieldAccess => "tried to access non-existent field",
            UnnecessaryIfConditionParenthesis => "unnecessary parenthesis surrounding if condition",
            MissingElse => "missing else clause",
            MissingBranchValue => "missing branch value",
            MissingBranchBody => "missing branch body",
            UnterminatedFormatString => "unterminated format string",
            UnterminatedFormatStringArg => "unterminated format string argument",
            UnescapedFormatStringCurlyParen => "unescaped `}` in format string",
            InvalidFormatString => "invalid format string",
            EmptyMatch => "empty match statement",
            UnreachableMatchArm => "unreachable match arm",
            FloatMatch => "can't match on a float",
            MatchNoCatchall => "match statement doesn't have a catchall pattern",
            StructMatch => "can't match a struct",
            NonExhaustiveMatch => "non-exhaustive match",
            EmptyImplBlock => "empty impl block",
            UnknownImplBlockTarget => "unknown impl block target",
            UnknownStructFieldType => "unknown type of struct field",
            UnnecessaryWhileConditionParenthesis => "unnecessary parenthesis around while condition",
            UnnecessaryMatchTargetParenthesis => "unnecessary parenthesis around match target",
            UnknownType => "unknown type",
            UnknownEnum => "unknown enum",
            UnknownEnumVariant => "unknown enum variant",
            InvalidNumberOfEnumVariantFields => "invalid number of fields in enum variant",
            DuplicateEnumVariant => "duplicate enum variant",
            DuplicateStructField => "duplicate struct field",
            SelfBinding => "self binding not allowed",
            UnknownMethod => "unknown method",
            NotAMethod => "not a method",
            DuplicateGeneric => "duplicate generic type",
            MismatchedGeneric => "mismatched generic",
            MissingGeneric => "missing generic",
            TooManyGenerics => "too many generics",
            ListStructInitialization => "`List` not allowed in struct initializer",
            ErrorReadingIncludedFile => "error reading included file",
            RequiredReboFunctionUnavailable => "required rebo function doesn't exist",
            RequiredReboFunctionDiffers => "required rebo function does not match found implementation",
            FunctionMatch => "can't match on a function",
            InvalidVarargs => "invalid varargs",
            MissingFunctionName => "missing function name",
            InvalidFormatStringSpecifier => "invalid format string specifier",
            AccessOfUnknown => "can't access field or method of unknown type",
            BreakLabelNotFound => "break label not found",
            BreakOutsideOfLoopLike => "break used outside of loop-like",
            BreakValueInNonLoop => "break value used in non-`loop`",
            ContinueLabelNotFound => "continue label not found",
            ContinueOutsideOfLoopLike => "continue used outside of loop-like",
            ReturnOutsideOfFunction => "return used outside of function",
            UnexpectedCharacter => "unexpected character",
            NonVariableAssign => "tried to assign to something other than a variable",
            NamedFunctionCapture => "named function cannot capture binding",
            ClosureCapturesMutablePrimitive => "closure captures mutable primitive",

            Panic => "explicit panic",
            AssertionFailed => "assertion failed",
            InvalidRegex => "invalid regex",
            FileError => "access denied to file for security reasons",
        }.to_string()
    }
}
