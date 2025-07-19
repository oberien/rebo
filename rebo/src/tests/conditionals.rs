use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

#[test]
fn if_else_usage() {
    test(r#"
        if true {} else { panic(""); }
        if true {} else if true { panic(""); }
        if true {} else if true { panic(""); } else { panic(""); }
        if false { panic(""); } else if true {} else { panic(""); }
        if false { panic(""); } else if false { panic(""); } else {}
        assert(if true { true } else { false });
        assert(1342 == if true { 1337 } else { panic("") } + 5);
    "#, ReturnValue::Ok(Value::Unit));
}
#[test]
fn if_else_diagnostics() {
    test(r#"
        // unnecessary parens
        if (true) {} else {}
        // mismatched types
        if true { 1337 } else { "" }
        // missing else branch
        if true { 1337 }
        // mismatched types
        if true { 1337 } else {}
        // mismatched types
        if true { 1337 } else { 42; }
        // expected bool
        if 1337 {}
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Warning(ErrorCode::UnnecessaryIfConditionParenthesis),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::MissingElse),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
    ]));
}
#[test]
fn match_usage() {
    test(r#"
        assert(match true { true => (), false => panic("") } == ());
        assert(match false { true => panic(""), _ => () } == ());
        assert(match false { true => panic(""), foo => assert(!foo) } == ());
        assert(match true { true => (), _ => panic("") } == ());
        assert(match true { true => (), foo => panic("") } == ());
        assert(match 1 { 1 => (), _ => panic("") } == ());
        match 1 { 0 => panic(""), foo => assert(foo == 1) };
        assert(match "uiae" { "uiae" => (), _ => panic("") } == ());
        assert(match "uiae" { "foo" => panic(""), _ => () } == ());
        assert(match () { () => () } == ());
        assert(match () { _ => () } == ());
        assert(match () { () => 1337 } == 1337);
        assert(match "foo" {
            "bar" => 1,
            "baz" => 2,
            whatever => match whatever {
                "foo" => 42,
                _ => 1337,
            },
        } == 42);
    "#, ReturnValue::Ok(Value::Unit));
}
#[test]
fn match_diagnostics() {
    test(r#"
        // non-exhaustive
        match true {}
        // unreachable
        match true { _ => (), foo => {} }
        // float match
        match 1.0 { _ => () }
        // missing catchall
        match 1 { 1 => () }
        // struct match
        struct Foo {}
        match Foo {} { _ => () }
        // non-exhaustive
        match true { true => (), }
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::NonExhaustiveMatch),
        Emitted::Warning(ErrorCode::UnreachableMatchArm),
        Emitted::Error(ErrorCode::FloatMatch),
        Emitted::Error(ErrorCode::MatchNoCatchall),
        Emitted::Error(ErrorCode::StructMatch),
        Emitted::Error(ErrorCode::NonExhaustiveMatch),
    ]));
}
