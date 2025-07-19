use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

#[test]
fn other_stuff() {
    test(r#"
        // "integer" should not be parsed as Keyword("int"),Ident("eger") but as Ident("integer")
        let integer = 1337;
    "#, ReturnValue::Ok(Value::Unit));
}

#[test]
fn idents_starting_with_underscore() {
    test(r#"
        let _foo = 1337;
        assert(_foo == 1337);
        match true {
            _val => (),
        }
    "#, ReturnValue::Ok(Value::Unit));
}

#[test]
fn format_strings() {
    test(r#"
        let foo = 42;
        assert(f"uiae" == "uiae");
        assert(f"{foo}" == "42");
        assert(f"{4 * 10 + 2}" == "42");
        assert(f"\{\}" == "{}");
        assert(f"{f"42"}" == "42");
    "#, ReturnValue::Ok(Value::Unit));
}
#[test]
fn format_string_diagnostics1() {
    test(r#"
        // unescaped format string curly paren
        f"}";
        // unterminated format string arg
        f"uiae {";
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::UnescapedFormatStringCurlyParen),
        Emitted::Error(ErrorCode::UnterminatedFormatStringArg),
    ]));
}
#[test]
fn format_string_diagnostics2() {
    test(r#"
        f"uiae
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::UnterminatedFormatString)]));
}
