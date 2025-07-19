use diagnostic::Emitted;
use crate::{ReturnValue, Value};
use crate::error_codes::ErrorCode;
use crate::tests::test;

#[test]
fn while_usage() {
    test(r#"
        let mut i = 0;
        while i < 3 {
            i = i + 1;
        }
        assert(i == 3);
    "#, ReturnValue::Ok(Value::Unit));
}
#[test]
fn while_diagnostics() {
    test(r#"
        // unnecessary parens
        while (true) {}
        // type conflict between bool & int
        while 1337 {}
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Warning(ErrorCode::UnnecessaryWhileConditionParenthesis),
        Emitted::Error(ErrorCode::UnableToInferType),
    ]));
}
