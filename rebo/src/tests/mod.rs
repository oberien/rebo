use diagnostic::Emitted;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_tree::HierarchicalLayer;
use rebo::{ErrorCode, ReboConfig};
use crate::ReturnValue;

mod lexer;
mod binops;
mod functions;
mod conditionals;
mod loops;
mod structs;
mod enums;
mod impl_blocks;
mod generics;
mod stdlib;
mod includes;
mod external_type;
mod required_rebo_function;

trait Sorted {
    fn sorted(&self) -> Self;
}
impl<T: Ord + Clone> Sorted for Vec<T> {
    fn sorted(&self) -> Self {
        let mut v = self.clone();
        v.sort();
        v
    }
}
impl Sorted for ReturnValue {
    fn sorted(&self) -> Self {
        match self {
            ReturnValue::Ok(val) => ReturnValue::Ok(val.clone()),
            ReturnValue::Diagnostics(diags) => ReturnValue::Diagnostics(diags.sorted()),
            ReturnValue::Panic => ReturnValue::Panic,
            ReturnValue::ParseError => ReturnValue::ParseError,
        }
    }
}

pub fn test(code: &str, expected: ReturnValue) {
    test_with_config(ReboConfig::new(), code, expected)
}
pub fn test_with_config(config: ReboConfig, code: &str, expected: ReturnValue) {
    tracing_subscriber::registry()
        .with(EnvFilter::from_default_env())
        .with(HierarchicalLayer::default().with_indent_lines(true))
        .with(tracing_subscriber::fmt::layer().with_test_writer())
    ;
    let res = rebo::run_with_config("test".to_string(), code.to_string(), config);
    assert_eq!(expected.sorted(), res.return_value.sorted());
}

#[test]
fn immutable_assign_diagnostic() {
    test(r#"
        // immutable assign
        let foo = 1;
        foo = 42;
    "#, ReturnValue::Diagnostics(vec![Emitted::Error(ErrorCode::ImmutableAssign)]));
}

#[test]
fn pre_parsed() {
    test(r#"
        // type conflict bool-and targets must be bool
        // type conflict add targets must be float / int
        2 + fn foo() {} && true
    "#, ReturnValue::Diagnostics(vec![
        Emitted::Error(ErrorCode::UnableToInferType),
        Emitted::Error(ErrorCode::UnableToInferType),
    ]));
}
