use rebo::{ReboConfig, IncludeDirectoryConfig, ReturnValue, Output, Stdlib, DisplayValue, ExecError};
use std::sync::Mutex;
use once_cell::sync::Lazy;
use std::rc::Rc;
use std::cell::RefCell;
use itertools::Itertools;

static PRINTOUT: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new(String::new()));

pub fn run_rebo(code: String) -> String {
    let out = Rc::new(RefCell::new(String::new()));
    let ret = {
        let res_string = Rc::clone(&out);
        let config = ReboConfig::new()
            .include_directory(IncludeDirectoryConfig::Everywhere)
            .diagnostic_output({
                Output::buffered(move |s| *res_string.borrow_mut() += &s)
            })
            .stdlib(Stdlib::all() - Stdlib::PRINT)
            .interrupt_interval(1_000_000)
            .interrupt_function(|_| {
                PRINTOUT.lock().unwrap().push_str("\x1b[31;1;4mExecution took too long. Killed\x1b[0m");
                Err(ExecError::Panic)
            })
            .add_function(print)
            ;
        PRINTOUT.lock().unwrap().clear();
        rebo::run_with_config("file.re".to_string(), code, config)
    };
    *out.borrow_mut() += &PRINTOUT.lock().unwrap();
    match ret {
        ReturnValue::Ok => *out.borrow_mut() += "\n\nExecution successful.",
        ReturnValue::Diagnostics(i) => *out.borrow_mut() += &format!("\n\n{i} diagnostics."),
        ReturnValue::ParseError => *out.borrow_mut() += "\n\nParse error.",
    }
    Rc::try_unwrap(out).unwrap().into_inner()
}

#[rebo::function(raw("print"))]
fn print(..: _) {
    let joined = args.as_slice().into_iter().map(|arg| DisplayValue(arg)).join(", ");
    let mut out = PRINTOUT.lock().unwrap();
    out.push_str(&joined);
    out.push('\n');
}
