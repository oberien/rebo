use rebo::ReturnValue;

fn main() {
    env_logger::init();
    let filename = std::env::args().nth(1).unwrap_or_else(|| "test.re".to_string());
    let code = std::fs::read_to_string(&filename).unwrap();
    match rebo::run(filename, code).return_value {
        ReturnValue::Ok(_) => (),
        ReturnValue::ParseError => ::std::process::exit(2),
        ReturnValue::Diagnostics(_) => ::std::process::exit(3),
        ReturnValue::Panic => ::std::process::exit(4),
    }
}
