use rebo::{ReboConfig, IncludeDirectoryConfig};

fn main() {
    env_logger::init();
    let filename = std::env::args().nth(1).unwrap_or_else(|| "test.re".to_string());
    let code = std::fs::read_to_string(&filename).unwrap();
    rebo::run(filename, code);
    // rebo::run_with_config(filename, code, ReboConfig::new().include_directory(IncludeDirectoryConfig::Everywhere));
}
