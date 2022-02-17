use wasm_logger::Config;
use log::Level;

pub fn setup() {
    console_error_panic_hook::set_once();
    wasm_logger::init(Config::new(Level::Info));
}
