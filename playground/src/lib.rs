use wasm_logger::Config;
use log::Level;
use serde::{Serialize, Deserialize};

pub fn setup() {
    console_error_panic_hook::set_once();
    wasm_logger::init(Config::new(Level::Info));
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CodePayload {
    pub serial: i32,
    pub code: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OutputPayload {
    pub serial: i32,
    pub output: String,
}
