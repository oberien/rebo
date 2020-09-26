fn main() {
    env_logger::init();
    let code = std::fs::read_to_string("test.re").unwrap();
    rebo::run("test.re", code);
}
