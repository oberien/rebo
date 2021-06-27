use log::LevelFilter;

fn main() {
    env_logger::builder()
        .filter_level(LevelFilter::Info)
        .parse_default_env()
        .init();
    let code = std::fs::read_to_string("test.re").unwrap();
    rebo::run("test.re".to_string(), code);
}
