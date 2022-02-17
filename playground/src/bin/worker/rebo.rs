use rebo::{ReboConfig, IncludeDirectoryConfig, ReturnValue, Output, Stdlib, DisplayValue, ExecError};
use itertools::Itertools;
use crate::post_message;
use instant::Instant;
use std::sync::Mutex;
use once_cell::sync::Lazy;

const UPDATE_INTERVAL: u128 = 250;
const MAX_OUTPUT: usize = 100 * 1024;
#[derive(Debug)]
struct State {
    start_time: Instant,
    last_update: u128,
    transmitted: usize,
    output_buffer: String,
}
impl Default for State {
    fn default() -> Self {
        State {
            start_time: Instant::now(),
            last_update: 0,
            transmitted: 0,
            output_buffer: String::new(),
        }
    }
}

static STATE: Lazy<Mutex<State>> = Lazy::new(Default::default);

pub fn run_rebo(code: String) {
    let ret = {
        let config = ReboConfig::new()
            .include_directory(IncludeDirectoryConfig::Everywhere)
            .diagnostic_output({
                Output::buffered(move |s| post_message(s))
            })
            .stdlib(Stdlib::all() - Stdlib::PRINT)
            .interrupt_interval(1_000)
            .interrupt_function(move |_| {
                let mut state = &mut *STATE.lock().unwrap();
                let elapsed = state.start_time.elapsed();
                if elapsed.as_millis() >= 10_000 {
                    state.output_buffer.push_str("\x1b[31;1;4mExecution took too long. Killed\x1b[0m");
                    return Err(ExecError::Panic)
                }

                if elapsed.as_millis() / UPDATE_INTERVAL > state.last_update {
                    state.last_update = elapsed.as_millis() / UPDATE_INTERVAL;
                    let mut output = std::mem::take(&mut state.output_buffer);
                    let left = MAX_OUTPUT - state.transmitted;
                    state.transmitted += output.len();
                    if state.transmitted > MAX_OUTPUT {
                        output.truncate(left);
                        output.push_str("\n\n\x1b[31;1;4mToo much output. Killed\x1b[0m");
                        post_message(output);
                        return Err(ExecError::Panic)
                    }
                    post_message(output);
                }

                Ok(())
            })
            .add_function(print)
            ;
        *STATE.lock().unwrap() = State::default();
        rebo::run_with_config("file.re".to_string(), code, config)
    };
    match ret {
        ReturnValue::Ok => STATE.lock().unwrap().output_buffer.push_str("\n\nExecution successful."),
        ReturnValue::Diagnostics(i) => STATE.lock().unwrap().output_buffer.push_str(&format!("\n\n{i} diagnostics.")),
        ReturnValue::ParseError => STATE.lock().unwrap().output_buffer.push_str("\n\nParse error."),
    }
    let output = std::mem::take(&mut STATE.lock().unwrap().output_buffer);
    post_message(output);
}

#[rebo::function(raw("print"))]
fn print(..: _) {
    let mut joined = args.as_slice().into_iter().map(|arg| DisplayValue(arg)).join(", ");
    joined.push('\n');
    STATE.lock().unwrap().output_buffer.push_str(&joined);
}
