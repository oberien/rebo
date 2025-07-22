use rebo::{ReboConfig, IncludeDirectoryConfig, ReturnValue, Output, Stdlib, DisplayValue, ExecError, IncludeConfig};
use itertools::Itertools;
use crate::post_output;
use instant::Instant;
use std::cell::RefCell;
use js_sys::{Uint32Array, Atomics};
use playground::CodePayload;

const UPDATE_INTERVAL: u128 = 250;
const MAX_OUTPUT: usize = 100 * 1024;
#[derive(Debug)]
struct State {
    buf: Uint32Array,
    current_serial: i32,
    start_time: Instant,
    last_update: u128,
    transmitted: usize,
    output_buffer: String,
}
impl Default for State {
    fn default() -> Self {
        State {
            buf: Uint32Array::default(),
            current_serial: 0,
            start_time: Instant::now(),
            last_update: 0,
            transmitted: 0,
            output_buffer: String::new(),
        }
    }
}

thread_local! {
    static STATE: RefCell<State> = RefCell::new(Default::default());
}

pub fn run_rebo(buf: Uint32Array, code: CodePayload) {
    let ret = {
        let config = ReboConfig::new()
            .include_config(IncludeConfig::Everywhere)
            .diagnostic_output({
                Output::buffered(move |s| STATE.with(|state| {
                    state.borrow_mut().output_buffer.push_str(&s);
                }))
            })
            .stdlib(Stdlib::all() - Stdlib::PRINT)
            .interrupt_interval(1_000)
            .interrupt_function(|_| STATE.with(|state| {
                let mut state = state.borrow_mut();

                // check if new code is available
                let new_serial = Atomics::load(&state.buf, 0)
                    .expect("can't load atomically within rebo interrupt");
                if new_serial > state.current_serial {
                    log::info!("aborting code {}", state.current_serial);
                    state.output_buffer.push_str("\x1b[31;1;4mNew code available. Execution aborted\x1b[0m");
                    return Err(ExecError::Panic)
                }

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
                        post_output(state.current_serial, output, None, None);
                        return Err(ExecError::Panic)
                    }
                    post_output(state.current_serial, output, None, None);
                }
                Ok(())
            }))
            .add_function(print)
            ;
        STATE.with(|state| {
            *state.borrow_mut() = State {
                buf,
                current_serial: code.serial,
                ..Default::default()
            };
        });
        rebo::run_with_config("file.re".to_string(), code.code, config)
    };
    STATE.with(|state| {
        let mut state = state.borrow_mut();
        match ret.return_value {
            ReturnValue::Ok(val) => state.output_buffer.push_str(&format!("\n\nExecution successful, return value: {}", DisplayValue(&val))),
            ReturnValue::Diagnostics(diags) => state.output_buffer.push_str(&format!("\n\n{} diagnostics.", diags.len())),
            ReturnValue::ParseError => state.output_buffer.push_str("\n\nParse error."),
            ReturnValue::Panic => state.output_buffer.push_str("\n\nPanic occurred during execution."),
        }
        let output = std::mem::take(&mut state.output_buffer);
        post_output(state.current_serial, output, ret.type_graph_before, ret.type_graph_after);
    });
}

#[rebo::function(raw("print"))]
fn print(..: _) {
    let mut joined = args.as_slice().into_iter().map(|arg| DisplayValue(arg)).join(", ");
    joined.push('\n');
    STATE.with(|state| state.borrow_mut().output_buffer.push_str(&joined));
}
