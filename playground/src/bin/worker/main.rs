use wasm_bindgen::{JsCast, JsValue};
use web_sys::{DedicatedWorkerGlobalScope, MessageEvent};
use wasm_bindgen::closure::Closure;
use js_sys::{SharedArrayBuffer, Uint32Array, Atomics};
use playground::{CodePayload, OutputPayload};

mod rebo;

fn worker_global_scope() -> DedicatedWorkerGlobalScope {
    JsValue::from(js_sys::global()).into()
}
fn post_message<T: Into<JsValue>>(t: T) {
    worker_global_scope().post_message(&t.into())
        .expect("can't post_message from worker");
}
fn post_output(serial: i32, output: String) {
    let val = serde_wasm_bindgen::to_value(&OutputPayload {
        serial,
        output,
    }).expect("can't serialize OutputPayload");
    post_message(val);
}

fn main() {
    playground::setup();

    let sab = SharedArrayBuffer::new(4);
    let buf = Uint32Array::new(&sab);

    let onmessage = {
        let buf = buf.clone();
        Closure::wrap(Box::new(move |e: MessageEvent| {
            let code: CodePayload = serde_wasm_bindgen::from_value(e.data())
                .expect("can't deserialize CodePayload");
            let new_serial = Atomics::load(&buf, 0)
                .expect("can't load atomically");
            if code.serial < new_serial {
                log::info!("dropping code {}", code.serial);
                return;
            }
            log::info!("executing code {}", code.serial);
            rebo::run_rebo(buf.clone().into(), code);
            // indicate to app that we are done
            post_message(true);
        }) as Box<dyn Fn(MessageEvent)>)
    };
    worker_global_scope().set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
    onmessage.forget();
    // indicate to app that we are ready to receive messages
    post_message(buf);
}
