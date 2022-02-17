use wasm_bindgen::{JsCast, JsValue};
use web_sys::{DedicatedWorkerGlobalScope, MessageEvent};
use wasm_bindgen::closure::Closure;

mod rebo;

fn worker_global_scope() -> DedicatedWorkerGlobalScope {
    JsValue::from(js_sys::global()).into()
}
fn post_message<T: Into<JsValue>>(t: T) {
    worker_global_scope().post_message(&t.into()).unwrap();
}

fn main() {
    playground::setup();

    let onmessage = {
        Closure::wrap(Box::new(move |e: MessageEvent| {
            let code = e.data().as_string().unwrap();
            rebo::run_rebo(code);
            post_message(true);
        }) as Box<dyn Fn(MessageEvent)>)
    };
    worker_global_scope().set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
    onmessage.forget();
    // indicate to app that we are ready to receive messages
    post_message(false);
}
