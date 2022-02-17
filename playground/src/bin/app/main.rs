use yew::{function_component, html, use_state, Html, Properties, Callback, use_ref, use_mut_ref, UseStateHandle};
use gloo_storage::{LocalStorage, Storage};

mod ace_editor;

use ace_editor::AceEditor;
use web_sys::{window, Worker, MessageEvent};
use wasm_bindgen::{JsCast, closure::Closure};
use js_sys::{Uint32Array, Atomics, Function};
use playground::{CodePayload, OutputPayload};
use std::cell::RefCell;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

fn main() {
    playground::setup();
    yew::start_app::<HelloWorld>();
}

fn worker_new() -> Worker {
    let window = window().expect("can't get window()");
    let origin = window.location().origin().expect("can't get origin()");
    let url = format!("{}/worker.js", origin);

    Worker::new(&url).expect("failed to spawn worker")
}

const DEFAULT_CODE: &str = r#"print("Hello, world!");"#;

#[function_component(HelloWorld)]
fn hello_world() -> Html {
    let initial_code = use_ref(|| {
        let initial_code: Result<String, _> = LocalStorage::get("code");
        initial_code.ok()
            .and_then(|s| if s.is_empty() { None } else { Some(s) })
            .unwrap_or_else(|| DEFAULT_CODE.to_string())
    });
    let output_string = use_mut_ref(|| String::new());
    let output = use_state(|| String::new());
    let shared_buffer = use_mut_ref(|| Uint32Array::default());
    let worker = {
        let initial_code = initial_code.clone();
        let output = output.clone();
        let output_string = output_string.clone();
        let shared_buffer = shared_buffer.clone();
        use_mut_ref(move || {
            let w = worker_new();
            let worker = w.clone();
            let onmessage: Function = Closure::wrap(Box::new(move |msg: MessageEvent| {
                if let Ok(buf) = msg.data().dyn_into() {
                    log::warn!("worker started");
                    *shared_buffer.borrow_mut() = buf;
                    send_code(&*output_string, &*shared_buffer.borrow(), &worker, &output, &*initial_code);
                } else if let Some(b) = msg.data().as_bool() {
                    match b {
                        false => {
                            unreachable!("worker sent `false`");
                        }
                        true => {
                            log::warn!("worker finished");
                        }
                    }
                } else {
                    let out: OutputPayload = serde_wasm_bindgen::from_value(msg.data())
                        .expect("can't deserialize OutputPayload");
                    let serial = Atomics::load(&*shared_buffer.borrow(), 0)
                        .expect("can't load atomic");
                    if out.serial == serial {
                        output_string.borrow_mut().push_str(&out.output);
                        output.set(output_string.borrow().clone());
                    }
                }
            }) as Box<dyn Fn(MessageEvent)>).into_js_value().into();
            w.set_onmessage(Some(&onmessage));
            w
        })
    };

    let on_change = {
        let output = output.clone();
        Callback::from(move |s: String| {
            if shared_buffer.borrow().length() > 0 {
                send_code(&*output_string, &*shared_buffer.borrow(), &*worker.borrow(), &output, &s);
            }
            LocalStorage::set("code", s).expect("can't set localStorage");
        })
    };

    return html! {
        <>
        <h1>{ "Rebo Playground" }</h1>
        <div class="outer">
            <AceEditor default_value={(*initial_code).clone()} {on_change} />
            <div class="output">
                <h3>{ "Output: "}</h3>
                <AnsiOutput text={(*output).clone()} />
            </div>
        </div>
        </>
    }
}

fn send_code(output_string: &RefCell<String>, shared_buffer: &Uint32Array, worker: &Worker, output: &UseStateHandle<String>, code: &str) {
    let serial = 1 + Atomics::add(shared_buffer, 0, 1)
        .expect("can't add atomically");
    let code = CodePayload {
        code: code.to_owned(),
        serial,
    };
    log::info!("sending code {serial}");
    let js_value = serde_wasm_bindgen::to_value(&code)
        .expect("can't serialize CodePayload");
    worker.post_message(&js_value).expect("can't post_message CodePayload");
    *output_string.borrow_mut() = String::new();
    output.set(String::new());
}

#[derive(Properties, PartialEq)]
struct AnsiOutputProps {
    text: String,
}

#[function_component(AnsiOutput)]
fn ansi_output(props: &AnsiOutputProps) -> Html {
    let rendered = ansi_to_html::convert_escaped(&props.text)
        .expect("ansi_to_html conversion failed");
    let pre = gloo_utils::document().create_element("pre")
        .expect("can't create element `pre`");
    pre.set_inner_html(&rendered);
    Html::VRef(pre.into())
}
