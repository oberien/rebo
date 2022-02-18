use yew::{function_component, Html, Properties, Component, Context, html};
use js_sys::{Uint32Array, Atomics};
use web_sys::{Worker, window, MessageEvent};
use wasm_bindgen::JsCast;
use playground::{OutputPayload, CodePayload};
use gloo_storage::{LocalStorage, Storage};
use gloo_events::EventListener;
use ace_editor::AceEditor;

mod ace_editor;


#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

fn main() {
    playground::setup();
    yew::start_app::<MainComponent>();
}

const DEFAULT_CODE: &str = r#"print("Hello, world!");"#;

pub enum Msg {
    WorkerStarted(Uint32Array),
    WorkerOutput(OutputPayload),
    WorkerStopped,
    CodeChanged(String),
}

#[derive(Properties, Default, PartialEq)]
pub struct Props {

}

pub struct MainComponent {
    code: String,
    output: String,
    shared_buffer: Uint32Array,
    worker: Worker,
    _worker_onmessage_event_listener: EventListener,
}

impl Component for MainComponent {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let code: Result<String, _> = LocalStorage::get("code");
        let code = code.ok()
            .and_then(|s| if s.is_empty() { None } else { Some(s) })
            .unwrap_or_else(|| DEFAULT_CODE.to_string());
        let shared_buffer = Uint32Array::default();

        let worker = worker_new();
        let link = ctx.link().clone();
        let worker_onmessage_event_listener = EventListener::new(&worker, "message", move |event| {
            let msg = event.dyn_ref::<MessageEvent>().unwrap();
            let msg = if let Ok(buf) = msg.data().dyn_into() {
                log::warn!("worker started");
                Msg::WorkerStarted(buf)
            } else if let Some(b) = msg.data().as_bool() {
                match b {
                    false => {
                        unreachable!("worker sent `false`")
                    }
                    true => {
                        log::warn!("worker finished");
                        Msg::WorkerStopped
                    }
                }
            } else {
                let out: OutputPayload = serde_wasm_bindgen::from_value(msg.data())
                    .expect("can't deserialize OutputPayload");
                Msg::WorkerOutput(out)
            };
            link.send_message(msg);
        });

        MainComponent {
            code,
            output: String::new(),
            shared_buffer,
            worker,
            _worker_onmessage_event_listener: worker_onmessage_event_listener,
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::WorkerStarted(shared_buffer) => {
                self.shared_buffer = shared_buffer;
                send_code(&self.shared_buffer, &self.worker, &self.code);
                false
            }
            Msg::WorkerOutput(OutputPayload { output, serial }) => {
                let expected_serial = Atomics::load(&self.shared_buffer, 0)
                    .expect("can't load atomic");
                if serial == expected_serial {
                    self.output.push_str(&output);
                    true
                } else {
                    false
                }
            }
            Msg::WorkerStopped => false,
            Msg::CodeChanged(code) => {
                self.code = code;
                if self.shared_buffer.length() > 0 {
                    send_code(&self.shared_buffer, &self.worker, &self.code);
                }
                self.output.clear();
                LocalStorage::set("code", &self.code).expect("can't set localStorage");
                false
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let on_change = ctx.link().callback(|code: String| Msg::CodeChanged(code));
        html! {
            <>
            <h1>{ "Rebo Playground" }</h1>
            <div class="outer">
                <AceEditor default_value={self.code.clone()} {on_change} />
                <div class="output">
                    <h3>{ "Output: "}</h3>
                    <AnsiOutput text={self.output.clone()} />
                </div>
            </div>
            </>
        }
    }
}

fn worker_new() -> Worker {
    let window = window().expect("can't get window()");
    let origin = window.location().origin().expect("can't get origin()");
    let url = format!("{}/worker.js", origin);

    Worker::new(&url).expect("failed to spawn worker")
}

fn send_code(shared_buffer: &Uint32Array, worker: &Worker, code: &str) {
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
