use flate2::Compression;
use flate2::bufread::{GzDecoder, GzEncoder};
use yew::{function_component, Html, Properties, Component, Context, html};
use js_sys::{Uint32Array, Atomics};
use web_sys::{Worker, window, MessageEvent};
use wasm_bindgen::{JsCast, JsValue};
use playground::{OutputPayload, CodePayload};
use gloo_storage::{LocalStorage, Storage};
use gloo_events::EventListener;
use std::io::{Read, Cursor};
use url::Url;
use editor::Editor;
use crate::config::{EditorType, Config, ConfigComponent, KeyboardHandler};

mod editor;
mod config;

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
    WorkerFinished,
    CodeChanged(String),
    SetDefaultCode(String),
    EditorChanged(EditorType),
    KeyboardHandlerChanged(KeyboardHandler),
    // GetLink,
}

pub struct MainComponent {
    config: Config,
    default_code: String,
    default_code_serial: i32,
    code: String,
    output: String,
    shared_buffer: Uint32Array,
    worker: Worker,
    _worker_onmessage_event_listener: EventListener,
    _window_onhashchange_event_listener: EventListener,
}

impl Component for MainComponent {
    type Message = Msg;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let config: Result<Config, _> = LocalStorage::get("config");
        let config = config.ok()
            .unwrap_or_else(Config::default);

        // load code from url-fragment-string or local-storage or default
        let code = load_fragment_code().or_else(|| {
            let code: Result<String, _> = LocalStorage::get("code");
            code.ok()
                .and_then(|s| if s.is_empty() { None } else { Some(s) })
        }).unwrap_or_else(|| DEFAULT_CODE.to_string());
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
                        Msg::WorkerFinished
                    }
                }
            } else {
                let out: OutputPayload = serde_wasm_bindgen::from_value(msg.data())
                    .expect("can't deserialize OutputPayload");
                Msg::WorkerOutput(out)
            };
            link.send_message(msg);
        });
        let link = ctx.link().clone();
        let window = window().expect("can't get window()");
        let window_onhashchange_event_listener = EventListener::new(&window, "hashchange", move |_event| {
            log::info!("onhashchange");
            if let Some(code) = load_fragment_code() {
                link.send_message(Msg::CodeChanged(code.clone()));
                link.send_message(Msg::SetDefaultCode(code));
            }
        });

        MainComponent {
            config,
            default_code: code.clone(),
            default_code_serial: 0,
            code,
            output: String::new(),
            shared_buffer,
            worker,
            _worker_onmessage_event_listener: worker_onmessage_event_listener,
            _window_onhashchange_event_listener: window_onhashchange_event_listener,
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
            Msg::WorkerFinished => false,
            Msg::CodeChanged(code) => {
                self.code = code;
                if self.shared_buffer.length() > 0 {
                    send_code(&self.shared_buffer, &self.worker, &self.code);
                }
                self.output.clear();
                LocalStorage::set("code", &self.code).expect("can't store code in localStorage");
                // set fragment
                let mut encoder = GzEncoder::new(Cursor::new(self.code.as_bytes()), Compression::new(6));
                let mut res = Vec::new();
                encoder.read_to_end(&mut res).unwrap();
                let base64 = base64::encode_config(&res, base64::URL_SAFE);
                window().expect("can't get window()")
                    .history().expect("can't get history()")
                    .replace_state_with_url(&JsValue::NULL, "", Some(&format!("/#{base64}"))).expect("can't replaceState");
                false
            }
            Msg::SetDefaultCode(code) => {
                log::error!("setDefaultCode");
                self.default_code = code;
                self.default_code_serial += 1;
                true
            }
            Msg::EditorChanged(editor) => {
                self.config.editor = editor;
                LocalStorage::set("config", &self.config).expect("can't store config in localStorage");
                true
            }
            Msg::KeyboardHandlerChanged(keyboard_hanlder) => {
                self.config.keyboard_handler = keyboard_hanlder;
                LocalStorage::set("config", &self.config).expect("can't store config in localStorage");
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let on_change = ctx.link().callback(|code: String| Msg::CodeChanged(code));
        let on_editor_change = ctx.link().callback(|editor: EditorType| Msg::EditorChanged(editor));
        let on_keyboard_handler_change = ctx.link().callback(|keyboard_handler: KeyboardHandler| Msg::KeyboardHandlerChanged(keyboard_handler));
        html! {
            <>
            <h1>{ "Rebo Playground" }</h1>
            <div class="outer">
                <div class="code-wrapper">
                    <ConfigComponent config={self.config.clone()} {on_editor_change} {on_keyboard_handler_change} />
                    <Editor config={self.config.clone()} default_value={self.default_code.clone()} default_value_serial={self.default_code_serial} {on_change} />
                </div>
                <div class="output">
                    <h3>{ "Output: "}</h3>
                    <AnsiOutput text={self.output.clone()} />
                </div>
            </div>
            </>
        }
    }

    fn destroy(&mut self, _ctx: &Context<Self>) {
        self.worker.terminate();
    }
}

/// Load code from the location's URL-fragment
fn load_fragment_code() -> Option<String> {
    let window = window().expect("can't get window()");
    let href = window.location().href().expect("can't get href");
    let url = Url::parse(&href).expect("href is not Url");
    let url_code = url.fragment().and_then(|s| if s.is_empty() { None } else { Some(s) });
    url_code.and_then(|encoded| {
        let decoded = match base64::decode_config(encoded, base64::URL_SAFE) {
            Ok(decoded) => decoded,
            Err(e) => {
                log::error!("can't base64decode query string: {:?}", e);
                return None
            }
        };
        let mut code = String::new();
        match GzDecoder::new(Cursor::new(decoded)).read_to_string(&mut code) {
            Ok(_) => (),
            Err(e) => {
                log::error!("invalid gz query string: {:?}", e);
                return None;
            }
        }
        LocalStorage::set("code", &code).expect("can't set localStorage");
        Some(code)
    })
}

fn worker_new() -> Worker {
    let window = window().expect("can't get window()");
    let origin = window.location().origin().expect("can't get origin()");
    let url = format!("{}/worker-start.js", origin);

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
