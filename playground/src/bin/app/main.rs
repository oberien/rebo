use flate2::Compression;
use flate2::bufread::{GzDecoder, GzEncoder};
use yew::{function_component, Html, Properties, Component, Context, html, use_memo, use_effect_with_deps, use_state, classes, Callback};
use js_sys::{Uint32Array, Atomics};
use web_sys::{Worker, window, MessageEvent, Node};
use wasm_bindgen::{JsCast, JsValue};
use playground::{OutputPayload, CodePayload};
use gloo_storage::{LocalStorage, Storage};
use gloo_events::EventListener;
use std::io::{Read, Cursor};
use serde::{Deserialize, Serialize};
use url::Url;
use wasm_bindgen::prelude::wasm_bindgen;
use editor::Editor;
use crate::config::{EditorType, Config, ConfigComponent, KeyboardHandler};

mod editor;
mod config;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

fn main() {
    playground::setup();
    yew::Renderer::<MainComponent>::new().render();
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
    type_graph_before: Option<String>,
    type_graph_after: Option<String>,
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
            type_graph_before: None,
            type_graph_after: None,
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
            Msg::WorkerOutput(OutputPayload { output, serial, type_graph_before, type_graph_after }) => {
                let expected_serial = Atomics::load(&self.shared_buffer, 0)
                    .expect("can't load atomic");
                if serial == expected_serial {
                    self.output.push_str(&output);
                    self.type_graph_before = type_graph_before;
                    self.type_graph_after = type_graph_after;
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
                self.type_graph_before = None;
                self.type_graph_after = None;
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
            <h1>
                { "Rebo Playground " }
                <span style="font-size: .75em">
                    <a href="https://rebo.oberien.de/book/" style="margin-left:1em" target="_blank">{ "documentation" }</a>
                    <a href="https://github.com/oberien/rebo" style="margin-left:1em" target="_blank">{ "github" }</a>
                </span>
            </h1>
            <div class="outer">
                <div class="left">
                    <ConfigComponent config={self.config.clone()} {on_editor_change} {on_keyboard_handler_change} />
                    <Editor config={self.config.clone()} default_value={self.default_code.clone()} default_value_serial={self.default_code_serial} {on_change} />
                </div>
                <div class="right">
                    <div class="right-top">
                        <h3>{ "Output:" }</h3>
                        <AnsiOutput text={self.output.clone()} />
                    </div>
                    <div class="right-bottom">
                        <div class="graph">
                            <h3>{ "Type-Graph Before typeck:" }</h3>
                            <VizGraph content={self.type_graph_before.clone()} />
                        </div>
                        <div class="graph">
                            <h3>{ "Type-Graph After typeck:" }</h3>
                            <VizGraph content={self.type_graph_after.clone()} />
                        </div>
                    </div>
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

#[derive(Properties, PartialEq)]
struct VizProperties {
    content: Option<String>,
}
#[function_component(VizGraph)]
fn viz_graph(props: &VizProperties) -> Html {
    let viz_counter_outer = use_state(|| 0);
    let viz = use_memo(
        |_| Viz::new("/static/full.render.js".to_string()),
        *viz_counter_outer,
    );

    let svg_outer = use_state(|| Option::<Node>::None);
    let content = props.content.clone();
    let svg = svg_outer.clone();
    let viz_counter = viz_counter_outer.clone();
    let _ = use_effect_with_deps(
        move |_| {
            let Some(graph) = content else { return };
            let promise = async move {
                let res = viz.render_svg_element(graph.clone()).await;
                match res {
                    Ok(s) => svg.set(Some(s.dyn_into().unwrap())),
                    Err(e) => {
                        viz_counter.set(*viz_counter + 1);
                        log::error!("{e:?}")
                    },
                }
            };
            wasm_bindgen_futures::spawn_local(promise);
        },
        props.content.clone(),
    );

    let svgpanzoom_outer = use_state(|| None);
    let svg = svg_outer.clone();
    let svgpanzoom = svgpanzoom_outer.clone();
    use_effect_with_deps(
        move |_| {
            if let Some(svg) = (*svg).clone() {
                svgpanzoom.set(Some(svg_pan_zoom(svg.dyn_into().unwrap())));
            }
        },
        (*svg_outer).clone(),
    );

    let maximized_outer = use_state(|| false);
    let maximized = maximized_outer.clone();
    let maximize = Callback::from(move |_| maximized.set(!*maximized));

    let inner = match &*svg_outer {
        Some(e) => {
            Html::VRef(e.clone())
        },
        None => html! {},
    };


    let svgpanzoom = svgpanzoom_outer.clone();
    use_effect_with_deps(
        move |_| {
            if let Some(svgpanzoom) = (*svgpanzoom).as_ref() {
                let before = SvgPanZoomSizeState::load(svgpanzoom);
                svgpanzoom.resize();
                // keep same relative zoom
                svgpanzoom.zoom(JsValue::from(before.zoom));
                let after = SvgPanZoomSizeState::load(svgpanzoom);

                // keep the center centered via panning
                let before_center_x = before.size.width / 2.;
                let before_center_y = before.size.height / 2.;
                let after_center_x = after.size.width / 2.;
                let after_center_y = after.size.height / 2.;
                let real_center_x = (before_center_x - before.pan.x) / before.size.real_zoom;
                let real_center_y = (before_center_y - before.pan.y) / before.size.real_zoom;
                let after_pan_x = after_center_x - real_center_x * after.size.real_zoom;
                let after_pan_y = after_center_y - real_center_y * after.size.real_zoom;
                svgpanzoom.pan(serde_wasm_bindgen::to_value(&Pan { x: after_pan_x, y: after_pan_y }).unwrap());
            }
        },
        *maximized_outer
    );

    html! {
        <div class={classes!("viz-graph", if *maximized_outer { Some("maximized") } else { None })}>
            <div class="maximize-icon" onclick={maximize}>{ "\u{26F6}" }</div>
            { inner }
        </div>
    }
}

#[derive(Debug, Clone)]
struct SvgPanZoomSizeState {
    size: Size,
    pan: Pan,
    zoom: f64,
}
impl SvgPanZoomSizeState {
    fn load(svgpanzoom: &SvgPanZoom) -> Self {
        SvgPanZoomSizeState {
            size: serde_wasm_bindgen::from_value(svgpanzoom.get_sizes()).unwrap(),
            pan: serde_wasm_bindgen::from_value(svgpanzoom.get_pan()).unwrap(),
            zoom: svgpanzoom.get_zoom().as_f64().unwrap(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
struct Size {
    width: f64,
    height: f64,
    #[serde(rename = "realZoom")]
    real_zoom: f64,
    #[serde(rename = "viewBox")]
    view_box: ViewBox,
}
#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
struct ViewBox {
    width: f64,
    height: f64,
    x: f64,
    y: f64,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
struct Pan {
    x: f64,
    y: f64,
}

#[wasm_bindgen]
extern "C" {
    type Viz;
    type SvgPanZoom;

    #[wasm_bindgen(constructor)]
    fn new(path: String) -> Viz;
    #[wasm_bindgen(method, catch, js_name = "renderSVGElement")]
    async fn render_svg_element(this: &Viz, graph: String) -> Result<JsValue, JsValue>;
    #[wasm_bindgen(js_name="svgPanZoom")]
    fn svg_pan_zoom(element: JsValue) -> SvgPanZoom;
    #[wasm_bindgen(method)]
    fn resize(this: &SvgPanZoom);
    #[wasm_bindgen(method)]
    fn fit(this: &SvgPanZoom);
    #[wasm_bindgen(method)]
    fn center(this: &SvgPanZoom);
    #[wasm_bindgen(method, js_name="getPan")]
    fn get_pan(this: &SvgPanZoom) -> JsValue;
    #[wasm_bindgen(method, js_name="pan")]
    fn pan(this: &SvgPanZoom, xy: JsValue);
    #[wasm_bindgen(method, js_name="getZoom")]
    fn get_zoom(this: &SvgPanZoom) -> JsValue;
    #[wasm_bindgen(method, js_name="zoom")]
    fn zoom(this: &SvgPanZoom, xy: JsValue);
    #[wasm_bindgen(method, js_name="getSizes")]
    fn get_sizes(this: &SvgPanZoom) -> JsValue;
}
