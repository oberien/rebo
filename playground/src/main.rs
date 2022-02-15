use yew::{function_component, html, use_state, Html, Properties, use_effect_with_deps};
use wasm_bindgen::{JsValue, closure::Closure, prelude::wasm_bindgen};
use web_sys::Element;
use wasm_logger::Config;
use gloo_storage::{LocalStorage, Storage};
use log::Level;

mod rebo;

fn main() {
    wasm_logger::init(Config::new(Level::Info));
    yew::start_app::<HelloWorld>();
}

const DEFAULT_CODE: &str = r#"print("Hello, world!");"#;

#[function_component(HelloWorld)]
fn hello_world() -> Html {
    let value = use_state(|| String::new());
    let output = use_state(|| String::new());

    {
        let value = value.clone();
        use_effect_with_deps(move |_| {
            let code: Result<String, _> = LocalStorage::get("code");
            let code = code.ok()
                .and_then(|s| if s.is_empty() { None } else { Some(s) })
                .unwrap_or_else(|| DEFAULT_CODE.to_string());
            value.set(code.clone());

            let on_change = {
                Closure::wrap(Box::new(move |s: String| {
                    value.set(s);
                }) as Box<dyn FnMut(String)>).into_js_value()
            };
            ace_editor_component_did_mount(on_change, code);
            || ()
        }, ());
    }

    {
        let value = value.clone();
        let dependency = (*value).clone();
        let output = output.clone();
        use_effect_with_deps(
            move |_| {
                output.set(rebo::run_rebo((*value).clone()));
                LocalStorage::set("code", &*value).unwrap();
                || ()
            },
            dependency,
        );
    }

    html! {
        <>
        <h1>{ "Rebo Playground" }</h1>
        <div class="outer">
            <AceEditor />
            <div class="output">
                <h3>{ "Output: "}</h3>
                <AnsiOutput text={(*output).clone()} />
            </div>
        </div>
        </>
    }
}

#[derive(Properties, PartialEq)]
struct AnsiOutputProps {
    text: String,
}

#[function_component(AnsiOutput)]
fn ansi_output(props: &AnsiOutputProps) -> Html {
    let rendered = ansi_to_html::convert_escaped(&props.text).unwrap();
    let pre = gloo_utils::document().create_element("pre").unwrap();
    pre.set_inner_html(&rendered);
    Html::VRef(pre.into())
}

#[function_component(AceEditor)]
fn ace_editor() -> Html {
    let div = gloo_utils::document().create_element("div").unwrap();
    div.set_class_name("code");
    ace_editor_create(&div);
    Html::VRef(div.into())
}

#[wasm_bindgen(module = "/src/ace-editor.js")]
extern "C" {
    #[wasm_bindgen(js_name = "ace_editor_create")]
    fn ace_editor_create(node: &Element);
    #[wasm_bindgen(js_name = "ace_editor_component_did_mount")]
    fn ace_editor_component_did_mount(on_change: JsValue, default_value: String);
}
