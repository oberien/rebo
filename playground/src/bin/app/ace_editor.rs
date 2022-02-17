use yew::{function_component, use_node_ref, use_effect_with_deps, html, Properties, Callback};
use web_sys::Element;
use wasm_bindgen::prelude::wasm_bindgen;
use serde::Serialize;
use wasm_bindgen::JsValue;
use wasm_bindgen::closure::Closure;

#[derive(Properties, PartialEq)]
pub struct AceEditorProps {
    pub default_value: String,
    pub on_change: Callback<String>,
}

#[function_component(AceEditor)]
pub fn ace_editor(props: &AceEditorProps) -> Html {
    let editor_ref = use_node_ref();

    {
        let editor_ref = editor_ref.clone();
        let deps = editor_ref.clone();
        let default_value = props.default_value.clone();
        let on_change = props.on_change.clone();
        use_effect_with_deps(move |_| {
            let editor = ace_edit(
                &editor_ref.cast::<Element>().unwrap(),
                JsValue::from_serde(&AceEditorConfig {
                    mode: "ace/mode/rust".to_string(),
                    theme: "ace/theme/idle_fingers".to_string(),
                    selectionStyle: "text".to_string(),
                    value: default_value,
                }).unwrap(),
            );

            let on_change = {
                let editor = editor.clone();
                Closure::wrap(Box::new(move || {
                    on_change.emit(editor.ace_get_value());
                }) as Box<dyn FnMut()>).into_js_value()
            };
            editor.ace_session_on("change".to_string(), on_change);
            || ()
        }, deps);
    }

    return html! {
        <div class="code" ref={editor_ref}></div>
    }
}

#[derive(Serialize)]
#[allow(non_snake_case)]
struct AceEditorConfig {
    mode: String,
    theme: String,
    selectionStyle: String,
    value: String,
}

#[wasm_bindgen]
extern "C" {
    #[derive(Clone)]
    type Editor;

    #[wasm_bindgen(js_name = "ace.edit")]
    fn ace_edit(node: &Element, cfg: JsValue) -> Editor;
    #[wasm_bindgen(method, js_name = "session.on")]
    fn ace_session_on(this: &Editor, target: String, on_change: JsValue);
    #[wasm_bindgen(method, js_name = "getValue")]
    fn ace_get_value(this: &Editor) -> String;
}

