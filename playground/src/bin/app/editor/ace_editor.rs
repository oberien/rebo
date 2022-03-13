use yew::{function_component, use_node_ref, use_effect_with_deps, use_mut_ref, html};
use web_sys::Element;
use wasm_bindgen::prelude::wasm_bindgen;
use serde::Serialize;
use wasm_bindgen::JsValue;
use wasm_bindgen::closure::Closure;
use crate::editor::EditorProps;

#[function_component(AceEditor)]
pub fn ace_editor(props: &EditorProps) -> Html {
    let editor_ref = use_node_ref();
    let editor = use_mut_ref(|| Editor::default());

    {
        let editor_ref = editor_ref.clone();
        let editor_inner = editor.clone();
        let default_value = props.default_value.clone();
        let on_change = props.on_change.clone();
        let keyboard_handler = props.config.keyboard_handler;
        let deps = editor_ref.clone();
        use_effect_with_deps(move |_| {
            let editor = ace_edit(
                &editor_ref.cast::<Element>().expect("cast editor to Element failed"),
                JsValue::from_serde(&AceEditorConfig {
                    mode: "ace/mode/rust",
                    theme: "ace/theme/idle_fingers",
                    selectionStyle: "text",
                    value: default_value,
                    keyboardHandler: keyboard_handler.to_ace_str(),
                }).expect("JsValue::from_serde(AceEditorConfig) failed"),
            );

            let on_change = {
                let editor = editor.clone();
                Closure::wrap(Box::new(move || {
                    on_change.emit(editor.ace_get_value());
                }) as Box<dyn FnMut()>).into_js_value()
            };
            editor.ace_session_on("change".to_string(), on_change);
            *editor_inner.borrow_mut() = editor;
            || ()
        }, deps);
    }

    {
        let keyboard_handler = props.config.keyboard_handler;
        let editor = editor.clone();
        use_effect_with_deps(move |_| {
            editor.borrow().ace_set_keyboard_handler(keyboard_handler.to_ace_str());
            || ()
        }, keyboard_handler)
    }

    {
        let editor = editor.clone();
        let default_value = props.default_value.clone();
        use_effect_with_deps(move |_| {
            log::error!("deps changed");
            editor.borrow().ace_set_value(&default_value, 1);
            || ()
        }, props.default_value_serial);
    }

    return html! {
        <div class="code" ref={editor_ref}></div>
    }
}

#[derive(Serialize)]
#[allow(non_snake_case)]
struct AceEditorConfig {
    mode: &'static str,
    theme: &'static str,
    selectionStyle: &'static str,
    value: String,
    keyboardHandler: Option<&'static str>,
}

#[wasm_bindgen]
extern "C" {
    #[derive(Clone, Default)]
    type Editor;

    #[wasm_bindgen(js_name = "ace.edit")]
    fn ace_edit(node: &Element, cfg: JsValue) -> Editor;
    #[wasm_bindgen(method, js_name = "session.on")]
    fn ace_session_on(this: &Editor, target: String, on_change: JsValue);
    #[wasm_bindgen(method, js_name = "getValue")]
    fn ace_get_value(this: &Editor) -> String;
    #[wasm_bindgen(method, js_name = "setValue")]
    fn ace_set_value(this: &Editor, value: &str, pos: i32);
    #[wasm_bindgen(method, js_name = "setKeyboardHandler")]
    fn ace_set_keyboard_handler(this: &Editor, keyboard_handler: Option<&str>);
}

