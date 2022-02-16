use yew::{function_component, html, use_state, Html, Properties, use_effect_with_deps, Callback, use_ref};
use wasm_logger::Config;
use gloo_storage::{LocalStorage, Storage};
use log::Level;

mod rebo;
mod ace_editor;

use ace_editor::AceEditor;

fn main() {
    wasm_logger::init(Config::new(Level::Info));
    yew::start_app::<HelloWorld>();
}

const DEFAULT_CODE: &str = r#"print("Hello, world!");"#;

#[function_component(HelloWorld)]
fn hello_world() -> Html {
    let code = use_ref(|| {
        let code: Result<String, _> = LocalStorage::get("code");
        code.ok()
            .and_then(|s| if s.is_empty() { None } else { Some(s) })
            .unwrap_or_else(|| DEFAULT_CODE.to_string())
    });
    let value = use_state(|| (*code).clone());
    let output = use_state(|| String::new());

    let on_change = {
        let value = value.clone();
        Callback::from(move |s: String| {
            value.set(s);
        })
    };

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
            <AceEditor default_value={(*code).clone()} {on_change} />
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
