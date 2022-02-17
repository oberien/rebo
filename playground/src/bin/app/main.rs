use yew::{function_component, html, use_state, Html, Properties, use_effect_with_deps, Callback, use_ref, use_mut_ref};
use gloo_storage::{LocalStorage, Storage};

mod ace_editor;

use ace_editor::AceEditor;
use web_sys::{window, Worker, MessageEvent};
use wasm_bindgen::closure::Closure;
use std::rc::Rc;
use js_sys::Function;
use wasm_bindgen::JsValue;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

fn main() {
    playground::setup();
    yew::start_app::<HelloWorld>();
}

fn worker_new() -> Worker {
    let origin = window().unwrap().location().origin().unwrap();
    let url = format!("{}/worker.js", origin);

    Worker::new(&url).expect("failed to spawn worker")
}

const DEFAULT_CODE: &str = r#"print("Hello, world!");"#;

#[function_component(HelloWorld)]
fn hello_world() -> Html {
    let code = use_mut_ref(|| {
        let code: Result<String, _> = LocalStorage::get("code");
        code.ok()
            .and_then(|s| if s.is_empty() { None } else { Some(s) })
            .unwrap_or_else(|| DEFAULT_CODE.to_string())
    });
    let value = use_state(|| code.borrow().clone());
    let output_string = use_mut_ref(|| String::new());
    let output = use_state(|| String::new());
    let worker = use_mut_ref(|| worker_new());

    let on_change = {
        let value = value.clone();
        let code = code.clone();
        Callback::from(move |s: String| {
            value.set(s.clone());
            *code.borrow_mut() = s;
        })
    };

    {
        let code = code.clone();
        let dependency = (*value).clone();
        let output = output.clone();
        let worker = worker.clone();
        let onmessage: Rc<Function> = {
            let output_string = output_string.clone();
            let output = output.clone();
            let worker = worker.clone();
            use_ref(move || Closure::wrap(Box::new(move |msg: MessageEvent| {
                if let Some(b) = msg.data().as_bool() {
                    match b {
                        false => {
                            log::warn!("worker started");
                            worker.borrow().post_message(&JsValue::from(&*code.borrow())).unwrap();
                        }
                        true => {
                            log::warn!("worker finished");
                            LocalStorage::set("code", &*code.borrow()).unwrap();
                        }
                    }
                } else {
                    let out = msg.data().as_string().unwrap();
                    output_string.borrow_mut().push_str(&out);
                    output.set(output_string.borrow().clone());
                }
            }) as Box<dyn Fn(MessageEvent)>).into_js_value().into())
        };
        use_effect_with_deps(
            move |_| {
                worker.borrow().terminate();
                *worker.borrow_mut() = worker_new();
                worker.borrow().set_onmessage(Some(&*onmessage));
                log::warn!("created");
                *output_string.borrow_mut() = String::new();
                output.set(String::new());
                || ()
            },
            dependency,
        );
    }

    return html! {
        <>
        <h1>{ "Rebo Playground" }</h1>
        <div class="outer">
            <AceEditor default_value={code.borrow().clone()} {on_change} />
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
