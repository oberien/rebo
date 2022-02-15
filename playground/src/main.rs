use yew::{function_component, html, use_state, Callback, Event, Html, Properties, use_effect_with_deps, InputEvent};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::HtmlTextAreaElement;
use rebo::{ReboConfig, IncludeDirectoryConfig, ReturnValue, Output, Stdlib, DisplayValue, ExecError};
use wasm_logger::Config;
use log::Level;
use std::cell::RefCell;
use std::rc::Rc;
use once_cell::sync::Lazy;
use std::sync::Mutex;
use itertools::Itertools;
use gloo_storage::{LocalStorage, Storage};

fn main() {
    wasm_logger::init(Config::new(Level::Info));
    yew::start_app::<HelloWorld>();
}

static PRINTOUT: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new(String::new()));
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
                .unwrap_or(DEFAULT_CODE.to_string());
            value.set(code);
            || ()
        }, ());
    }

    let oninput = {
        let value = value.clone();
        Callback::from(move |event: InputEvent| {
            let event: Event = event.dyn_into().unwrap_throw();
            let event_target = event.target().unwrap_throw();
            let target: HtmlTextAreaElement = event_target.dyn_into().unwrap_throw();
            value.set(target.value());
        })
    };

    {
        let value = value.clone();
        let dependency = (*value).clone();
        let output = output.clone();
        use_effect_with_deps(
            move |_| {
                let out = Rc::new(RefCell::new(String::new()));
                let ret = {
                    let res_string = Rc::clone(&out);
                    let config = ReboConfig::new()
                        .include_directory(IncludeDirectoryConfig::Everywhere)
                        .diagnostic_output({
                            Output::buffered(move |s| *res_string.borrow_mut() += &s)
                        })
                        .stdlib(Stdlib::all() - Stdlib::PRINT)
                        .interrupt_interval(1_000_000)
                        .interrupt_function(|_| {
                            PRINTOUT.lock().unwrap().push_str("\x1b[31;1;4mExecution took too long. Killed\x1b[0m");
                            Err(ExecError::Panic)
                        })
                        .add_function(print)
                        ;
                    PRINTOUT.lock().unwrap().clear();
                    rebo::run_with_config("file.re".to_string(), (*value).clone(), config)
                };
                *out.borrow_mut() += &PRINTOUT.lock().unwrap();
                match ret {
                    ReturnValue::Ok => *out.borrow_mut() += "\n\nExecution successful.",
                    ReturnValue::Diagnostics(i) => *out.borrow_mut() += &format!("\n\n{i} diagnostics."),
                    ReturnValue::ParseError => *out.borrow_mut() += "\n\nParse error.",
                }
                output.set(Rc::try_unwrap(out).unwrap().into_inner());
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
            <textarea class="code" value={(*value).clone()} {oninput}>{ "Increment" }</textarea>
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

#[rebo::function(raw("print"))]
fn print(..: _) {
    let joined = args.as_slice().into_iter().map(|arg| DisplayValue(arg)).join(", ");
    PRINTOUT.lock().unwrap().push_str(&joined);
}
