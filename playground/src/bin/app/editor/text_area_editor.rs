use yew::{function_component, use_state, InputEvent, Event, html, Callback, use_effect_with_deps, Html};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use web_sys::HtmlTextAreaElement;
use crate::editor::EditorProps;

#[function_component(TextAreaEditor)]
pub fn text_area_editor(props: &EditorProps) -> Html {
    let value = use_state(|| props.default_value.clone());

    {
        let value = value.clone();
        let default_value = props.default_value.clone();
        use_effect_with_deps(move |_| {
            value.set(default_value);
            || ()
        }, props.default_value_serial);
    }

    let oninput = {
        let value = value.clone();
        let on_change = props.on_change.clone();
        Callback::from(move |event: InputEvent| {
            let event: Event = event.dyn_into().unwrap_throw();
            let event_target = event.target().unwrap_throw();
            let target: HtmlTextAreaElement = event_target.dyn_into().unwrap_throw();
            let val = target.value();
            // actually a noop as the textarea's content contains that value
            value.set(val.clone());
            on_change.emit(val);
        })
    };

    html! {
       <textarea class="code" value={(*value).clone()} {oninput}></textarea>
    }
}