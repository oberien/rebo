use yew::{html, function_component, Callback, Properties};
use ace_editor::AceEditor;
use text_area_editor::TextAreaEditor;
use crate::config::{Config, EditorType};

mod ace_editor;
mod text_area_editor;

#[derive(Properties, PartialEq)]
pub struct EditorProps {
    pub config: Config,
    pub default_value: String,
    pub default_value_serial: i32,
    pub on_change: Callback<String>,
}

#[function_component(Editor)]
pub fn editor(props: &EditorProps) -> Html {
    html! {
        if props.config.editor == EditorType::Ace {
            <AceEditor config={props.config.clone()} default_value={props.default_value.clone()} default_value_serial={props.default_value_serial} on_change={props.on_change.clone()} />
        } else {
            <TextAreaEditor config={props.config.clone()} default_value={props.default_value.clone()} default_value_serial={props.default_value_serial} on_change={props.on_change.clone()} />
        }
    }
}
