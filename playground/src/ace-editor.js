const editorRef = React.createRef();

export function ace_editor_create(node) {
    let element = React.createElement('div', {
        ref: editorRef,
        id: "editor",
    });
    ReactDOM.render(element, node);
}
export function ace_editor_component_did_mount(onChange, defaultValue) {
    console.log(editorRef);
    let editor = ace.edit(editorRef.current, {
        mode: "ace/mode/rust",
        theme: "ace/theme/monokai",
        selectionStyle: "text",
        value: defaultValue,
    });

    editor.session.on('change', () => onChange(editor.getValue()));
}
