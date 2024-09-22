pub fn xdot(graph: &str) {
    use std::process::{Command, Stdio};
    use std::io::Write;
    let mut xdot = Command::new("xdot")
        .arg("-")
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .unwrap();
    let mut stdin = xdot.stdin.take().unwrap();
    stdin.write_all(graph.as_bytes()).unwrap();
    stdin.flush().unwrap();
    drop(stdin);
    xdot.wait().unwrap();
}
