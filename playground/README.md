# Rebo Playground

## Running / Bundling

* `rustup target add wasm32-unknown-unknown`
* `cargo install trunk`
* `trunk serve --release`

## Deploy

* `trunk build --release`
* `scp dist/* server:folder`