# Rebo Playground

## Running / Bundling

* `rustup target add wasm32-unknown-unknown`
* `cargo install trunk --git https://github.com/kristoff3r/trunk --branch rust_worker`
    * <https://github.com/thedodd/trunk/pull/285>
* `trunk serve`

## Deploy

* `trunk build --release`
* `scp -r dist/* server:folder`