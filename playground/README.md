# Rebo Playground

## Running / Bundling

* `rustup target add wasm32-unknown-unknown`
* `cargo install trunk --git https://github.com/oberien/trunk --branch master`
    * <https://github.com/thedodd/trunk/pull/285>
    * <https://github.com/thedodd/trunk/pull/322>
* `trunk serve`

## Deploy

* `trunk build --release`
* `scp -r dist/* server:folder`
* the server must send the headers:
  ```
  Cross-Origin-Opener-Policy: same-origin
  Cross-Origin-Embedder-Policy: require-corp
  ```