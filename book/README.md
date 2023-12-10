# rebo Book

Here lie the contents of the rebo book, to be found at <https://rebo.oberien.de/book/>.

## Prerequisites
* mdbook
  * install via package manager (e.g. `pacman -S mdbook`)
  * or see <https://rust-lang.github.io/mdBook/guide/installation.html>
* compiled rebo in `../target/release/rebo`
  * `cd ..; cargo build --release`

## Build locally

```sh
mdbook serve --open
```

## Deploy

```sh
mdbook build
scp -r book/ server:folder
```