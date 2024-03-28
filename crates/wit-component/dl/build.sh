# This script will rebuild libdl.so from source.
#
# To run this, you'll need Rust nightly 2023-07-27 or later, including the
# `wasm32-wasi` target, plus `wasi-sdk` 21 or later, installed at $WASI_SDK_PATH

RUSTFLAGS="-C relocation-model=pic" cargo +nightly build -Z build-std=panic_abort,std --release --target=wasm32-wasi
$WASI_SDK_PATH/bin/clang -shared -o libdl.so -Wl,--whole-archive target/wasm32-wasi/release/libdl.a -Wl,--no-whole-archive
