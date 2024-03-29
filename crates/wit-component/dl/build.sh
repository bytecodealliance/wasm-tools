# This script will rebuild libdl.so from source.
#
# To run this, you'll need to use Rust nightly 2023-07-27 or later, including
# the `wasm32-wasi` target, plus `wasi-sdk` 21 or later, installed at
# $WASI_SDK_PATH.
#
# Example: WASI_SDK_PATH=/opt/wasi-sdk RUST_TOOLCHAIN=+nightly bash build.sh ../libdl.so

RUSTFLAGS="-C relocation-model=pic" cargo $RUST_TOOLCHAIN build -Z build-std=panic_abort,std --release --target=wasm32-wasi
$WASI_SDK_PATH/bin/clang -shared -o $1 -Wl,--whole-archive ../../../target/wasm32-wasi/release/libdl.a -Wl,--no-whole-archive
