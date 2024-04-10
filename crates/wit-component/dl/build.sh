# This script will rebuild libdl.so from source.
#
# To run this, you'll need to use `wasi-sdk` 21 or later, installed at
# $WASI_SDK_PATH.
#
# Example: WASI_SDK_PATH=/opt/wasi-sdk bash build.sh ../libdl.so

CARGO_PROFILE_RELEASE_LTO=true RUSTFLAGS="-C relocation-model=pic" cargo build --release --target=wasm32-wasi
$WASI_SDK_PATH/bin/clang -shared -o $1 -Wl,--whole-archive ../../../target/wasm32-wasi/release/libdl.a -Wl,--no-whole-archive
cargo run --manifest-path ../../../Cargo.toml -- strip $1 -o $1
cargo run --manifest-path ../../../Cargo.toml -- strip --delete name $1 -o $1
