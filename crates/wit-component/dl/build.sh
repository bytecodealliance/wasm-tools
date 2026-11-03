# This script will rebuild libdl32.so and libdl64.so from source, writing them
# to the directory given as the first argument.
#
# Example: bash build.sh ..

set -ex

out=$1
target_dir=../../../target/dl
wasm_ld=$(rustc --print sysroot)/lib/rustlib/$(rustc -vV | sed -n 's/^host: //p')/bin/gcc-ld/wasm-ld

build() {
  arch=$1
  output=$2
  RUSTFLAGS="-C relocation-model=pic" \
    CARGO_PROFILE_RELEASE_LTO=true \
    CARGO_PROFILE_RELEASE_PANIC=abort \
    CARGO_TARGET_DIR=$target_dir \
    cargo build --release --target=$arch-unknown-unknown -Zbuild-std=core
  $wasm_ld -m$arch -shared --experimental-pic \
    -o $output \
    --whole-archive \
    $target_dir/$arch-unknown-unknown/release/libdl.a \
    --no-whole-archive \
    --allow-undefined
  cargo run --manifest-path ../../../Cargo.toml -- strip $output -o $output
  cargo run --manifest-path ../../../Cargo.toml -- strip --delete name $output -o $output
}

build wasm32 $out/libdl32.so
build wasm64 $out/libdl64.so
