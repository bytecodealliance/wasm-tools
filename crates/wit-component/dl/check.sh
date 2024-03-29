bash ./build.sh ../../../target/wasm32-wasi/release/tmp.so
if diff ../../../target/wasm32-wasi/release/tmp.so ../libdl.so; then
  exit 0
else
  echo "libdl.so is out-of-date; please run crates/wit-component/dl/build.sh to update it">&2
  exit 1
fi
