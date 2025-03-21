;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1

(assert_invalid
  (module binary
    "\00asm"
    "\0d\00\01\00"
    )
  "unknown binary version and encoding combination: 0xd and 0x1, note: encoded as a component but the WebAssembly component model feature is not enabled - enable the feature to allow component validation")
