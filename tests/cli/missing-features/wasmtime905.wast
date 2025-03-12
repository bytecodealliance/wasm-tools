;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1

(assert_invalid
    (module (func (local externref)))
    "reference types support is not enabled"
)
