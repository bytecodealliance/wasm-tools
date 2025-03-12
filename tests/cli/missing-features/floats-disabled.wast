;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1,-floats

(assert_invalid
  (module
    (func (param f32))
  )
  "floating-point support is disabled")
(assert_invalid
  (module
    (func (local f32))
  )
  "floating-point support is disabled")
