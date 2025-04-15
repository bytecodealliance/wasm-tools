;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1

(assert_invalid
  (module (elem func))
  "bulk memory must be enabled"
)

(assert_invalid
  (module (elem declare func))
  "bulk memory must be enabled"
)
