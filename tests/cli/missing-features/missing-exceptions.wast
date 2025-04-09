;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm1

(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00"
    "\0d\01\00"
    )
  "exceptions proposal not enabled")

(assert_malformed
  (module
    (import "" "" (tag))
    )
  "exceptions proposal not enabled")
