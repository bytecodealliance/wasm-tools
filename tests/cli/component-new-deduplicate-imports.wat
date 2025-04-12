;; RUN: component new % -o component.wasm --adapt tests/cli/wasi_snapshot_preview1.wasm --deduplicate-imports
(module
  ;; The second of these imports gets stripped out, letting `component new` succeed.
  (func $i (import "wasi_snapshot_preview1" "proc_exit") (param i32))
  (func $j (import "wasi_snapshot_preview1" "proc_exit") (param i32))
  (func (export "exported_func")
    i32.const 42
    call $i
    i32.const 42
    call $j
  )
  (memory (;0;) 1)
  (export "memory" (memory 0))
)
