;; options: deduplicate-imports
(module
  ;; Import a wasi function twice.
  (func $i (import "wasi-snapshot-preview1" "proc_exit") (param i32))
  (func $j (import "wasi-snapshot-preview1" "proc_exit") (param i32))

  ;; Call both copies of the import. They should both end up pointed to the same one.
  (func
    i32.const 42
    call $i
    i32.const 42
    call $j
  )

  ;; Required by wasi
  (memory (export "memory") 1)
)
