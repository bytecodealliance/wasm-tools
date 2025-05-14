;; options: deduplicate-imports
(module
  ;; Import a wasi function twice.
  (func $exit1 (import "wasi-snapshot-preview1" "proc_exit") (param i32))
  (func $exit2 (import "wasi-snapshot-preview1" "proc_exit") (param i32))

  ;; define `f2` before the second `f` import shows up
  (import "cm32p2" "f" (func $f_v1))
  (import "cm32p2" "f2" (func $f2 (result i32)))
  (import "cm32p2" "f" (func $f_v2))
  (import "cm32p2" "f" (func $f_v3))

  ;; define two `g` imports before the "real" `g2` import shows up
  (import "cm32p2" "g" (func $g_v1))
  (import "cm32p2" "g" (func $g_v2))
  (import "cm32p2" "g2" (func $g2 (result i32)))

  ;; Call both copies of the import. They should both end up pointed to the same one.
  (func
    call $f_v1
    call $f_v2
    call $f_v3
    call $f2
    drop

    call $g_v1
    call $g_v2
    call $g2
    drop

    i32.const 42
    call $exit1
    i32.const 42
    call $exit2
  )

  ;; Required by wasi
  (memory (export "memory") 1)
)
