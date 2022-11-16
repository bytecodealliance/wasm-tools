(module
  ;; import something from an external interface
  (import "foo" "foo" (func))

  ;; import some wasi functions
  (import "wasi_snapshot_preview1" "proc_exit" (func (param i32)))
  (import "wasi_snapshot_preview1" "random_get" (func (param i32 i32) (result i32)))

  ;; required by wasi
  (memory (export "memory") 1)
)
