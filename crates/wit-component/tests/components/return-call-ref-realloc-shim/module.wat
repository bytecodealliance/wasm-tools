;;! return-call-ref = true

(module
  (import "env" "memory" (memory 1))
  (import "$root" "returns-string" (func (param i32)))
  (import "$root" "takes-string" (func (param i32 i32)))

  (func (export "__wasm_task_hook") (param i32) unreachable)
  (func (export "_initialize"))

  (func (export "takes-and-returns") (param i32 i32) (result i32) unreachable)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
