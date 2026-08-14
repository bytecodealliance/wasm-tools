(module
  (import "$root" "takes-string" (func (param i32 i32)))

  (func (export "__wasm_task_hook") (param i32) unreachable)
  (func (export "run"))
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)

  (memory (export "memory") 1)
)
