(module
  (import "$root" "returns-string" (func (param i32)))
  (import "$root" "takes-string" (func (param i32 i32)))

  (func (export "__wasm_task_hook") (param i32) unreachable)

  (func (export "takes-and-returns") (param i32 i32) (result i32) unreachable)
  (func (export "cabi_post_takes-and-returns") (param i32) unreachable)
  (func (export "no-realloc"))
  (func (export "x#with-string") (param i32 i32) unreachable)
  (func (export "x#[dtor]r") (param i32) unreachable)

  (func (export "cabi_import_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  (func (export "cabi_export_realloc") (param i32 i32 i32 i32) (result i32) unreachable)

  (memory (export "memory") 1)
)
