(module
  (import "$root" "returns-string" (func (param i64)))
  (import "$root" "takes-string" (func (param i64 i64)))

  (func (export "__wasm_task_hook") (param i32) unreachable)

  (func (export "takes-and-returns") (param i64 i64) (result i64) unreachable)
  (func (export "cabi_post_takes-and-returns") (param i64) unreachable)
  (func (export "no-realloc"))
  (func (export "x#with-string") (param i64 i64) unreachable)
  (func (export "x#[dtor]r") (param i64) unreachable)

  (func (export "cabi_import_realloc") (param i64 i64 i64 i64) (result i64) unreachable)
  (func (export "cabi_export_realloc") (param i64 i64 i64 i64) (result i64) unreachable)

  (memory (export "memory") i64 1)
)
