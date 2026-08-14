(module
  (import "env" "__wasm_get_tls_base" (func (result i32)))
  (import "env" "__wasm_set_tls_base" (func (param i32)))
  (import "$root" "bar" (func (param i32)))

  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)

  (memory (export "memory") 1)
)
