(module
  (func (export "__wasm_init_task") unreachable)
  (func (export "__wasm_init_async_task") unreachable)
  (func (export "[async-lift-stackful]foo") unreachable)
  (func (export "bar") unreachable)
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)
