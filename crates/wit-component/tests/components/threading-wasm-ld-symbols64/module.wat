(module
  (import "env" "__wasm_get_stack_pointer" (func (result i64)))
  (import "env" "__wasm_set_stack_pointer" (func (param i64)))
  (import "env" "__wasm_get_tls_base" (func (result i64)))
  (import "env" "__wasm_set_tls_base" (func (param i64)))
)
