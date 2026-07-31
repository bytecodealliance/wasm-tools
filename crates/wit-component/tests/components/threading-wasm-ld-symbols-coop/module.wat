(module
  (import "env" "__wasm_get_stack_pointer" (func (result i32)))
  (import "env" "__wasm_set_stack_pointer" (func (param i32)))
  (import "env" "__wasm_get_tls_base" (func (result i32)))
  (import "env" "__wasm_set_tls_base" (func (param i32)))
  (import "$root" "[thread-new-indirect-v0]" (func (param i32 i32) (result i32)))
  (table (export "__indirect_function_table") 1 funcref)
)
