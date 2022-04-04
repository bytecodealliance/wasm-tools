(module
  (memory $memory (export "memory") 1)
  (func $canonical_abi_realloc (export "canonical_abi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  (func $canonical_abi_free (export "canonical_abi_free") (param i32 i32 i32) unreachable)
  (func (export "a") unreachable)
  (func (export "b") (result i32) unreachable)
  (func (export "c") (param i32 i32) (result i32) unreachable)
  (func (export "d") (param i32 i32) unreachable)
)