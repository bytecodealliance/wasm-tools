(module
  (memory $memory (export "memory") 1)
  (func $canonical_abi_realloc (export "canonical_abi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  (func $a (export "a") unreachable)
  (func $b (export "b") (result i32) unreachable)
  (func $c (export "c") (param i32 i32) (result i32) unreachable)
  (func $d (export "d") (param i32 i32) unreachable)
)