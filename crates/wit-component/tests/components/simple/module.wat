(module
  (memory $memory (export "memory") 1)
  (func $cabi_realloc (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  (func $a (export "a: func() -> ()") unreachable)
  (func $b (export "b: func() -> string") (result i32) unreachable)
  (func $c (export "c: func(x: string) -> string") (param i32 i32) (result i32) unreachable)
  (func $d (export "d: func(x: list<string>) -> ()") (param i32 i32) unreachable)
)