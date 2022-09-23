(module
  (import "foo" "a: func() -> ()" (func))
  (import "bar" "a: func(x: u64, y: string) -> ()" (func (param i64 i32 i32)))
  (import "baz" "baz: func(x: list<u8>) -> list<u8>" (func (param i32 i32 i32)))
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
)