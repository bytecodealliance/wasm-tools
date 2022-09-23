(module
  (import "foo" "a: func() -> string" (func (param i32)))
  (memory (export "memory") 1)
  (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  (func (export "a: func(x: string) -> tuple<string, u32, string>") (param i32 i32) (result i32) unreachable)
  (func (export "bar#a: func() -> ()") unreachable)
  (func (export "bar#b: func() -> string") (result i32) unreachable)
)