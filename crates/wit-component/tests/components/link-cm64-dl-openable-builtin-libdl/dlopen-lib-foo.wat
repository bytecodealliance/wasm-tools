(module
  (@dylink.0
    (mem-info (memory 0 4))
  )
  (type (func (param i32) (result i32)))
  (type (func (param i64 i32) (result i64)))
  (import "env" "memory" (memory i64 1))
  (import "test:test/test" "foo" (func $import_foo (type 0)))
  (import "env" "dlopen" (func $dlopen (type 1)))
  (func $foo (type 0) (param i32) (result i32)
    unreachable
  )
  (global $what i64 i64.const 42)
  (export "test:test/test#foo" (func $foo))
  (export "bar" (func $foo))
  (export "baz" (func $foo))
  (export "what" (global $what))
)
