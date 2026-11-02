(module
  (import "new" "get-two" (func $get_two (param i64)))
  (import "new" "read" (func $read (param i64)))
  ;; The main module doesn't export `cabi_realloc`, so `wit-component`
  ;; synthesizes one in terms of `memory.grow`.
  (import "__main_module__" "cabi_realloc" (func $cabi_realloc (param i64 i64 i64 i64) (result i64)))
  (import "env" "memory" (memory i64 0))

  ;; Without a `realloc` in the main module the stack is allocated eagerly
  ;; with `memory.grow` in a start function.
  (global $__stack_pointer (mut i64) i64.const 0)

  ;; Use the synthesized realloc for the canonical ABI as well.
  (func (export "cabi_import_realloc") (param i64 i64 i64 i64) (result i64)
    (call $cabi_realloc (local.get 0) (local.get 1) (local.get 2) (local.get 3))
  )

  (func (export "get_sum") (result i32)
    (local i64 i64)

    global.get $__stack_pointer
    local.tee 0
    i64.const 8
    i64.sub
    local.tee 1
    global.set $__stack_pointer

    local.get 1
    call $get_two
    local.get 1
    call $read

    (i32.add
      (i32.load (local.get 1))
      (i32.load offset=4 (local.get 1)))

    local.get 0
    global.set $__stack_pointer
  )
)
