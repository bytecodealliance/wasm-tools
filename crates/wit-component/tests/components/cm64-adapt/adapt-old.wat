(module
  (import "new" "get-two" (func $get_two (param i64)))
  (import "new" "log" (func $log (param i64 i64)))
  (import "new" "read" (func $read (param i32 i64)))
  (import "env" "memory" (memory i64 0))

  (global $__stack_pointer (mut i64) i64.const 0)
  (global $some_other_mutable_global (mut i32) i32.const 0)

  ;; `wit-component` should use this to track the status of a lazy stack
  ;; allocation, which is done with `cabi_realloc_adapter` from the main
  ;; module. Note that this adapter doesn't import `cabi_realloc` itself so
  ;; `wit-component` must inject the import.
  (global $allocation_state (mut i32) i32.const 0)

  (func (export "get_sum") (result i32)
    (local i64 i64)

    ;; `wit-component` should have injected a call to a function that allocates
    ;; the stack and sets $allocation_state to 2
    (if (i32.ne (global.get $allocation_state) (i32.const 2)) (then (unreachable)))

    ;; Allocate 8 bytes of stack space for the two u32 return values.
    global.get $__stack_pointer
    local.tee 0
    i64.const 8
    i64.sub
    local.tee 1
    global.set $__stack_pointer

    local.get 1
    call $get_two

    (i32.add
      (i32.load (local.get 1))
      (i32.load offset=4 (local.get 1)))

    ;; Another mutable global shouldn't be confused for the stack pointer.
    (global.set $some_other_mutable_global (global.get $some_other_mutable_global))

    local.get 0
    global.set $__stack_pointer
  )

  (func (export "log") (param i64 i64)
    local.get 0
    local.get 1
    call $log
    i32.const 0
    global.get $__stack_pointer
    call $read
  )

  (func (export "entrypoint") (param i64 i64)
    unreachable
  )

  (func (export "cabi_import_realloc") (param i64 i64 i64 i64) (result i64)
    unreachable
  )

  (func (export "cabi_export_realloc") (param i64 i64 i64 i64) (result i64)
    unreachable
  )
)
