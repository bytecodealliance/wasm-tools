;;! realloc-via-memory-grow = true

(module
  (import "new" "get-two" (func $get_two (param i32)))
  (import "__main_module__" "cabi_realloc" (func $cabi_realloc (param i32 i32 i32 i32) (result i32)))
  (import "env" "memory" (memory 0))

  (global $__stack_pointer (mut i32) i32.const 0)
  (global $allocation_state (mut i32) i32.const 0)

  (func (export "get_sum") (result i32)
    (local i32 i32)

    (if (i32.ne (global.get $allocation_state) (i32.const 2)) (then (unreachable)))

    (local.set 0
      (call $cabi_realloc
        (i32.const 0)
        (i32.const 0)
        (i32.const 8)
        (i32.const 65536)))

    (i32.store (local.get 0) (i32.const 42))

    global.get $__stack_pointer
    local.tee 0
    i32.const 8
    i32.sub
    local.tee 1
    global.set $__stack_pointer

    local.get 1
    call $get_two

    (i32.add
      (i32.load (local.get 1))
      (i32.load offset=4 (local.get 1)))

    local.get 0
    global.set $__stack_pointer
  )
)
