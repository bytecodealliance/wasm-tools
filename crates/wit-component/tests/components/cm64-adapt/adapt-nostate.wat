(module
  (import "new2" "get-two" (func $get_two (param i64)))
  (import "__main_module__" "cabi_realloc" (func $cabi_realloc (param i64 i64 i64 i64) (result i64)))
  (import "env" "memory" (memory i64 0))

  ;; Without an `$allocation_state` global the stack can't be lazily
  ;; allocated with `cabi_realloc`, so it's instead allocated eagerly with
  ;; `memory.grow`.
  (global $__stack_pointer (mut i64) i64.const 0)

  (func (export "get_sum") (result i32)
    (local i64 i64)

    ;; Allocate a page using `$cabi_realloc` from the main module.
    (local.set 0
      (call $cabi_realloc
        (i64.const 0)
        (i64.const 0)
        (i64.const 8)
        (i64.const 65536)))

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

    local.get 0
    global.set $__stack_pointer
  )
)
