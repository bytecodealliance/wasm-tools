(component
  (type (;0;)
    (instance
      (type (;0;) (func (result "a" u32) (result "b" u32)))
      (export (;0;) "get-two" (func (type 0)))
    )
  )
  (import "new" (instance (;0;) (type 0)))
  (core module (;0;)
    (type (;0;) (func (result i32)))
    (import "old" "get_sum" (func (;0;) (type 0)))
    (memory (;0;) 1)
    (export "memory" (memory 0))
  )
  (core module (;1;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func (result i32)))
    (type (;2;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;3;) (func))
    (import "env" "memory" (memory (;0;) 0))
    (import "new" "get-two" (func $get_two (;0;) (type 0)))
    (func (;1;) (type 1) (result i32)
      (local i32 i32)
      global.get $__stack_pointer
      local.tee 0
      i32.const 8
      i32.sub
      local.tee 1
      global.set $__stack_pointer
      local.get 1
      call $get_two
      local.get 1
      i32.load
      local.get 1
      i32.load offset=4
      i32.add
      global.get $some_other_mutable_global
      global.set $some_other_mutable_global
      local.get 0
      global.set $__stack_pointer
    )
    (func $realloc_via_memory_grow (;2;) (type 2) (param i32 i32 i32 i32) (result i32)
      (local i32)
      i32.const 0
      local.get 0
      i32.ne
      if ;; label = @1
        unreachable
      end
      i32.const 0
      local.get 1
      i32.ne
      if ;; label = @1
        unreachable
      end
      i32.const 65536
      local.get 3
      i32.ne
      if ;; label = @1
        unreachable
      end
      i32.const 1
      memory.grow
      local.tee 4
      i32.const -1
      i32.eq
      if ;; label = @1
        unreachable
      end
      local.get 4
      i32.const 16
      i32.shl
    )
    (func $allocate_stack (;3;) (type 3)
      i32.const 0
      i32.const 0
      i32.const 8
      i32.const 65536
      call $realloc_via_memory_grow
      i32.const 65536
      i32.add
      global.set $__stack_pointer
    )
    (global $__stack_pointer (;0;) (mut i32) i32.const 0)
    (global $some_other_mutable_global (;1;) (mut i32) i32.const 0)
    (export "get_sum" (func 1))
    (start $allocate_stack)
  )
  (core module (;2;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func (result i32)))
    (func $indirect-new-get-two (;0;) (type 0) (param i32)
      local.get 0
      i32.const 0
      call_indirect (type 0)
    )
    (func $adapt-old-get_sum (;1;) (type 1) (result i32)
      i32.const 1
      call_indirect (type 1)
    )
    (table (;0;) 2 2 funcref)
    (export "0" (func $indirect-new-get-two))
    (export "1" (func $adapt-old-get_sum))
    (export "$imports" (table 0))
  )
  (core module (;3;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func (result i32)))
    (import "" "0" (func (;0;) (type 0)))
    (import "" "1" (func (;1;) (type 1)))
    (import "" "$imports" (table (;0;) 2 2 funcref))
    (elem (;0;) (i32.const 0) func 0 1)
  )
  (core instance (;0;) (instantiate 2))
  (alias core export 0 "1" (core func (;0;)))
  (core instance (;1;)
    (export "get_sum" (func 0))
  )
  (core instance (;2;) (instantiate 0
      (with "old" (instance 1))
    )
  )
  (alias core export 2 "memory" (core memory (;0;)))
  (core instance (;3;)
    (export "memory" (memory 0))
  )
  (alias core export 0 "0" (core func (;1;)))
  (core instance (;4;)
    (export "get-two" (func 1))
  )
  (core instance (;5;) (instantiate 1
      (with "env" (instance 3))
      (with "new" (instance 4))
    )
  )
  (alias core export 0 "$imports" (core table (;0;)))
  (alias export 0 "get-two" (func (;0;)))
  (core func (;2;) (canon lower (func 0) (memory 0)))
  (alias core export 5 "get_sum" (core func (;3;)))
  (core instance (;6;)
    (export "$imports" (table 0))
    (export "0" (func 2))
    (export "1" (func 3))
  )
  (core instance (;7;) (instantiate 3
      (with "" (instance 6))
    )
  )
)