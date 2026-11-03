(component
  (type $ty-new (;0;)
    (instance
      (type (;0;) (tuple u32 u32))
      (type (;1;) (func (result 0)))
      (export (;0;) "get-two" (func (type 1)))
      (type (;2;) (list u8))
      (type (;3;) (func (result 2)))
      (export (;1;) "read" (func (type 3)))
    )
  )
  (import "new" (instance $new (;0;) (type $ty-new)))
  (core module $main (;0;)
    (type (;0;) (func (result i32)))
    (import "old" "get_sum" (func (;0;) (type 0) (result i32)))
    (memory (;0;) i64 1)
    (export "memory" (memory 0))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component:adapter:old (;1;)
    (type (;0;) (func (param i64)))
    (type (;1;) (func (param i64 i64 i64 i64) (result i64)))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i64 i64 i64 i64) (result i64)))
    (type (;4;) (func))
    (import "env" "memory" (memory (;0;) i64 0))
    (import "new" "get-two" (func $get_two (;0;) (type 0) (param i64)))
    (import "new" "read" (func $read (;1;) (type 0) (param i64)))
    (global $__stack_pointer (;0;) (mut i64) i64.const 0)
    (export "cabi_import_realloc" (func 3))
    (export "get_sum" (func 4))
    (start $allocate_stack)
    (func $cabi_realloc (;2;) (type 1) (param i64 i64 i64 i64) (result i64)
      (local i64)
      i64.const 0
      local.get 0
      i64.ne
      if ;; label = @1
        unreachable
      end
      i64.const 0
      local.get 1
      i64.ne
      if ;; label = @1
        unreachable
      end
      i64.const 65536
      local.get 3
      i64.ne
      if ;; label = @1
        unreachable
      end
      i64.const 1
      memory.grow
      local.tee 4
      i64.const -1
      i64.eq
      if ;; label = @1
        unreachable
      end
      local.get 4
      i64.const 16
      i64.shl
    )
    (func (;3;) (type 1) (param i64 i64 i64 i64) (result i64)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $cabi_realloc
    )
    (func (;4;) (type 2) (result i32)
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
      local.get 1
      i32.load
      local.get 1
      i32.load offset=4
      i32.add
      local.get 0
      global.set $__stack_pointer
    )
    (func (;5;) (type 3) (param i64 i64 i64 i64) (result i64)
      (local i64)
      i64.const 0
      local.get 0
      i64.ne
      if ;; label = @1
        unreachable
      end
      i64.const 0
      local.get 1
      i64.ne
      if ;; label = @1
        unreachable
      end
      i64.const 65536
      local.get 3
      i64.ne
      if ;; label = @1
        unreachable
      end
      i64.const 1
      memory.grow
      local.tee 4
      i64.const -1
      i64.eq
      if ;; label = @1
        unreachable
      end
      local.get 4
      i64.const 16
      i64.shl
    )
    (func $allocate_stack (;6;) (type 4)
      i64.const 0
      i64.const 0
      i64.const 8
      i64.const 65536
      call 5
      i64.const 65536
      i64.add
      global.set $__stack_pointer
    )
  )
  (core module $wit-component-shim-module (;2;)
    (type (;0;) (func (result i32)))
    (type (;1;) (func (param i64)))
    (table (;0;) 3 3 funcref)
    (export "0" (func $adapt-old-get_sum))
    (export "1" (func $indirect-new-get-two))
    (export "2" (func $indirect-new-read))
    (export "$imports" (table 0))
    (func $adapt-old-get_sum (;0;) (type 0) (result i32)
      i32.const 0
      call_indirect (type 0)
    )
    (func $indirect-new-get-two (;1;) (type 1) (param i64)
      local.get 0
      i32.const 1
      call_indirect (type 1)
    )
    (func $indirect-new-read (;2;) (type 1) (param i64)
      local.get 0
      i32.const 2
      call_indirect (type 1)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "0" (core func $adapt-old-get_sum (;0;)))
  (core instance $old (;1;)
    (export "get_sum" (func $adapt-old-get_sum))
  )
  (core instance $main (;2;) (instantiate $main
      (with "old" (instance $old))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core instance $env (;3;)
    (export "memory" (memory $memory))
  )
  (alias core export $wit-component-shim-instance "1" (core func $indirect-new-get-two (;1;)))
  (alias core export $wit-component-shim-instance "2" (core func $indirect-new-read (;2;)))
  (core instance $new (;4;)
    (export "get-two" (func $indirect-new-get-two))
    (export "read" (func $indirect-new-read))
  )
  (core instance $"#core-instance5 old" (@name "old") (;5;) (instantiate $wit-component:adapter:old
      (with "env" (instance $env))
      (with "new" (instance $new))
    )
  )
  (core module $wit-component-fixup (;3;)
    (type (;0;) (func (result i32)))
    (type (;1;) (func (param i64)))
    (import "actual" "0" (func $0 (;0;) (type 0) (result i32)))
    (import "actual" "1" (func $1 (;1;) (type 1) (param i64)))
    (import "actual" "2" (func $2 (;2;) (type 1) (param i64)))
    (import "shim" "$imports" (table (;0;) 3 3 funcref))
    (elem (;0;) (i32.const 0) func $0 $1 $2)
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $"#core-instance5 old" "get_sum" (core func $get_sum (;3;)))
  (alias export $new "get-two" (func $get-two (;0;)))
  (core func $"#core-func4 indirect-new-get-two" (@name "indirect-new-get-two") (;4;) (canon lower (func $get-two) (memory $memory)))
  (alias export $new "read" (func $read (;1;)))
  (alias core export $"#core-instance5 old" "cabi_import_realloc" (core func $cabi_import_realloc (;5;)))
  (core func $"#core-func6 indirect-new-read" (@name "indirect-new-read") (;6;) (canon lower (func $read) (memory $memory) (realloc $cabi_import_realloc)))
  (core instance $actual (;6;)
    (export "0" (func $get_sum))
    (export "1" (func $"#core-func4 indirect-new-get-two"))
    (export "2" (func $"#core-func6 indirect-new-read"))
  )
  (core instance $fixup (;7;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
