(component
  (type $ty-new2 (;0;)
    (instance
      (type (;0;) (tuple u32 u32))
      (type (;1;) (func (result 0)))
      (export (;0;) "get-two" (func (type 1)))
    )
  )
  (import "new2" (instance $new2 (;0;) (type $ty-new2)))
  (type $ty-new (;1;)
    (instance
      (type (;0;) (tuple u32 u32))
      (type (;1;) (func (result 0)))
      (export (;0;) "get-two" (func (type 1)))
      (type (;2;) (func (param "s" string)))
      (export (;1;) "log" (func (type 2)))
      (type (;3;) (list u8))
      (type (;4;) (func (param "amt" u32) (result 3)))
      (export (;2;) "read" (func (type 4)))
    )
  )
  (import "new" (instance $new (;1;) (type $ty-new)))
  (core module $main (;0;)
    (type (;0;) (func (result i32)))
    (type (;1;) (func (param i64 i64)))
    (type (;2;) (func (param i64 i64 i64 i64) (result i64)))
    (import "old" "get_sum" (func (;0;) (type 0) (result i32)))
    (import "old" "log" (func (;1;) (type 1) (param i64 i64)))
    (import "nostate" "get_sum" (func (;2;) (type 0) (result i32)))
    (memory (;0;) i64 1)
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 3))
    (export "cabi_realloc_adapter" (func 4))
    (func (;3;) (type 2) (param i64 i64 i64 i64) (result i64)
      unreachable
    )
    (func (;4;) (type 2) (param i64 i64 i64 i64) (result i64)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component:adapter:nostate (;1;)
    (type (;0;) (func (param i64)))
    (type (;1;) (func (param i64 i64 i64 i64) (result i64)))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i64 i64 i64 i64) (result i64)))
    (type (;4;) (func))
    (import "env" "memory" (memory (;0;) i64 0))
    (import "new2" "get-two" (func $get_two (;0;) (type 0) (param i64)))
    (import "__main_module__" "cabi_realloc_adapter" (func $cabi_realloc (;1;) (type 1) (param i64 i64 i64 i64) (result i64)))
    (global $__stack_pointer (;0;) (mut i64) i64.const 0)
    (export "get_sum" (func 2))
    (start $allocate_stack)
    (func (;2;) (type 2) (result i32)
      (local i64 i64)
      i64.const 0
      i64.const 0
      i64.const 8
      i64.const 65536
      call $cabi_realloc
      local.set 0
      global.get $__stack_pointer
      local.tee 0
      i64.const 8
      i64.sub
      local.tee 1
      global.set $__stack_pointer
      local.get 1
      call $get_two
      local.get 1
      i32.load
      local.get 1
      i32.load offset=4
      i32.add
      local.get 0
      global.set $__stack_pointer
    )
    (func (;3;) (type 3) (param i64 i64 i64 i64) (result i64)
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
    (func $allocate_stack (;4;) (type 4)
      i64.const 0
      i64.const 0
      i64.const 8
      i64.const 65536
      call 3
      i64.const 65536
      i64.add
      global.set $__stack_pointer
    )
  )
  (core module $wit-component:adapter:old (;2;)
    (type (;0;) (func (param i64)))
    (type (;1;) (func (param i64 i64)))
    (type (;2;) (func (param i32 i64)))
    (type (;3;) (func (result i32)))
    (type (;4;) (func (param i64 i64 i64 i64) (result i64)))
    (type (;5;) (func (param i64 i64 i64 i64) (result i64)))
    (type (;6;) (func))
    (import "env" "memory" (memory (;0;) i64 0))
    (import "new" "get-two" (func $get_two (;0;) (type 0) (param i64)))
    (import "new" "log" (func $log (;1;) (type 1) (param i64 i64)))
    (import "new" "read" (func $read (;2;) (type 2) (param i32 i64)))
    (import "__main_module__" "cabi_realloc_adapter" (func $cabi_realloc_adapter (;3;) (type 5) (param i64 i64 i64 i64) (result i64)))
    (global $__stack_pointer (;0;) (mut i64) i64.const 0)
    (global $some_other_mutable_global (;1;) (mut i32) i32.const 0)
    (global $allocation_state (;2;) (mut i32) i32.const 0)
    (export "get_sum" (func 4))
    (export "log" (func 5))
    (export "entrypoint" (func 6))
    (export "cabi_import_realloc" (func 7))
    (export "cabi_export_realloc" (func 8))
    (func (;4;) (type 3) (result i32)
      (local i64 i64)
      call $allocate_stack
      global.get $allocation_state
      i32.const 2
      i32.ne
      if ;; label = @1
        unreachable
      end
      global.get $__stack_pointer
      local.tee 0
      i64.const 8
      i64.sub
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
    (func (;5;) (type 1) (param i64 i64)
      call $allocate_stack
      local.get 0
      local.get 1
      call $log
      i32.const 0
      global.get $__stack_pointer
      call $read
    )
    (func (;6;) (type 1) (param i64 i64)
      call $allocate_stack
      unreachable
    )
    (func (;7;) (type 4) (param i64 i64 i64 i64) (result i64)
      call $allocate_stack
      unreachable
    )
    (func (;8;) (type 4) (param i64 i64 i64 i64) (result i64)
      call $allocate_stack
      unreachable
    )
    (func $allocate_stack (;9;) (type 6)
      global.get $allocation_state
      i32.const 0
      i32.eq
      if ;; label = @1
        i32.const 1
        global.set $allocation_state
        i64.const 0
        i64.const 0
        i64.const 8
        i64.const 65536
        call $cabi_realloc_adapter
        i64.const 65536
        i64.add
        global.set $__stack_pointer
        i32.const 2
        global.set $allocation_state
      end
    )
  )
  (core module $wit-component-shim-module (;3;)
    (type (;0;) (func (result i32)))
    (type (;1;) (func (param i64 i64)))
    (type (;2;) (func (param i64)))
    (type (;3;) (func (param i32 i64)))
    (table (;0;) 7 7 funcref)
    (export "0" (func $adapt-old-get_sum))
    (export "1" (func $adapt-old-log))
    (export "2" (func $adapt-nostate-get_sum))
    (export "3" (func $indirect-new2-get-two))
    (export "4" (func $indirect-new-get-two))
    (export "5" (func $indirect-new-log))
    (export "6" (func $indirect-new-read))
    (export "$imports" (table 0))
    (func $adapt-old-get_sum (;0;) (type 0) (result i32)
      i32.const 0
      call_indirect (type 0)
    )
    (func $adapt-old-log (;1;) (type 1) (param i64 i64)
      local.get 0
      local.get 1
      i32.const 1
      call_indirect (type 1)
    )
    (func $adapt-nostate-get_sum (;2;) (type 0) (result i32)
      i32.const 2
      call_indirect (type 0)
    )
    (func $indirect-new2-get-two (;3;) (type 2) (param i64)
      local.get 0
      i32.const 3
      call_indirect (type 2)
    )
    (func $indirect-new-get-two (;4;) (type 2) (param i64)
      local.get 0
      i32.const 4
      call_indirect (type 2)
    )
    (func $indirect-new-log (;5;) (type 1) (param i64 i64)
      local.get 0
      local.get 1
      i32.const 5
      call_indirect (type 1)
    )
    (func $indirect-new-read (;6;) (type 3) (param i32 i64)
      local.get 0
      local.get 1
      i32.const 6
      call_indirect (type 3)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "0" (core func $adapt-old-get_sum (;0;)))
  (alias core export $wit-component-shim-instance "1" (core func $adapt-old-log (;1;)))
  (core instance $old (;1;)
    (export "get_sum" (func $adapt-old-get_sum))
    (export "log" (func $adapt-old-log))
  )
  (alias core export $wit-component-shim-instance "2" (core func $adapt-nostate-get_sum (;2;)))
  (core instance $nostate (;2;)
    (export "get_sum" (func $adapt-nostate-get_sum))
  )
  (core instance $main (;3;) (instantiate $main
      (with "old" (instance $old))
      (with "nostate" (instance $nostate))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core instance $env (;4;)
    (export "memory" (memory $memory))
  )
  (alias core export $wit-component-shim-instance "3" (core func $indirect-new2-get-two (;3;)))
  (core instance $new2 (;5;)
    (export "get-two" (func $indirect-new2-get-two))
  )
  (alias core export $main "cabi_realloc_adapter" (core func $cabi_realloc_adapter (;4;)))
  (core instance $__main_module__ (;6;)
    (export "cabi_realloc_adapter" (func $cabi_realloc_adapter))
  )
  (core instance $"#core-instance7 nostate" (@name "nostate") (;7;) (instantiate $wit-component:adapter:nostate
      (with "env" (instance $env))
      (with "new2" (instance $new2))
      (with "__main_module__" (instance $__main_module__))
    )
  )
  (core instance $"#core-instance8 env" (@name "env") (;8;)
    (export "memory" (memory $memory))
  )
  (alias core export $wit-component-shim-instance "4" (core func $indirect-new-get-two (;5;)))
  (alias core export $wit-component-shim-instance "5" (core func $indirect-new-log (;6;)))
  (alias core export $wit-component-shim-instance "6" (core func $indirect-new-read (;7;)))
  (core instance $new (;9;)
    (export "get-two" (func $indirect-new-get-two))
    (export "log" (func $indirect-new-log))
    (export "read" (func $indirect-new-read))
  )
  (core instance $"#core-instance10 __main_module__" (@name "__main_module__") (;10;)
    (export "cabi_realloc_adapter" (func $cabi_realloc_adapter))
  )
  (core instance $"#core-instance11 old" (@name "old") (;11;) (instantiate $wit-component:adapter:old
      (with "env" (instance $"#core-instance8 env"))
      (with "new" (instance $new))
      (with "__main_module__" (instance $"#core-instance10 __main_module__"))
    )
  )
  (core module $wit-component-fixup (;4;)
    (type (;0;) (func (result i32)))
    (type (;1;) (func (param i64 i64)))
    (type (;2;) (func (param i64)))
    (type (;3;) (func (param i32 i64)))
    (import "actual" "0" (func $0 (;0;) (type 0) (result i32)))
    (import "actual" "1" (func $1 (;1;) (type 1) (param i64 i64)))
    (import "actual" "2" (func $2 (;2;) (type 0) (result i32)))
    (import "actual" "3" (func $3 (;3;) (type 2) (param i64)))
    (import "actual" "4" (func $4 (;4;) (type 2) (param i64)))
    (import "actual" "5" (func $5 (;5;) (type 1) (param i64 i64)))
    (import "actual" "6" (func $6 (;6;) (type 3) (param i32 i64)))
    (import "shim" "$imports" (table (;0;) 7 7 funcref))
    (elem (;0;) (i32.const 0) func $0 $1 $2 $3 $4 $5 $6)
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $"#core-instance11 old" "get_sum" (core func $get_sum (;8;)))
  (alias core export $"#core-instance11 old" "log" (core func $log (;9;)))
  (alias core export $"#core-instance7 nostate" "get_sum" (core func $"#core-func10 get_sum" (@name "get_sum") (;10;)))
  (alias export $new2 "get-two" (func $get-two (;0;)))
  (core func $"#core-func11 indirect-new2-get-two" (@name "indirect-new2-get-two") (;11;) (canon lower (func $get-two) (memory $memory)))
  (alias export $new "get-two" (func $"#func1 get-two" (@name "get-two") (;1;)))
  (core func $"#core-func12 indirect-new-get-two" (@name "indirect-new-get-two") (;12;) (canon lower (func $"#func1 get-two") (memory $memory)))
  (alias export $new "log" (func $log (;2;)))
  (core func $"#core-func13 indirect-new-log" (@name "indirect-new-log") (;13;) (canon lower (func $log) (memory $memory) string-encoding=utf8))
  (alias export $new "read" (func $read (;3;)))
  (alias core export $"#core-instance11 old" "cabi_import_realloc" (core func $cabi_import_realloc (;14;)))
  (core func $"#core-func15 indirect-new-read" (@name "indirect-new-read") (;15;) (canon lower (func $read) (memory $memory) (realloc $cabi_import_realloc)))
  (core instance $actual (;12;)
    (export "0" (func $get_sum))
    (export "1" (func $log))
    (export "2" (func $"#core-func10 get_sum"))
    (export "3" (func $"#core-func11 indirect-new2-get-two"))
    (export "4" (func $"#core-func12 indirect-new-get-two"))
    (export "5" (func $"#core-func13 indirect-new-log"))
    (export "6" (func $"#core-func15 indirect-new-read"))
  )
  (core instance $fixup (;13;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (type (;2;) (list string))
  (type (;3;) (func (param "args" 2)))
  (alias core export $"#core-instance11 old" "entrypoint" (core func $entrypoint (;16;)))
  (alias core export $"#core-instance11 old" "cabi_export_realloc" (core func $cabi_export_realloc (;17;)))
  (func $entrypoint (;4;) (type 3) (canon lift (core func $entrypoint) (memory $memory) (realloc $cabi_export_realloc) string-encoding=utf8))
  (export $"#func5 entrypoint" (@name "entrypoint") (;5;) "entrypoint" (func $entrypoint))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
