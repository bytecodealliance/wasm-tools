(component
  (type $ty-test:test/test (;0;)
    (instance
      (type (;0;) (func (param "v" s32) (result s32)))
      (export (;0;) "bar" (func (type 0)))
    )
  )
  (import "test:test/test" (instance $test:test/test (;0;) (type $ty-test:test/test)))
  (core module $main (;0;)
    (type (;0;) (func))
    (type (;1;) (func))
    (table (;0;) 3 funcref)
    (memory (;0;) 17)
    (global (;0;) i32 i32.const 1048592)
    (global (;1;) i32 i32.const 1)
    (global (;2;) (mut i32) i32.const 0)
    (global (;3;) i32 i32.const 1048624)
    (global (;4;) i32 i32.const 1)
    (global (;5;) i32 i32.const 1048624)
    (global (;6;) i32 i32.const 1)
    (global (;7;) (mut i32) i32.const 0)
    (global (;8;) (mut i32) i32.const 1048640)
    (global (;9;) (mut i32) i32.const 1114112)
    (export "bar:memory_base" (global 0))
    (export "bar:table_base" (global 1))
    (export "bar:well" (global 2))
    (export "c:memory_base" (global 3))
    (export "c:table_base" (global 4))
    (export "foo:memory_base" (global 5))
    (export "foo:table_base" (global 6))
    (export "foo:um" (global 7))
    (export "__heap_base" (global 8))
    (export "__heap_end" (global 9))
    (export "__wasm_init_task" (func 0))
    (export "__wasm_init_async_task" (func 1))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (func (;0;) (type 0)
      i32.const 1
      call_indirect (type 0)
    )
    (func (;1;) (type 1)
      i32.const 2
      call_indirect (type 1)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module $c (;1;)
    (@dylink.0
      (mem-info (memory 0 4))
    )
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i32)))
    (import "GOT.mem" "__heap_base" (global $__heap_base (;0;) (mut i32)))
    (import "GOT.mem" "__heap_end" (global $__heap_end (;1;) (mut i32)))
    (global $heap (;2;) (mut i32) i32.const 0)
    (export "malloc" (func $malloc))
    (export "abort" (func $abort))
    (export "__wasm_init_task" (func $init_task))
    (export "__wasm_init_async_task" (func $init_task))
    (export "__wasm_get_stack_pointer" (func $get_stack_pointer))
    (export "__wasm_set_stack_pointer" (func $set_stack_pointer))
    (start $start)
    (func $start (;0;) (type 0)
      global.get $__heap_base
      global.set $heap
    )
    (func $malloc (;1;) (type 1) (param i32) (result i32)
      global.get $heap
      global.get $heap
      local.get 0
      i32.add
      global.set $heap
    )
    (func $abort (;2;) (type 0)
      unreachable
    )
    (func $init_task (;3;) (type 0)
      unreachable
    )
    (func $get_stack_pointer (;4;) (type 2) (result i32)
      unreachable
    )
    (func $set_stack_pointer (;5;) (type 3) (param i32)
      unreachable
    )
  )
  (core module $foo (;2;)
    (@dylink.0
      (mem-info (memory 4 4))
      (needed "c")
    )
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "env" "__memory_base" (global $__memory_base (;0;) i32))
    (import "env" "__table_base" (global $__table_base (;1;) i32))
    (import "env" "__wasm_get_stack_pointer" (func $__wasm_get_stack_pointer (;0;) (type 2)))
    (import "env" "__wasm_set_stack_pointer" (func $__wasm_set_stack_pointer (;1;) (type 3)))
    (import "env" "malloc" (func $malloc (;2;) (type 1)))
    (import "env" "abort" (func $abort (;3;) (type 0)))
    (import "GOT.mem" "um" (global $um (;2;) (mut i32)))
    (import "test:test/test" "bar" (func $bar (;4;) (type 1)))
    (global (;3;) i32 i32.const 0)
    (export "__wasm_call_ctors" (func $__wasm_call_ctors))
    (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
    (export "foo" (func $foo))
    (export "well" (global 3))
    (func $__wasm_call_ctors (;5;) (type 0))
    (func $__wasm_apply_data_relocs (;6;) (type 0))
    (func $foo (;7;) (type 1) (param i32) (result i32)
      call $__wasm_get_stack_pointer
      i32.const 16
      i32.sub
      call $__wasm_set_stack_pointer
      i32.const 4
      call $malloc
      i32.const 0
      i32.eq
      if ;; label = @1
        call $abort
        unreachable
      end
      local.get 0
      global.get $um
      i32.load offset=16
      i32.add
      i32.const 42
      i32.add
      call $bar
      call $__wasm_get_stack_pointer
      i32.const 16
      i32.add
      call $__wasm_set_stack_pointer
    )
    (data $.data (;0;) (global.get $__memory_base) "\04\00\00\00")
  )
  (core module $bar (;3;)
    (@dylink.0
      (mem-info (memory 20 4))
      (needed "foo")
    )
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "env" "__memory_base" (global $__memory_base (;0;) i32))
    (import "env" "__table_base" (global $__table_base (;1;) i32))
    (import "env" "foo" (func $foo (;0;) (type 0)))
    (import "GOT.mem" "well" (global $well (;2;) (mut i32)))
    (global (;3;) i32 i32.const 0)
    (export "__wasm_call_ctors" (func $__wasm_call_ctors))
    (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
    (export "test:test/test#bar" (func $bar))
    (export "um" (global 3))
    (func $__wasm_call_ctors (;1;) (type 1))
    (func $__wasm_apply_data_relocs (;2;) (type 1))
    (func $bar (;3;) (type 0) (param i32) (result i32)
      local.get 0
      call $foo
      global.get $well
      i32.load
      i32.add
    )
    (data $.data (;0;) (global.get $__memory_base) "\01\00\00\00\02\00\00\00\03\00\00\00\04\00\00\00\05\00\00\00")
  )
  (core instance $main (;0;) (instantiate $main))
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $main "__heap_base" (core global $__heap_base (;0;)))
  (alias core export $main "__heap_end" (core global $__heap_end (;1;)))
  (core instance $GOT.mem (;1;)
    (export "__heap_base" (global $__heap_base))
    (export "__heap_end" (global $__heap_end))
  )
  (core instance $c (;2;) (instantiate $c
      (with "GOT.mem" (instance $GOT.mem))
    )
  )
  (alias core export $main "__indirect_function_table" (core table $__indirect_function_table (;0;)))
  (alias core export $main "foo:memory_base" (core global $foo:memory_base (;2;)))
  (alias core export $main "foo:table_base" (core global $foo:table_base (;3;)))
  (core func $"context.get 0" (;0;) (canon context.get i32 0))
  (core func $"context.set 0" (;1;) (canon context.set i32 0))
  (alias core export $c "malloc" (core func $malloc (;2;)))
  (alias core export $c "abort" (core func $abort (;3;)))
  (core instance $env (;3;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__memory_base" (global $foo:memory_base))
    (export "__table_base" (global $foo:table_base))
    (export "__wasm_get_stack_pointer" (func $"context.get 0"))
    (export "__wasm_set_stack_pointer" (func $"context.set 0"))
    (export "malloc" (func $malloc))
    (export "abort" (func $abort))
  )
  (alias core export $main "foo:um" (core global $foo:um (;4;)))
  (core instance $"#core-instance4 GOT.mem" (@name "GOT.mem") (;4;)
    (export "um" (global $foo:um))
  )
  (alias export $test:test/test "bar" (func $bar (;0;)))
  (core func $bar (;4;) (canon lower (func $bar)))
  (core instance $test:test/test (;5;)
    (export "bar" (func $bar))
  )
  (core instance $foo (;6;) (instantiate $foo
      (with "env" (instance $env))
      (with "GOT.mem" (instance $"#core-instance4 GOT.mem"))
      (with "test:test/test" (instance $test:test/test))
    )
  )
  (alias core export $main "bar:memory_base" (core global $bar:memory_base (;5;)))
  (alias core export $main "bar:table_base" (core global $bar:table_base (;6;)))
  (alias core export $foo "foo" (core func $foo (;5;)))
  (core instance $"#core-instance7 env" (@name "env") (;7;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__memory_base" (global $bar:memory_base))
    (export "__table_base" (global $bar:table_base))
    (export "foo" (func $foo))
  )
  (alias core export $main "bar:well" (core global $bar:well (;7;)))
  (core instance $"#core-instance8 GOT.mem" (@name "GOT.mem") (;8;)
    (export "well" (global $bar:well))
  )
  (core instance $bar (;9;) (instantiate $bar
      (with "env" (instance $"#core-instance7 env"))
      (with "GOT.mem" (instance $"#core-instance8 GOT.mem"))
    )
  )
  (core module $wit-component-fixup (;4;)
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (import "main" "memory" (memory (;0;) 0))
    (import "main" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "main" "foo:memory_base" (global $foo:memory_base (;0;) i32))
    (import "foo" "well" (global $well (;1;) i32))
    (import "main" "bar:well" (global $bar:well (;2;) (mut i32)))
    (import "main" "bar:memory_base" (global $bar:memory_base (;3;) i32))
    (import "bar" "um" (global $um (;4;) i32))
    (import "main" "foo:um" (global $foo:um (;5;) (mut i32)))
    (import "bar" "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs (;0;) (type 0)))
    (import "foo" "__wasm_apply_data_relocs" (func $"#func1 __wasm_apply_data_relocs" (@name "__wasm_apply_data_relocs") (;1;) (type 0)))
    (import "bar" "__wasm_call_ctors" (func $__wasm_call_ctors (;2;) (type 0)))
    (import "foo" "__wasm_call_ctors" (func $"#func3 __wasm_call_ctors" (@name "__wasm_call_ctors") (;3;) (type 0)))
    (import "c" "__wasm_init_task" (func $__wasm_init_task (;4;) (type 0)))
    (import "c" "__wasm_init_async_task" (func $__wasm_init_async_task (;5;) (type 0)))
    (import "main" "__wasm_init_task" (func $"#func6 __wasm_init_task" (@name "__wasm_init_task") (;6;) (type 0)))
    (import "bar" "test:test/test#bar" (func $test:test/test#bar (;7;) (type 1)))
    (export "hook0" (func $hook-test:test/test#bar))
    (start $start)
    (elem (;0;) (i32.const 1) func)
    (elem (;1;) (i32.const 1) func $__wasm_init_task $__wasm_init_async_task)
    (func $hook-test:test/test#bar (;8;) (type 1) (param i32) (result i32)
      call $"#func6 __wasm_init_task"
      local.get 0
      call $test:test/test#bar
    )
    (func $start (;9;) (type 0)
      global.get $foo:memory_base
      global.get $well
      i32.add
      global.set $bar:well
      global.get $bar:memory_base
      global.get $um
      i32.add
      global.set $foo:um
      call $__wasm_apply_data_relocs
      call $"#func1 __wasm_apply_data_relocs"
      call $"#func6 __wasm_init_task"
      call $__wasm_call_ctors
      call $"#func3 __wasm_call_ctors"
    )
    (data (;0;) (i32.const 1048576) "\00\00\00\00\00\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $fixup (;10;) (instantiate $wit-component-fixup
      (with "main" (instance $main))
      (with "foo" (instance $foo))
      (with "bar" (instance $bar))
      (with "c" (instance $c))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;6;)))
  (type (;1;) (func (param "v" s32) (result s32)))
  (func $"#func1 bar" (@name "bar") (;1;) (type 1) (canon lift (core func $hook0)))
  (component $test:test/test-shim-component (;0;)
    (type (;0;) (func (param "v" s32) (result s32)))
    (import "import-func-bar" (func (;0;) (type 0)))
    (type (;1;) (func (param "v" s32) (result s32)))
    (export (;1;) "bar" (func 0) (func (type 1)))
  )
  (instance $test:test/test-shim-instance (;1;) (instantiate $test:test/test-shim-component
      (with "import-func-bar" (func $"#func1 bar"))
    )
  )
  (export $"#instance2 test:test/test" (@name "test:test/test") (;2;) "test:test/test" (instance $test:test/test-shim-instance))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
