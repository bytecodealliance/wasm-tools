(component
  (type $ty-test:test/test (;0;)
    (instance
      (type (;0;) (func (param "v" string) (result string)))
      (export (;0;) "bar" (func (type 0)))
    )
  )
  (import "test:test/test" (instance $test:test/test (;0;) (type $ty-test:test/test)))
  (core module $main (;0;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32)))
    (import "c" "cabi_realloc" (func (;0;) (type 0)))
    (table (;0;) 2 funcref)
    (memory (;0;) 17)
    (global (;0;) i32 i32.const 1048592)
    (global (;1;) i32 i32.const 1)
    (global (;2;) i32 i32.const 1048592)
    (global (;3;) i32 i32.const 1)
    (global (;4;) (mut i32) i32.const 1048608)
    (export "cabi_realloc" (func 0))
    (export "c:memory_base" (global 0))
    (export "c:table_base" (global 1))
    (export "foo:memory_base" (global 2))
    (export "foo:table_base" (global 3))
    (export "__heap_base" (global 4))
    (export "__wasm_task_hook" (func 1))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (func (;1;) (type 1) (param i32)
      local.get 0
      i32.const 1
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
    (type $void (;0;) (func))
    (type $hook (;1;) (func (param i32)))
    (type $malloc (;2;) (func (param i32) (result i32)))
    (type $realloc (;3;) (func (param i32 i32 i32 i32) (result i32)))
    (import "GOT.mem" "__heap_base" (global $__heap_base (;0;) (mut i32)))
    (global $heap (;1;) (mut i32) i32.const 0)
    (export "malloc" (func $malloc))
    (export "cabi_realloc" (func $cabi_realloc))
    (export "abort" (func $abort))
    (export "__wasm_task_hook" (func $__wasm_task_hook))
    (start $start)
    (func $start (;0;) (type $void)
      global.get $__heap_base
      global.set $heap
    )
    (func $malloc (;1;) (type $malloc) (param i32) (result i32)
      global.get $heap
      global.get $heap
      local.get 0
      i32.add
      global.set $heap
    )
    (func $cabi_realloc (;2;) (type $realloc) (param i32 i32 i32 i32) (result i32)
      local.get 3
      call $malloc
    )
    (func $abort (;3;) (type $void)
      unreachable
    )
    (func $__wasm_task_hook (;4;) (type $hook) (param i32)
      unreachable
    )
  )
  (core module $foo (;2;)
    (@dylink.0
      (mem-info (memory 16 4))
      (needed "c")
    )
    (type (;0;) (func (param i32 i32 i32)))
    (type (;1;) (func (result i32)))
    (type (;2;) (func (param i32 i32) (result i32)))
    (type (;3;) (func (param i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "test:test/test" "bar" (func $bar (;0;) (type 0)))
    (export "run" (func 1))
    (export "greet" (func 2))
    (export "cabi_post_greet" (func 3))
    (func (;1;) (type 1) (result i32)
      unreachable
    )
    (func (;2;) (type 2) (param i32 i32) (result i32)
      unreachable
    )
    (func (;3;) (type 3) (param i32)
      unreachable
    )
  )
  (core module $wit-component-shim-module (;3;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32 i32 i32)))
    (table (;0;) 3 3 funcref)
    (export "0" (func $adapt-c-cabi_realloc))
    (export "1" (func $indirect-test:test/test-bar))
    (export "2" (func $realloc-main-cabi_realloc))
    (export "$imports" (table 0))
    (func $adapt-c-cabi_realloc (;0;) (type 0) (param i32 i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 0
      call_indirect (type 0)
    )
    (func $indirect-test:test/test-bar (;1;) (type 1) (param i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      i32.const 1
      call_indirect (type 1)
    )
    (func $realloc-main-cabi_realloc (;2;) (type 0) (param i32 i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 2
      call_indirect (type 0)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "0" (core func $adapt-c-cabi_realloc (;0;)))
  (core instance $c (;1;)
    (export "cabi_realloc" (func $adapt-c-cabi_realloc))
  )
  (core instance $main (;2;) (instantiate $main
      (with "c" (instance $c))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $main "__heap_base" (core global $__heap_base (;0;)))
  (core instance $GOT.mem (;3;)
    (export "__heap_base" (global $__heap_base))
  )
  (core instance $"#core-instance4 c" (@name "c") (;4;) (instantiate $c
      (with "GOT.mem" (instance $GOT.mem))
    )
  )
  (core instance $env (;5;)
    (export "memory" (memory $memory))
  )
  (alias core export $wit-component-shim-instance "1" (core func $indirect-test:test/test-bar (;1;)))
  (core instance $test:test/test (;6;)
    (export "bar" (func $indirect-test:test/test-bar))
  )
  (core instance $foo (;7;) (instantiate $foo
      (with "env" (instance $env))
      (with "test:test/test" (instance $test:test/test))
    )
  )
  (core module $wit-component-fixup (;4;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32 i32 i32)))
    (type (;2;) (func))
    (type (;3;) (func (param i32)))
    (type (;4;) (func (result i32)))
    (type (;5;) (func (param i32 i32) (result i32)))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "actual" "1" (func $1 (;1;) (type 1)))
    (import "actual" "2" (func $2 (;2;) (type 0)))
    (import "main" "memory" (memory (;0;) 0))
    (import "main" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "c" "__wasm_task_hook" (func $__wasm_task_hook (;3;) (type 3)))
    (import "main" "__wasm_task_hook" (func $"#func4 __wasm_task_hook" (@name "__wasm_task_hook") (;4;) (type 3)))
    (import "foo" "run" (func $run (;5;) (type 4)))
    (import "foo" "greet" (func $greet (;6;) (type 5)))
    (import "foo" "cabi_post_greet" (func $cabi_post_greet (;7;) (type 3)))
    (import "main" "cabi_realloc" (func $cabi_realloc (;8;) (type 0)))
    (import "c" "cabi_realloc" (func $"#func9 cabi_realloc" (@name "cabi_realloc") (;9;) (type 0)))
    (import "shim" "$imports" (table (;1;) 3 3 funcref))
    (export "hook0" (func $hook-run))
    (export "hook1" (func $hook-greet))
    (export "hook2" (func $hook-cabi_post_greet))
    (export "hook3" (func $hook-cabi_realloc))
    (export "hook4" (func $"#func14 hook-cabi_realloc"))
    (elem (;0;) (i32.const 1) func)
    (elem (;1;) (i32.const 1) func $__wasm_task_hook)
    (elem (;2;) (table 1) (i32.const 0) func $0 $1 $hook-cabi_realloc)
    (func $hook-run (;10;) (type 4) (result i32)
      i32.const 0
      call $"#func4 __wasm_task_hook"
      call $run
      i32.const 1
      call $"#func4 __wasm_task_hook"
    )
    (func $hook-greet (;11;) (type 5) (param i32 i32) (result i32)
      i32.const 0
      call $"#func4 __wasm_task_hook"
      local.get 0
      local.get 1
      call $greet
    )
    (func $hook-cabi_post_greet (;12;) (type 3) (param i32)
      local.get 0
      call $cabi_post_greet
      i32.const 1
      call $"#func4 __wasm_task_hook"
    )
    (func $hook-cabi_realloc (;13;) (type 0) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $"#func4 __wasm_task_hook"
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $cabi_realloc
      i32.const 13
      call $"#func4 __wasm_task_hook"
    )
    (func $"#func14 hook-cabi_realloc" (@name "hook-cabi_realloc") (;14;) (type 0) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $"#func4 __wasm_task_hook"
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $"#func9 cabi_realloc"
      i32.const 13
      call $"#func4 __wasm_task_hook"
    )
    (data (;0;) (i32.const 1048576) "\00\00\00\00\00\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $"#core-instance4 c" "cabi_realloc" (core func $cabi_realloc (;2;)))
  (alias export $test:test/test "bar" (func $bar (;0;)))
  (alias core export $wit-component-shim-instance "2" (core func $realloc-main-cabi_realloc (;3;)))
  (core func $"#core-func4 indirect-test:test/test-bar" (@name "indirect-test:test/test-bar") (;4;) (canon lower (func $bar) (memory $memory) (realloc $realloc-main-cabi_realloc) string-encoding=utf8))
  (alias core export $main "cabi_realloc" (core func $"#core-func5 cabi_realloc" (@name "cabi_realloc") (;5;)))
  (core instance $actual (;8;)
    (export "0" (func $cabi_realloc))
    (export "1" (func $"#core-func4 indirect-test:test/test-bar"))
    (export "2" (func $"#core-func5 cabi_realloc"))
  )
  (core instance $fixup (;9;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "c" (instance $"#core-instance4 c"))
      (with "foo" (instance $foo))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;6;)))
  (alias core export $fixup "hook1" (core func $hook1 (;7;)))
  (alias core export $fixup "hook2" (core func $hook2 (;8;)))
  (alias core export $fixup "hook3" (core func $hook3 (;9;)))
  (alias core export $fixup "hook4" (core func $hook4 (;10;)))
  (type (;1;) (func (result u32)))
  (func $run (;1;) (type 1) (canon lift (core func $hook0)))
  (export $"#func2 run" (@name "run") (;2;) "run" (func $run))
  (type (;2;) (func (param "name" string) (result string)))
  (func $greet (;3;) (type 2) (canon lift (core func $hook1) (memory $memory) (realloc $hook3) string-encoding=utf8 (post-return $hook2)))
  (export $"#func4 greet" (@name "greet") (;4;) "greet" (func $greet))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
