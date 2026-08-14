(component
  (type (;0;) (func))
  (import "foo1" (func $foo1 (;0;) (type 0)))
  (type (;1;) (func (result string)))
  (import "bar" (func $bar (;1;) (type 1)))
  (core module $main (;0;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32)))
    (import "foo" "cabi_realloc" (func (;0;) (type 0)))
    (table (;0;) 2 funcref)
    (memory (;0;) 17)
    (global (;0;) i32 i32.const 1048592)
    (global (;1;) i32 i32.const 1)
    (global (;2;) i32 i32.const 1048592)
    (global (;3;) i32 i32.const 1)
    (global (;4;) (mut i32) i32.const 1048608)
    (global (;5;) (mut i32) i32.const 1114112)
    (export "cabi_realloc" (func 0))
    (export "c:memory_base" (global 0))
    (export "c:table_base" (global 1))
    (export "foo:memory_base" (global 2))
    (export "foo:table_base" (global 3))
    (export "__heap_base" (global 4))
    (export "__heap_end" (global 5))
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
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (type (;2;) (func (param i32)))
    (import "GOT.mem" "__heap_base" (global $__heap_base (;0;) (mut i32)))
    (import "GOT.mem" "__heap_end" (global $__heap_end (;1;) (mut i32)))
    (global $heap (;2;) (mut i32) i32.const 0)
    (export "__wasm_task_hook" (func 3))
    (export "malloc" (func $malloc))
    (export "abort" (func $abort))
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
    (func (;3;) (type 2) (param i32)
      unreachable
    )
  )
  (core module $foo (;2;)
    (@dylink.0
      (mem-info (memory 4 4))
      (needed "c")
    )
    (type (;0;) (func))
    (type (;1;) (func (param i32)))
    (type (;2;) (func (param i32 i32) (result i32)))
    (type (;3;) (func (param i32 i32 i32 i32) (result i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "$root" "foo1" (func (;0;) (type 0)))
    (import "$root" "bar" (func (;1;) (type 1)))
    (export "baz" (func 2))
    (export "foo2" (func 3))
    (export "cabi_post_foo2" (func 4))
    (export "cabi_realloc" (func 5))
    (export "cabi_import_realloc" (func 6))
    (export "cabi_export_realloc" (func 7))
    (func (;2;) (type 0))
    (func (;3;) (type 2) (param i32 i32) (result i32)
      unreachable
    )
    (func (;4;) (type 1) (param i32)
      unreachable
    )
    (func (;5;) (type 3) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (func (;6;) (type 3) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (func (;7;) (type 3) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
  )
  (core module $wit-component-shim-module (;3;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32)))
    (table (;0;) 3 3 funcref)
    (export "0" (func $adapt-foo-cabi_realloc))
    (export "1" (func $indirect-$root-bar))
    (export "2" (func $realloc-foo-cabi_import_realloc))
    (export "$imports" (table 0))
    (func $adapt-foo-cabi_realloc (;0;) (type 0) (param i32 i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 0
      call_indirect (type 0)
    )
    (func $indirect-$root-bar (;1;) (type 1) (param i32)
      local.get 0
      i32.const 1
      call_indirect (type 1)
    )
    (func $realloc-foo-cabi_import_realloc (;2;) (type 0) (param i32 i32 i32 i32) (result i32)
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
  (alias core export $wit-component-shim-instance "0" (core func $adapt-foo-cabi_realloc (;0;)))
  (core instance $foo (;1;)
    (export "cabi_realloc" (func $adapt-foo-cabi_realloc))
  )
  (core instance $main (;2;) (instantiate $main
      (with "foo" (instance $foo))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $main "__heap_base" (core global $__heap_base (;0;)))
  (alias core export $main "__heap_end" (core global $__heap_end (;1;)))
  (core instance $GOT.mem (;3;)
    (export "__heap_base" (global $__heap_base))
    (export "__heap_end" (global $__heap_end))
  )
  (core instance $c (;4;) (instantiate $c
      (with "GOT.mem" (instance $GOT.mem))
    )
  )
  (core instance $env (;5;)
    (export "memory" (memory $memory))
  )
  (core func $foo1 (;1;) (canon lower (func $foo1)))
  (alias core export $wit-component-shim-instance "1" (core func $indirect-$root-bar (;2;)))
  (core instance $$root (;6;)
    (export "foo1" (func $foo1))
    (export "bar" (func $indirect-$root-bar))
  )
  (core instance $"#core-instance7 foo" (@name "foo") (;7;) (instantiate $foo
      (with "env" (instance $env))
      (with "$root" (instance $$root))
    )
  )
  (core module $wit-component-fixup (;4;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func))
    (type (;3;) (func (param i32 i32) (result i32)))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "actual" "1" (func $1 (;1;) (type 1)))
    (import "actual" "2" (func $2 (;2;) (type 0)))
    (import "main" "memory" (memory (;0;) 0))
    (import "main" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "c" "__wasm_task_hook" (func $__wasm_task_hook (;3;) (type 1)))
    (import "main" "__wasm_task_hook" (func $"#func4 __wasm_task_hook" (@name "__wasm_task_hook") (;4;) (type 1)))
    (import "foo" "baz" (func $baz (;5;) (type 2)))
    (import "foo" "foo2" (func $foo2 (;6;) (type 3)))
    (import "foo" "cabi_post_foo2" (func $cabi_post_foo2 (;7;) (type 1)))
    (import "main" "cabi_realloc" (func $cabi_realloc (;8;) (type 0)))
    (import "foo" "cabi_realloc" (func $"#func9 cabi_realloc" (@name "cabi_realloc") (;9;) (type 0)))
    (import "foo" "cabi_import_realloc" (func $cabi_import_realloc (;10;) (type 0)))
    (import "foo" "cabi_export_realloc" (func $cabi_export_realloc (;11;) (type 0)))
    (import "shim" "$imports" (table (;1;) 3 3 funcref))
    (export "hook0" (func $hook-baz))
    (export "hook1" (func $hook-foo2))
    (export "hook2" (func $hook-cabi_post_foo2))
    (export "hook3" (func $hook-cabi_realloc))
    (export "hook4" (func $"#func16 hook-cabi_realloc"))
    (export "hook5" (func $hook-cabi_import_realloc))
    (export "hook6" (func $hook-cabi_export_realloc))
    (elem (;0;) (i32.const 1) func)
    (elem (;1;) (i32.const 1) func $__wasm_task_hook)
    (elem (;2;) (table 1) (i32.const 0) func $0 $1 $hook-cabi_import_realloc)
    (func $hook-baz (;12;) (type 2)
      i32.const 0
      call $"#func4 __wasm_task_hook"
      call $baz
      i32.const 1
      call $"#func4 __wasm_task_hook"
    )
    (func $hook-foo2 (;13;) (type 3) (param i32 i32) (result i32)
      i32.const 0
      call $"#func4 __wasm_task_hook"
      local.get 0
      local.get 1
      call $foo2
      i32.const 1
      call $"#func4 __wasm_task_hook"
    )
    (func $hook-cabi_post_foo2 (;14;) (type 1) (param i32)
      i32.const 10
      call $"#func4 __wasm_task_hook"
      local.get 0
      call $cabi_post_foo2
      i32.const 11
      call $"#func4 __wasm_task_hook"
    )
    (func $hook-cabi_realloc (;15;) (type 0) (param i32 i32 i32 i32) (result i32)
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
    (func $"#func16 hook-cabi_realloc" (@name "hook-cabi_realloc") (;16;) (type 0) (param i32 i32 i32 i32) (result i32)
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
    (func $hook-cabi_import_realloc (;17;) (type 0) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $"#func4 __wasm_task_hook"
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $cabi_import_realloc
      i32.const 13
      call $"#func4 __wasm_task_hook"
    )
    (func $hook-cabi_export_realloc (;18;) (type 0) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $"#func4 __wasm_task_hook"
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $cabi_export_realloc
      i32.const 13
      call $"#func4 __wasm_task_hook"
    )
    (data (;0;) (i32.const 1048576) "\00\00\00\00\00\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $"#core-instance7 foo" "cabi_realloc" (core func $cabi_realloc (;3;)))
  (alias core export $wit-component-shim-instance "2" (core func $realloc-foo-cabi_import_realloc (;4;)))
  (core func $"#core-func5 indirect-$root-bar" (@name "indirect-$root-bar") (;5;) (canon lower (func $bar) (memory $memory) (realloc $realloc-foo-cabi_import_realloc) string-encoding=utf8))
  (alias core export $"#core-instance7 foo" "cabi_import_realloc" (core func $cabi_import_realloc (;6;)))
  (core instance $actual (;8;)
    (export "0" (func $cabi_realloc))
    (export "1" (func $"#core-func5 indirect-$root-bar"))
    (export "2" (func $cabi_import_realloc))
  )
  (core instance $fixup (;9;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "c" (instance $c))
      (with "foo" (instance $"#core-instance7 foo"))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;7;)))
  (alias core export $fixup "hook1" (core func $hook1 (;8;)))
  (alias core export $fixup "hook2" (core func $hook2 (;9;)))
  (alias core export $fixup "hook3" (core func $hook3 (;10;)))
  (alias core export $fixup "hook4" (core func $hook4 (;11;)))
  (alias core export $fixup "hook5" (core func $hook5 (;12;)))
  (alias core export $fixup "hook6" (core func $hook6 (;13;)))
  (func $baz (;2;) (type 0) (canon lift (core func $hook0)))
  (export $"#func3 baz" (@name "baz") (;3;) "baz" (func $baz))
  (type (;2;) (list u8))
  (type (;3;) (option 2))
  (type (;4;) (func (param "x" string) (result 3)))
  (func $foo2 (;4;) (type 4) (canon lift (core func $hook1) (memory $memory) (realloc $hook6) string-encoding=utf8 (post-return $hook2)))
  (export $"#func5 foo2" (@name "foo2") (;5;) "foo2" (func $foo2))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
