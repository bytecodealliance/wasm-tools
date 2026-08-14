(component
  (core module $main (;0;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32)))
    (table (;0;) 3 funcref)
    (memory (;0;) 17)
    (global (;0;) i32 i32.const 1048592)
    (global (;1;) i32 i32.const 1)
    (export "foo:memory_base" (global 0))
    (export "foo:table_base" (global 1))
    (export "cabi_realloc" (func 0))
    (export "__wasm_task_hook" (func 1))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (func (;0;) (type 0) (param i32 i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 1
      call_indirect (type 0)
    )
    (func (;1;) (type 1) (param i32)
      local.get 0
      i32.const 2
      call_indirect (type 1)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module $foo (;1;)
    (@dylink.0
      (mem-info (memory 0 4))
    )
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func (param i32 i32)))
    (import "env" "cabi_realloc" (func $cabi_realloc.0 (;0;) (type 0)))
    (export "cabi_realloc" (func $cabi_realloc.1))
    (export "test:test/test#foo" (func $foo))
    (export "__wasm_task_hook" (func $hook))
    (func $cabi_realloc.1 (;1;) (type 0) (param i32 i32 i32 i32) (result i32)
      i32.const -257976192
    )
    (func $foo (;2;) (type 2) (param i32 i32))
    (func $hook (;3;) (type 1) (param i32)
      unreachable
    )
  )
  (core instance $main (;0;) (instantiate $main))
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $main "cabi_realloc" (core func $cabi_realloc (;0;)))
  (core instance $env (;1;)
    (export "cabi_realloc" (func $cabi_realloc))
  )
  (core instance $foo (;2;) (instantiate $foo
      (with "env" (instance $env))
    )
  )
  (core module $wit-component-fixup (;2;)
    (type (;0;) (func))
    (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;2;) (func (param i32)))
    (type (;3;) (func (param i32 i32)))
    (import "main" "memory" (memory (;0;) 0))
    (import "main" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "foo" "cabi_realloc" (func $cabi_realloc (;0;) (type 1)))
    (import "foo" "__wasm_task_hook" (func $__wasm_task_hook (;1;) (type 2)))
    (import "main" "__wasm_task_hook" (func $"#func2 __wasm_task_hook" (@name "__wasm_task_hook") (;2;) (type 2)))
    (import "foo" "test:test/test#foo" (func $test:test/test#foo (;3;) (type 3)))
    (import "main" "cabi_realloc" (func $"#func4 cabi_realloc" (@name "cabi_realloc") (;4;) (type 1)))
    (export "hook0" (func $hook-test:test/test#foo))
    (export "hook1" (func $hook-cabi_realloc))
    (export "hook2" (func $"#func7 hook-cabi_realloc"))
    (elem (;0;) (i32.const 1) func)
    (elem (;1;) (i32.const 1) func $cabi_realloc $__wasm_task_hook)
    (func $hook-test:test/test#foo (;5;) (type 3) (param i32 i32)
      i32.const 0
      call $"#func2 __wasm_task_hook"
      local.get 0
      local.get 1
      call $test:test/test#foo
      i32.const 1
      call $"#func2 __wasm_task_hook"
    )
    (func $hook-cabi_realloc (;6;) (type 1) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $"#func2 __wasm_task_hook"
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $"#func4 cabi_realloc"
      i32.const 13
      call $"#func2 __wasm_task_hook"
    )
    (func $"#func7 hook-cabi_realloc" (@name "hook-cabi_realloc") (;7;) (type 1) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $"#func2 __wasm_task_hook"
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $cabi_realloc
      i32.const 13
      call $"#func2 __wasm_task_hook"
    )
    (data (;0;) (i32.const 1048576) "\00\00\00\00\00\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $fixup (;3;) (instantiate $wit-component-fixup
      (with "main" (instance $main))
      (with "foo" (instance $foo))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;1;)))
  (alias core export $fixup "hook1" (core func $hook1 (;2;)))
  (alias core export $fixup "hook2" (core func $hook2 (;3;)))
  (type (;0;) (func (param "x" string)))
  (func $foo (;0;) (type 0) (canon lift (core func $hook0) (memory $memory) (realloc $hook2) string-encoding=utf8))
  (component $test:test/test-shim-component (;0;)
    (type (;0;) (func (param "x" string)))
    (import "import-func-foo" (func (;0;) (type 0)))
    (type (;1;) (func (param "x" string)))
    (export (;1;) "foo" (func 0) (func (type 1)))
  )
  (instance $test:test/test-shim-instance (;0;) (instantiate $test:test/test-shim-component
      (with "import-func-foo" (func $foo))
    )
  )
  (export $test:test/test (;1;) "test:test/test" (instance $test:test/test-shim-instance))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
