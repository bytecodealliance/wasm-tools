(component
  (type (;0;) (func (result string)))
  (import "returns-string" (func $returns-string (;0;) (type 0)))
  (type (;1;) (func (param "s" string)))
  (import "takes-string" (func $takes-string (;1;) (type 1)))
  (core module $main (;0;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func (param i32 i32)))
    (type (;2;) (func (param i32 i32) (result i32)))
    (type (;3;) (func (param i32 i32 i32 i32) (result i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "$root" "returns-string" (func (;0;) (type 0)))
    (import "$root" "takes-string" (func (;1;) (type 1)))
    (export "__wasm_task_hook" (func 2))
    (export "takes-and-returns" (func 3))
    (export "cabi_realloc" (func 4))
    (func (;2;) (type 0) (param i32)
      unreachable
    )
    (func (;3;) (type 2) (param i32 i32) (result i32)
      unreachable
    )
    (func (;4;) (type 3) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component-shim-module (;1;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (table (;0;) 1 1 funcref)
    (memory (;0;) 1)
    (export "0" (func $realloc-main-cabi_realloc))
    (export "$imports" (table 0))
    (export "$memory" (memory 0))
    (func $realloc-main-cabi_realloc (;0;) (type 0) (param i32 i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 0
      call_indirect (type 0)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "$memory" (core memory $memory (;0;)))
  (core instance $env (;1;)
    (export "memory" (memory $memory))
  )
  (alias core export $wit-component-shim-instance "0" (core func $realloc-main-cabi_realloc (;0;)))
  (core func $returns-string (;1;) (canon lower (func $returns-string) (memory $memory) (realloc $realloc-main-cabi_realloc) string-encoding=utf8))
  (core func $takes-string (;2;) (canon lower (func $takes-string) (memory $memory) string-encoding=utf8))
  (core instance $$root (;2;)
    (export "returns-string" (func $returns-string))
    (export "takes-string" (func $takes-string))
  )
  (core instance $main (;3;) (instantiate $main
      (with "env" (instance $env))
      (with "$root" (instance $$root))
    )
  )
  (core module $wit-component-fixup (;2;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func (param i32 i32) (result i32)))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "main" "__wasm_task_hook" (func $__wasm_task_hook (;1;) (type 1)))
    (import "main" "takes-and-returns" (func $takes-and-returns (;2;) (type 2)))
    (import "main" "cabi_realloc" (func $cabi_realloc (;3;) (type 0)))
    (import "shim" "$imports" (table (;0;) 1 1 funcref))
    (export "hook0" (func $hook-takes-and-returns))
    (export "hook1" (func $hook-cabi_realloc))
    (elem (;0;) (i32.const 0) func $hook-cabi_realloc)
    (func $hook-takes-and-returns (;4;) (type 2) (param i32 i32) (result i32)
      i32.const 0
      call $__wasm_task_hook
      local.get 0
      local.get 1
      call $takes-and-returns
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-cabi_realloc (;5;) (type 0) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $__wasm_task_hook
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $cabi_realloc
      i32.const 13
      call $__wasm_task_hook
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $main "cabi_realloc" (core func $cabi_realloc (;3;)))
  (core instance $actual (;4;)
    (export "0" (func $cabi_realloc))
  )
  (core instance $fixup (;5;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;4;)))
  (alias core export $fixup "hook1" (core func $hook1 (;5;)))
  (type (;2;) (func (param "s" string) (result string)))
  (func $takes-and-returns (;2;) (type 2) (canon lift (core func $hook0) (memory $memory) (realloc $hook1) string-encoding=utf8))
  (export $"#func3 takes-and-returns" (@name "takes-and-returns") (;3;) "takes-and-returns" (func $takes-and-returns))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
