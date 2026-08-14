(component
  (type (;0;) (func (param "s" string)))
  (import "takes-string" (func $takes-string (;0;) (type 0)))
  (core module $main (;0;)
    (type (;0;) (func (param i32 i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func))
    (type (;3;) (func (param i32 i32 i32 i32) (result i32)))
    (import "$root" "takes-string" (func (;0;) (type 0)))
    (memory (;0;) 1)
    (export "__wasm_task_hook" (func 1))
    (export "run" (func 2))
    (export "cabi_realloc" (func 3))
    (export "memory" (memory 0))
    (func (;1;) (type 1) (param i32)
      unreachable
    )
    (func (;2;) (type 2))
    (func (;3;) (type 3) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component-shim-module (;1;)
    (type (;0;) (func (param i32 i32)))
    (table (;0;) 1 1 funcref)
    (export "0" (func $indirect-$root-takes-string))
    (export "$imports" (table 0))
    (func $indirect-$root-takes-string (;0;) (type 0) (param i32 i32)
      local.get 0
      local.get 1
      i32.const 0
      call_indirect (type 0)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "0" (core func $indirect-$root-takes-string (;0;)))
  (core instance $$root (;1;)
    (export "takes-string" (func $indirect-$root-takes-string))
  )
  (core instance $main (;2;) (instantiate $main
      (with "$root" (instance $$root))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core module $wit-component-fixup (;2;)
    (type (;0;) (func (param i32 i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func))
    (type (;3;) (func (param i32 i32 i32 i32) (result i32)))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "main" "__wasm_task_hook" (func $__wasm_task_hook (;1;) (type 1)))
    (import "main" "run" (func $run (;2;) (type 2)))
    (import "main" "cabi_realloc" (func $cabi_realloc (;3;) (type 3)))
    (import "shim" "$imports" (table (;0;) 1 1 funcref))
    (export "hook0" (func $hook-run))
    (export "hook1" (func $hook-cabi_realloc))
    (elem (;0;) (i32.const 0) func $0)
    (func $hook-run (;4;) (type 2)
      i32.const 0
      call $__wasm_task_hook
      call $run
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-cabi_realloc (;5;) (type 3) (param i32 i32 i32 i32) (result i32)
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
  (core func $"#core-func1 indirect-$root-takes-string" (@name "indirect-$root-takes-string") (;1;) (canon lower (func $takes-string) (memory $memory) string-encoding=utf8))
  (core instance $actual (;3;)
    (export "0" (func $"#core-func1 indirect-$root-takes-string"))
  )
  (core instance $fixup (;4;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;2;)))
  (alias core export $fixup "hook1" (core func $hook1 (;3;)))
  (type (;1;) (func))
  (func $run (;1;) (type 1) (canon lift (core func $hook0)))
  (export $"#func2 run" (@name "run") (;2;) "run" (func $run))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
