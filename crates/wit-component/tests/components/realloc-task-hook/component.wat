(component
  (type (;0;) (func (result string)))
  (import "returns-string" (func $returns-string (;0;) (type 0)))
  (type (;1;) (func (param "s" string)))
  (import "takes-string" (func $takes-string (;1;) (type 1)))
  (core module $main (;0;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func (param i32 i32)))
    (type (;2;) (func (param i32 i32) (result i32)))
    (type (;3;) (func))
    (type (;4;) (func (param i32 i32 i32 i32) (result i32)))
    (import "$root" "returns-string" (func (;0;) (type 0)))
    (import "$root" "takes-string" (func (;1;) (type 1)))
    (memory (;0;) 1)
    (export "__wasm_task_hook" (func 2))
    (export "takes-and-returns" (func 3))
    (export "cabi_post_takes-and-returns" (func 4))
    (export "no-realloc" (func 5))
    (export "x#with-string" (func 6))
    (export "x#[dtor]r" (func 7))
    (export "cabi_import_realloc" (func 8))
    (export "cabi_export_realloc" (func 9))
    (export "memory" (memory 0))
    (func (;2;) (type 0) (param i32)
      unreachable
    )
    (func (;3;) (type 2) (param i32 i32) (result i32)
      unreachable
    )
    (func (;4;) (type 0) (param i32)
      unreachable
    )
    (func (;5;) (type 3))
    (func (;6;) (type 1) (param i32 i32)
      unreachable
    )
    (func (;7;) (type 0) (param i32)
      unreachable
    )
    (func (;8;) (type 4) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (func (;9;) (type 4) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component-shim-module (;1;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;2;) (func (param i32 i32)))
    (table (;0;) 4 4 funcref)
    (export "0" (func $indirect-$root-returns-string))
    (export "1" (func $realloc-main-cabi_import_realloc))
    (export "2" (func $indirect-$root-takes-string))
    (export "3" (func $dtor-r))
    (export "$imports" (table 0))
    (func $indirect-$root-returns-string (;0;) (type 0) (param i32)
      local.get 0
      i32.const 0
      call_indirect (type 0)
    )
    (func $realloc-main-cabi_import_realloc (;1;) (type 1) (param i32 i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 1
      call_indirect (type 1)
    )
    (func $indirect-$root-takes-string (;2;) (type 2) (param i32 i32)
      local.get 0
      local.get 1
      i32.const 2
      call_indirect (type 2)
    )
    (func $dtor-r (;3;) (type 0) (param i32)
      local.get 0
      i32.const 3
      call_indirect (type 0)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "3" (core func $dtor-r (;0;)))
  (type $r (;2;) (resource (rep i32) (dtor $dtor-r)))
  (alias core export $wit-component-shim-instance "0" (core func $indirect-$root-returns-string (;1;)))
  (alias core export $wit-component-shim-instance "2" (core func $indirect-$root-takes-string (;2;)))
  (core instance $$root (;1;)
    (export "returns-string" (func $indirect-$root-returns-string))
    (export "takes-string" (func $indirect-$root-takes-string))
  )
  (core instance $main (;2;) (instantiate $main
      (with "$root" (instance $$root))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core module $wit-component-fixup (;2;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;2;) (func (param i32 i32)))
    (type (;3;) (func (param i32 i32) (result i32)))
    (type (;4;) (func))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "actual" "1" (func $1 (;1;) (type 1)))
    (import "actual" "2" (func $2 (;2;) (type 2)))
    (import "actual" "3" (func $3 (;3;) (type 0)))
    (import "main" "__wasm_task_hook" (func $__wasm_task_hook (;4;) (type 0)))
    (import "main" "takes-and-returns" (func $takes-and-returns (;5;) (type 3)))
    (import "main" "cabi_post_takes-and-returns" (func $cabi_post_takes-and-returns (;6;) (type 0)))
    (import "main" "no-realloc" (func $no-realloc (;7;) (type 4)))
    (import "main" "x#with-string" (func $x#with-string (;8;) (type 2)))
    (import "main" "cabi_import_realloc" (func $cabi_import_realloc (;9;) (type 1)))
    (import "main" "cabi_export_realloc" (func $cabi_export_realloc (;10;) (type 1)))
    (import "shim" "$imports" (table (;0;) 4 4 funcref))
    (export "hook0" (func $hook-takes-and-returns))
    (export "hook1" (func $hook-cabi_post_takes-and-returns))
    (export "hook2" (func $hook-no-realloc))
    (export "hook3" (func $hook-x#with-string))
    (export "hook4" (func $hook-cabi_import_realloc))
    (export "hook5" (func $hook-cabi_export_realloc))
    (elem (;0;) (i32.const 0) func $0 $hook-cabi_import_realloc $2 $hook-resource-dtor)
    (func $hook-takes-and-returns (;11;) (type 3) (param i32 i32) (result i32)
      i32.const 0
      call $__wasm_task_hook
      local.get 0
      local.get 1
      call $takes-and-returns
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-cabi_post_takes-and-returns (;12;) (type 0) (param i32)
      i32.const 10
      call $__wasm_task_hook
      local.get 0
      call $cabi_post_takes-and-returns
      i32.const 11
      call $__wasm_task_hook
    )
    (func $hook-no-realloc (;13;) (type 4)
      i32.const 0
      call $__wasm_task_hook
      call $no-realloc
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-x#with-string (;14;) (type 2) (param i32 i32)
      i32.const 0
      call $__wasm_task_hook
      local.get 0
      local.get 1
      call $x#with-string
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-cabi_import_realloc (;15;) (type 1) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $__wasm_task_hook
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $cabi_import_realloc
      i32.const 13
      call $__wasm_task_hook
    )
    (func $hook-cabi_export_realloc (;16;) (type 1) (param i32 i32 i32 i32) (result i32)
      i32.const 12
      call $__wasm_task_hook
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $cabi_export_realloc
      i32.const 13
      call $__wasm_task_hook
    )
    (func $hook-resource-dtor (;17;) (type 0) (param i32)
      i32.const 8
      call $__wasm_task_hook
      local.get 0
      call $3
      i32.const 9
      call $__wasm_task_hook
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $wit-component-shim-instance "1" (core func $realloc-main-cabi_import_realloc (;3;)))
  (core func $"#core-func4 indirect-$root-returns-string" (@name "indirect-$root-returns-string") (;4;) (canon lower (func $returns-string) (memory $memory) (realloc $realloc-main-cabi_import_realloc) string-encoding=utf8))
  (alias core export $main "cabi_import_realloc" (core func $cabi_import_realloc (;5;)))
  (core func $"#core-func6 indirect-$root-takes-string" (@name "indirect-$root-takes-string") (;6;) (canon lower (func $takes-string) (memory $memory) string-encoding=utf8))
  (alias core export $main "x#[dtor]r" (core func $"x#[dtor]r" (;7;)))
  (core instance $actual (;3;)
    (export "0" (func $"#core-func4 indirect-$root-returns-string"))
    (export "1" (func $cabi_import_realloc))
    (export "2" (func $"#core-func6 indirect-$root-takes-string"))
    (export "3" (func $"x#[dtor]r"))
  )
  (core instance $fixup (;4;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;8;)))
  (alias core export $fixup "hook1" (core func $hook1 (;9;)))
  (alias core export $fixup "hook2" (core func $hook2 (;10;)))
  (alias core export $fixup "hook3" (core func $hook3 (;11;)))
  (alias core export $fixup "hook4" (core func $hook4 (;12;)))
  (alias core export $fixup "hook5" (core func $hook5 (;13;)))
  (type (;3;) (func (param "s" string) (result string)))
  (func $takes-and-returns (;2;) (type 3) (canon lift (core func $hook0) (memory $memory) (realloc $hook5) string-encoding=utf8 (post-return $hook1)))
  (export $"#func3 takes-and-returns" (@name "takes-and-returns") (;3;) "takes-and-returns" (func $takes-and-returns))
  (type (;4;) (func))
  (func $no-realloc (;4;) (type 4) (canon lift (core func $hook2)))
  (export $"#func5 no-realloc" (@name "no-realloc") (;5;) "no-realloc" (func $no-realloc))
  (func $with-string (;6;) (type 1) (canon lift (core func $hook3) (memory $memory) (realloc $hook5) string-encoding=utf8))
  (component $x-shim-component (;0;)
    (import "import-type-r" (type (;0;) (sub resource)))
    (type (;1;) (func (param "s" string)))
    (import "import-func-with-string" (func (;0;) (type 1)))
    (export (;2;) "r" (type 0))
    (type (;3;) (func (param "s" string)))
    (export (;1;) "with-string" (func 0) (func (type 3)))
  )
  (instance $x-shim-instance (;0;) (instantiate $x-shim-component
      (with "import-func-with-string" (func $with-string))
      (with "import-type-r" (type $r))
    )
  )
  (export $x (;1;) "x" (instance $x-shim-instance))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
