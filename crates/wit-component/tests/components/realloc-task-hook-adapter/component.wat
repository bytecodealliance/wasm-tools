(component
  (type $ty-new (;0;)
    (instance
      (type (;0;) (list u8))
      (type (;1;) (func (param "amt" u32) (result 0)))
      (export (;0;) "read" (func (type 1)))
    )
  )
  (import "new" (instance $new (;0;) (type $ty-new)))
  (core module $main (;0;)
    (type (;0;) (func (param i32 i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func (param i32 i32 i32 i32) (result i32)))
    (import "old" "read" (func (;0;) (type 0)))
    (memory (;0;) 1)
    (export "main" (func 1))
    (export "__wasm_task_hook" (func 2))
    (export "cabi_realloc" (func 3))
    (export "memory" (memory 0))
    (func (;1;) (type 0) (param $args i32) (param $argv i32))
    (func (;2;) (type 1) (param i32)
      unreachable
    )
    (func (;3;) (type 2) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component:adapter:old (;1;)
    (type (;0;) (func (param i32 i32)))
    (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
    (import "new" "read" (func $read (;0;) (type 0)))
    (global $sp (;0;) (mut i32) i32.const 0)
    (export "entrypoint" (func 1))
    (export "cabi_export_realloc" (func 2))
    (export "read" (func 3))
    (export "cabi_import_realloc" (func 4))
    (func (;1;) (type 0) (param i32 i32)
      unreachable
    )
    (func (;2;) (type 1) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (func (;3;) (type 0) (param i32 i32)
      (local i32)
      global.get $sp
      i32.const 8
      i32.sub
      local.tee 2
      global.set $sp
      local.get 1
      local.get 2
      call $read
      local.get 2
      i32.const 8
      i32.add
      global.set $sp
    )
    (func (;4;) (type 1) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
  )
  (core module $wit-component-shim-module (;2;)
    (type (;0;) (func (param i32 i32)))
    (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
    (table (;0;) 3 3 funcref)
    (export "0" (func $adapt-old-read))
    (export "1" (func $indirect-new-read))
    (export "2" (func $realloc-old-cabi_import_realloc))
    (export "$imports" (table 0))
    (func $adapt-old-read (;0;) (type 0) (param i32 i32)
      local.get 0
      local.get 1
      i32.const 0
      call_indirect (type 0)
    )
    (func $indirect-new-read (;1;) (type 0) (param i32 i32)
      local.get 0
      local.get 1
      i32.const 1
      call_indirect (type 0)
    )
    (func $realloc-old-cabi_import_realloc (;2;) (type 1) (param i32 i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 2
      call_indirect (type 1)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "0" (core func $adapt-old-read (;0;)))
  (core instance $old (;1;)
    (export "read" (func $adapt-old-read))
  )
  (core instance $main (;2;) (instantiate $main
      (with "old" (instance $old))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $wit-component-shim-instance "1" (core func $indirect-new-read (;1;)))
  (core instance $new (;3;)
    (export "read" (func $indirect-new-read))
  )
  (core instance $"#core-instance4 old" (@name "old") (;4;) (instantiate $wit-component:adapter:old
      (with "new" (instance $new))
    )
  )
  (core module $wit-component-fixup (;3;)
    (type (;0;) (func (param i32 i32)))
    (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;2;) (func (param i32)))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "actual" "1" (func $1 (;1;) (type 0)))
    (import "actual" "2" (func $2 (;2;) (type 1)))
    (import "main" "__wasm_task_hook" (func $__wasm_task_hook (;3;) (type 2)))
    (import "old" "entrypoint" (func $entrypoint (;4;) (type 0)))
    (import "main" "cabi_realloc" (func $cabi_realloc (;5;) (type 1)))
    (import "old" "cabi_export_realloc" (func $cabi_export_realloc (;6;) (type 1)))
    (import "old" "cabi_import_realloc" (func $cabi_import_realloc (;7;) (type 1)))
    (import "shim" "$imports" (table (;0;) 3 3 funcref))
    (export "hook0" (func $hook-entrypoint))
    (export "hook1" (func $hook-cabi_realloc))
    (export "hook2" (func $hook-cabi_export_realloc))
    (export "hook3" (func $hook-cabi_import_realloc))
    (elem (;0;) (i32.const 0) func $0 $1 $hook-cabi_import_realloc)
    (func $hook-entrypoint (;8;) (type 0) (param i32 i32)
      i32.const 0
      call $__wasm_task_hook
      local.get 0
      local.get 1
      call $entrypoint
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-cabi_realloc (;9;) (type 1) (param i32 i32 i32 i32) (result i32)
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
    (func $hook-cabi_export_realloc (;10;) (type 1) (param i32 i32 i32 i32) (result i32)
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
    (func $hook-cabi_import_realloc (;11;) (type 1) (param i32 i32 i32 i32) (result i32)
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
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $"#core-instance4 old" "read" (core func $read (;2;)))
  (alias export $new "read" (func $read (;0;)))
  (alias core export $wit-component-shim-instance "2" (core func $realloc-old-cabi_import_realloc (;3;)))
  (core func $"#core-func4 indirect-new-read" (@name "indirect-new-read") (;4;) (canon lower (func $read) (memory $memory) (realloc $realloc-old-cabi_import_realloc)))
  (alias core export $"#core-instance4 old" "cabi_import_realloc" (core func $cabi_import_realloc (;5;)))
  (core instance $actual (;5;)
    (export "0" (func $read))
    (export "1" (func $"#core-func4 indirect-new-read"))
    (export "2" (func $cabi_import_realloc))
  )
  (core instance $fixup (;6;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "old" (instance $"#core-instance4 old"))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;6;)))
  (alias core export $fixup "hook1" (core func $hook1 (;7;)))
  (alias core export $fixup "hook2" (core func $hook2 (;8;)))
  (alias core export $fixup "hook3" (core func $hook3 (;9;)))
  (type (;1;) (list string))
  (type (;2;) (func (param "args" 1)))
  (func $entrypoint (;1;) (type 2) (canon lift (core func $hook0) (memory $memory) (realloc $hook2) string-encoding=utf8))
  (export $"#func2 entrypoint" (@name "entrypoint") (;2;) "entrypoint" (func $entrypoint))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
