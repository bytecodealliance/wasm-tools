(component
  (core module $main (;0;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i32 i32 i32) (result i32)))
    (type (;4;) (func (param i32 i32)))
    (type (;5;) (func (param i32 i32) (result i32)))
    (type (;6;) (func (param i32 i32 i32 i32) (result i32)))
    (memory (;0;) 1)
    (export "__wasm_task_hook" (func 0))
    (export "[async-lift-stackful]async-stackful" (func 1))
    (export "[async-lift]async-callback" (func 2))
    (export "[callback][async-lift]async-callback" (func 3))
    (export "sync" (func 4))
    (export "cabi_post_sync" (func 5))
    (export "_initialize" (func 6))
    (export "[async-lift-stackful]async-stackful-argret" (func 7))
    (export "[async-lift]async-callback-argret" (func 8))
    (export "[callback][async-lift]async-callback-argret" (func 9))
    (export "sync-argret" (func 10))
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 11))
    (export "x#sync" (func 12))
    (export "x#[dtor]r" (func 13))
    (func (;0;) (type 0) (param i32)
      unreachable
    )
    (func (;1;) (type 1)
      unreachable
    )
    (func (;2;) (type 2) (result i32)
      unreachable
    )
    (func (;3;) (type 3) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func (;4;) (type 1)
      unreachable
    )
    (func (;5;) (type 1)
      unreachable
    )
    (func (;6;) (type 1))
    (func (;7;) (type 4) (param i32 i32)
      unreachable
    )
    (func (;8;) (type 5) (param i32 i32) (result i32)
      unreachable
    )
    (func (;9;) (type 3) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func (;10;) (type 5) (param i32 i32) (result i32)
      unreachable
    )
    (func (;11;) (type 6) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (func (;12;) (type 1)
      unreachable
    )
    (func (;13;) (type 0) (param i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component-shim-module (;1;)
    (type (;0;) (func (param i32)))
    (table (;0;) 1 1 funcref)
    (export "0" (func $dtor-r))
    (export "$imports" (table 0))
    (func $dtor-r (;0;) (type 0) (param i32)
      local.get 0
      i32.const 0
      call_indirect (type 0)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (alias core export $wit-component-shim-instance "0" (core func $dtor-r (;0;)))
  (type $r (;0;) (resource (rep i32) (dtor $dtor-r)))
  (core instance $main (;1;) (instantiate $main))
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core module $wit-component-fixup (;2;)
    (type (;0;) (func (param i32)))
    (type (;1;) (func))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i32 i32 i32) (result i32)))
    (type (;4;) (func (param i32 i32)))
    (type (;5;) (func (param i32 i32) (result i32)))
    (type (;6;) (func (param i32 i32 i32 i32) (result i32)))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "main" "_initialize" (func $_initialize (;1;) (type 1)))
    (import "main" "__wasm_task_hook" (func $__wasm_task_hook (;2;) (type 0)))
    (import "main" "[async-lift-stackful]async-stackful" (func $"[async-lift-stackful]async-stackful" (;3;) (type 1)))
    (import "main" "[async-lift]async-callback" (func $"[async-lift]async-callback" (;4;) (type 2)))
    (import "main" "[callback][async-lift]async-callback" (func $"[callback][async-lift]async-callback" (;5;) (type 3)))
    (import "main" "sync" (func $sync (;6;) (type 1)))
    (import "main" "cabi_post_sync" (func $cabi_post_sync (;7;) (type 1)))
    (import "main" "[async-lift-stackful]async-stackful-argret" (func $"[async-lift-stackful]async-stackful-argret" (;8;) (type 4)))
    (import "main" "[async-lift]async-callback-argret" (func $"[async-lift]async-callback-argret" (;9;) (type 5)))
    (import "main" "[callback][async-lift]async-callback-argret" (func $"[callback][async-lift]async-callback-argret" (;10;) (type 3)))
    (import "main" "sync-argret" (func $sync-argret (;11;) (type 5)))
    (import "main" "x#sync" (func $x#sync (;12;) (type 1)))
    (import "main" "cabi_realloc" (func $cabi_realloc (;13;) (type 6)))
    (import "shim" "$imports" (table (;0;) 1 1 funcref))
    (export "hook0" (func $"hook-[async-lift-stackful]async-stackful"))
    (export "hook1" (func $"hook-[async-lift]async-callback"))
    (export "hook2" (func $"hook-[callback][async-lift]async-callback"))
    (export "hook3" (func $hook-sync))
    (export "hook4" (func $hook-cabi_post_sync))
    (export "hook5" (func $"hook-[async-lift-stackful]async-stackful-argret"))
    (export "hook6" (func $"hook-[async-lift]async-callback-argret"))
    (export "hook7" (func $"hook-[callback][async-lift]async-callback-argret"))
    (export "hook8" (func $hook-sync-argret))
    (export "hook9" (func $hook-x#sync))
    (export "hook10" (func $hook-cabi_realloc))
    (start $start)
    (elem (;0;) (i32.const 0) func $hook-resource-dtor)
    (func $"hook-[async-lift-stackful]async-stackful" (;14;) (type 1)
      i32.const 2
      call $__wasm_task_hook
      call $"[async-lift-stackful]async-stackful"
      i32.const 5
      call $__wasm_task_hook
    )
    (func $"hook-[async-lift]async-callback" (;15;) (type 2) (result i32)
      (local i32)
      i32.const 2
      call $__wasm_task_hook
      call $"[async-lift]async-callback"
      local.set 0
      i32.const 4
      i32.const 5
      local.get 0
      select
      call $__wasm_task_hook
      local.get 0
    )
    (func $"hook-[callback][async-lift]async-callback" (;16;) (type 3) (param i32 i32 i32) (result i32)
      (local i32)
      i32.const 3
      call $__wasm_task_hook
      local.get 0
      local.get 1
      local.get 2
      call $"[callback][async-lift]async-callback"
      local.set 3
      i32.const 4
      i32.const 5
      local.get 3
      select
      call $__wasm_task_hook
      local.get 3
    )
    (func $hook-sync (;17;) (type 1)
      i32.const 0
      call $__wasm_task_hook
      call $sync
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-cabi_post_sync (;18;) (type 1)
      i32.const 10
      call $__wasm_task_hook
      call $cabi_post_sync
      i32.const 11
      call $__wasm_task_hook
    )
    (func $"hook-[async-lift-stackful]async-stackful-argret" (;19;) (type 4) (param i32 i32)
      i32.const 2
      call $__wasm_task_hook
      local.get 0
      local.get 1
      call $"[async-lift-stackful]async-stackful-argret"
      i32.const 5
      call $__wasm_task_hook
    )
    (func $"hook-[async-lift]async-callback-argret" (;20;) (type 5) (param i32 i32) (result i32)
      (local i32)
      i32.const 2
      call $__wasm_task_hook
      local.get 0
      local.get 1
      call $"[async-lift]async-callback-argret"
      local.set 2
      i32.const 4
      i32.const 5
      local.get 2
      select
      call $__wasm_task_hook
      local.get 2
    )
    (func $"hook-[callback][async-lift]async-callback-argret" (;21;) (type 3) (param i32 i32 i32) (result i32)
      (local i32)
      i32.const 3
      call $__wasm_task_hook
      local.get 0
      local.get 1
      local.get 2
      call $"[callback][async-lift]async-callback-argret"
      local.set 3
      i32.const 4
      i32.const 5
      local.get 3
      select
      call $__wasm_task_hook
      local.get 3
    )
    (func $hook-sync-argret (;22;) (type 5) (param i32 i32) (result i32)
      i32.const 0
      call $__wasm_task_hook
      local.get 0
      local.get 1
      call $sync-argret
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-x#sync (;23;) (type 1)
      i32.const 0
      call $__wasm_task_hook
      call $x#sync
      i32.const 1
      call $__wasm_task_hook
    )
    (func $hook-cabi_realloc (;24;) (type 6) (param i32 i32 i32 i32) (result i32)
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
    (func $hook-resource-dtor (;25;) (type 0) (param i32)
      i32.const 8
      call $__wasm_task_hook
      local.get 0
      call $0
      i32.const 9
      call $__wasm_task_hook
    )
    (func $start (;26;) (type 1)
      i32.const 6
      call $__wasm_task_hook
      call $_initialize
      i32.const 7
      call $__wasm_task_hook
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $main "x#[dtor]r" (core func $"x#[dtor]r" (;1;)))
  (core instance $actual (;2;)
    (export "0" (func $"x#[dtor]r"))
  )
  (core instance $fixup (;3;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (alias core export $fixup "hook0" (core func $hook0 (;2;)))
  (alias core export $fixup "hook1" (core func $hook1 (;3;)))
  (alias core export $fixup "hook2" (core func $hook2 (;4;)))
  (alias core export $fixup "hook3" (core func $hook3 (;5;)))
  (alias core export $fixup "hook4" (core func $hook4 (;6;)))
  (alias core export $fixup "hook5" (core func $hook5 (;7;)))
  (alias core export $fixup "hook6" (core func $hook6 (;8;)))
  (alias core export $fixup "hook7" (core func $hook7 (;9;)))
  (alias core export $fixup "hook8" (core func $hook8 (;10;)))
  (alias core export $fixup "hook9" (core func $hook9 (;11;)))
  (alias core export $fixup "hook10" (core func $hook10 (;12;)))
  (type (;1;) (func async))
  (func $async-stackful (;0;) (type 1) (canon lift (core func $hook0) async))
  (export $"#func1 async-stackful" (@name "async-stackful") (;1;) "async-stackful" (func $async-stackful))
  (func $async-callback (;2;) (type 1) (canon lift (core func $hook1) async (callback $hook2)))
  (export $"#func3 async-callback" (@name "async-callback") (;3;) "async-callback" (func $async-callback))
  (type (;2;) (func))
  (func $sync (;4;) (type 2) (canon lift (core func $hook3) (post-return $hook4)))
  (export $"#func5 sync" (@name "sync") (;5;) "sync" (func $sync))
  (type (;3;) (func async (param "s" string) (result string)))
  (func $async-stackful-argret (;6;) (type 3) (canon lift (core func $hook5) (memory $memory) (realloc $hook10) string-encoding=utf8 async))
  (export $"#func7 async-stackful-argret" (@name "async-stackful-argret") (;7;) "async-stackful-argret" (func $async-stackful-argret))
  (func $async-callback-argret (;8;) (type 3) (canon lift (core func $hook6) (memory $memory) (realloc $hook10) string-encoding=utf8 async (callback $hook7)))
  (export $"#func9 async-callback-argret" (@name "async-callback-argret") (;9;) "async-callback-argret" (func $async-callback-argret))
  (type (;4;) (func (param "s" string) (result string)))
  (func $sync-argret (;10;) (type 4) (canon lift (core func $hook8) (memory $memory) (realloc $hook10) string-encoding=utf8))
  (export $"#func11 sync-argret" (@name "sync-argret") (;11;) "sync-argret" (func $sync-argret))
  (func $"#func12 sync" (@name "sync") (;12;) (type 2) (canon lift (core func $hook9)))
  (component $x-shim-component (;0;)
    (import "import-type-r" (type (;0;) (sub resource)))
    (type (;1;) (func))
    (import "import-func-sync" (func (;0;) (type 1)))
    (export (;2;) "r" (type 0))
    (type (;3;) (func))
    (export (;1;) "sync" (func 0) (func (type 3)))
  )
  (instance $x-shim-instance (;0;) (instantiate $x-shim-component
      (with "import-func-sync" (func $"#func12 sync"))
      (with "import-type-r" (type $r))
    )
  )
  (export $x (;1;) "x" (instance $x-shim-instance))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
