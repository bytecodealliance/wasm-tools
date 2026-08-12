(component
  (core module $main (;0;)
    (type (;0;) (func))
    (type (;1;) (func (result i32)))
    (type (;2;) (func (param i32 i32 i32) (result i32)))
    (type (;3;) (func (param i32 i32)))
    (type (;4;) (func (param i32 i32) (result i32)))
    (type (;5;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;6;) (func (param i32)))
    (memory (;0;) 1)
    (export "__wasm_init_task" (func 0))
    (export "__wasm_init_async_task" (func 1))
    (export "[async-lift-stackful]async-stackful" (func 2))
    (export "[async-lift]async-callback" (func 3))
    (export "[callback][async-lift]async-callback" (func 4))
    (export "sync" (func 5))
    (export "_initialize" (func 6))
    (export "[async-lift-stackful]async-stackful-argret" (func 7))
    (export "[async-lift]async-callback-argret" (func 8))
    (export "[callback][async-lift]async-callback-argret" (func 9))
    (export "sync-argret" (func 10))
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 11))
    (export "x#sync" (func 12))
    (export "x#[dtor]r" (func 13))
    (func (;0;) (type 0)
      unreachable
    )
    (func (;1;) (type 0)
      unreachable
    )
    (func (;2;) (type 0)
      unreachable
    )
    (func (;3;) (type 1) (result i32)
      unreachable
    )
    (func (;4;) (type 2) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func (;5;) (type 0)
      unreachable
    )
    (func (;6;) (type 0))
    (func (;7;) (type 3) (param i32 i32)
      unreachable
    )
    (func (;8;) (type 4) (param i32 i32) (result i32)
      unreachable
    )
    (func (;9;) (type 2) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func (;10;) (type 4) (param i32 i32) (result i32)
      unreachable
    )
    (func (;11;) (type 5) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (func (;12;) (type 0)
      unreachable
    )
    (func (;13;) (type 6) (param i32)
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
    (type (;3;) (func (param i32 i32)))
    (type (;4;) (func (param i32 i32) (result i32)))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "main" "_initialize" (func $_initialize (;1;) (type 1)))
    (import "main" "__wasm_init_task" (func $__wasm_init_task (;2;) (type 1)))
    (import "main" "__wasm_init_async_task" (func $__wasm_init_async_task (;3;) (type 1)))
    (import "main" "[async-lift-stackful]async-stackful" (func $"[async-lift-stackful]async-stackful" (;4;) (type 1)))
    (import "main" "[async-lift]async-callback" (func $"[async-lift]async-callback" (;5;) (type 2)))
    (import "main" "sync" (func $sync (;6;) (type 1)))
    (import "main" "[async-lift-stackful]async-stackful-argret" (func $"[async-lift-stackful]async-stackful-argret" (;7;) (type 3)))
    (import "main" "[async-lift]async-callback-argret" (func $"[async-lift]async-callback-argret" (;8;) (type 4)))
    (import "main" "sync-argret" (func $sync-argret (;9;) (type 4)))
    (import "main" "x#sync" (func $x#sync (;10;) (type 1)))
    (import "shim" "$imports" (table (;0;) 1 1 funcref))
    (export "hook0" (func $"hook-[async-lift-stackful]async-stackful"))
    (export "hook1" (func $"hook-[async-lift]async-callback"))
    (export "hook2" (func $hook-sync))
    (export "hook3" (func $"hook-[async-lift-stackful]async-stackful-argret"))
    (export "hook4" (func $"hook-[async-lift]async-callback-argret"))
    (export "hook5" (func $hook-sync-argret))
    (export "hook6" (func $hook-x#sync))
    (start $start)
    (elem (;0;) (i32.const 0) func 18)
    (func $"hook-[async-lift-stackful]async-stackful" (;11;) (type 1)
      call $__wasm_init_async_task
      call $"[async-lift-stackful]async-stackful"
    )
    (func $"hook-[async-lift]async-callback" (;12;) (type 2) (result i32)
      call $__wasm_init_async_task
      call $"[async-lift]async-callback"
    )
    (func $hook-sync (;13;) (type 1)
      call $__wasm_init_task
      call $sync
    )
    (func $"hook-[async-lift-stackful]async-stackful-argret" (;14;) (type 3) (param i32 i32)
      call $__wasm_init_async_task
      local.get 0
      local.get 1
      call $"[async-lift-stackful]async-stackful-argret"
    )
    (func $"hook-[async-lift]async-callback-argret" (;15;) (type 4) (param i32 i32) (result i32)
      call $__wasm_init_async_task
      local.get 0
      local.get 1
      call $"[async-lift]async-callback-argret"
    )
    (func $hook-sync-argret (;16;) (type 4) (param i32 i32) (result i32)
      call $__wasm_init_task
      local.get 0
      local.get 1
      call $sync-argret
    )
    (func $hook-x#sync (;17;) (type 1)
      call $__wasm_init_task
      call $x#sync
    )
    (func (;18;) (type 0) (param i32)
      call $__wasm_init_task
      local.get 0
      call $0
    )
    (func $start (;19;) (type 1)
      call $__wasm_init_task
      call $_initialize
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
  (type (;1;) (func async))
  (alias core export $main "cabi_realloc" (core func $cabi_realloc (;9;)))
  (func $async-stackful (;0;) (type 1) (canon lift (core func $hook0) async))
  (export $"#func1 async-stackful" (@name "async-stackful") (;1;) "async-stackful" (func $async-stackful))
  (alias core export $main "[callback][async-lift]async-callback" (core func $"[callback][async-lift]async-callback" (;10;)))
  (func $async-callback (;2;) (type 1) (canon lift (core func $hook1) async (callback $"[callback][async-lift]async-callback")))
  (export $"#func3 async-callback" (@name "async-callback") (;3;) "async-callback" (func $async-callback))
  (type (;2;) (func))
  (func $sync (;4;) (type 2) (canon lift (core func $hook2)))
  (export $"#func5 sync" (@name "sync") (;5;) "sync" (func $sync))
  (type (;3;) (func async (param "s" string) (result string)))
  (func $async-stackful-argret (;6;) (type 3) (canon lift (core func $hook3) (memory $memory) (realloc $cabi_realloc) string-encoding=utf8 async))
  (export $"#func7 async-stackful-argret" (@name "async-stackful-argret") (;7;) "async-stackful-argret" (func $async-stackful-argret))
  (alias core export $main "[callback][async-lift]async-callback-argret" (core func $"[callback][async-lift]async-callback-argret" (;11;)))
  (func $async-callback-argret (;8;) (type 3) (canon lift (core func $hook4) (memory $memory) (realloc $cabi_realloc) string-encoding=utf8 async (callback $"[callback][async-lift]async-callback-argret")))
  (export $"#func9 async-callback-argret" (@name "async-callback-argret") (;9;) "async-callback-argret" (func $async-callback-argret))
  (type (;4;) (func (param "s" string) (result string)))
  (func $sync-argret (;10;) (type 4) (canon lift (core func $hook5) (memory $memory) (realloc $cabi_realloc) string-encoding=utf8))
  (export $"#func11 sync-argret" (@name "sync-argret") (;11;) "sync-argret" (func $sync-argret))
  (func $"#func12 sync" (@name "sync") (;12;) (type 2) (canon lift (core func $hook6)))
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
