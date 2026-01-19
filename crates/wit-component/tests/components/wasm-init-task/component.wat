(component
  (core module $main (;0;)
    (type (;0;) (func))
    (type (;1;) (func (result i32)))
    (type (;2;) (func (param i32 i32 i32) (result i32)))
    (type (;3;) (func (param i32 i32)))
    (type (;4;) (func (param i32 i32) (result i32)))
    (type (;5;) (func (param i32 i32 i32 i32) (result i32)))
    (memory (;0;) 1)
    (export "__wasm_init_task" (func 0))
    (export "__wasm_init_async_task" (func 1))
    (export "[async-lift-stackful]async-stackful" (func 2))
    (export "[async-lift]async-callback" (func 3))
    (export "[callback][async-lift]async-callback" (func 4))
    (export "sync" (func 5))
    (export "[async-lift-stackful]async-stackful-argret" (func 6))
    (export "[async-lift]async-callback-argret" (func 7))
    (export "[callback][async-lift]async-callback-argret" (func 8))
    (export "sync-argret" (func 9))
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 10))
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
    (func (;6;) (type 3) (param i32 i32)
      unreachable
    )
    (func (;7;) (type 4) (param i32 i32) (result i32)
      unreachable
    )
    (func (;8;) (type 2) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func (;9;) (type 4) (param i32 i32) (result i32)
      unreachable
    )
    (func (;10;) (type 5) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core instance $main (;0;) (instantiate $main))
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core module $init-task-wrappers (;1;)
    (type (;0;) (func))
    (type (;1;) (func))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i32 i32)))
    (type (;4;) (func (param i32 i32) (result i32)))
    (type (;5;) (func (param i32 i32) (result i32)))
    (import "" "__wasm_init_task" (func (;0;) (type 0)))
    (import "" "__wasm_init_async_task" (func (;1;) (type 0)))
    (import "" "import-func-async-stackful" (func (;2;) (type 1)))
    (import "" "import-func-async-callback" (func (;3;) (type 2)))
    (import "" "import-func-sync" (func (;4;) (type 1)))
    (import "" "import-func-async-stackful-argret" (func (;5;) (type 3)))
    (import "" "import-func-async-callback-argret" (func (;6;) (type 4)))
    (import "" "import-func-sync-argret" (func (;7;) (type 5)))
    (export "[async-lift-stackful]async-stackful" (func 8))
    (export "[async-lift]async-callback" (func 9))
    (export "sync" (func 10))
    (export "[async-lift-stackful]async-stackful-argret" (func 11))
    (export "[async-lift]async-callback-argret" (func 12))
    (export "sync-argret" (func 13))
    (func (;8;) (type 1)
      call 1
      call 2
    )
    (func (;9;) (type 2) (result i32)
      call 1
      call 3
    )
    (func (;10;) (type 1)
      call 0
      call 4
    )
    (func (;11;) (type 3) (param i32 i32)
      call 1
      local.get 0
      local.get 1
      call 5
    )
    (func (;12;) (type 4) (param i32 i32) (result i32)
      call 1
      local.get 0
      local.get 1
      call 6
    )
    (func (;13;) (type 5) (param i32 i32) (result i32)
      call 0
      local.get 0
      local.get 1
      call 7
    )
  )
  (alias core export $main "__wasm_init_task" (core func $__wasm_init_task (;0;)))
  (alias core export $main "__wasm_init_async_task" (core func $__wasm_init_async_task (;1;)))
  (alias core export $main "[async-lift-stackful]async-stackful" (core func $"[async-lift-stackful]async-stackful" (;2;)))
  (alias core export $main "[async-lift]async-callback" (core func $"[async-lift]async-callback" (;3;)))
  (alias core export $main "sync" (core func $sync (;4;)))
  (alias core export $main "[async-lift-stackful]async-stackful-argret" (core func $"[async-lift-stackful]async-stackful-argret" (;5;)))
  (alias core export $main "[async-lift]async-callback-argret" (core func $"[async-lift]async-callback-argret" (;6;)))
  (alias core export $main "sync-argret" (core func $sync-argret (;7;)))
  (core instance $init-task-wrappers-args (;1;)
    (export "__wasm_init_task" (func $__wasm_init_task))
    (export "__wasm_init_async_task" (func $__wasm_init_async_task))
    (export "import-func-async-stackful" (func $"[async-lift-stackful]async-stackful"))
    (export "import-func-async-callback" (func $"[async-lift]async-callback"))
    (export "import-func-sync" (func $sync))
    (export "import-func-async-stackful-argret" (func $"[async-lift-stackful]async-stackful-argret"))
    (export "import-func-async-callback-argret" (func $"[async-lift]async-callback-argret"))
    (export "import-func-sync-argret" (func $sync-argret))
  )
  (core instance $init-task-wrappers-instance (;2;) (instantiate $init-task-wrappers
      (with "" (instance $init-task-wrappers-args))
    )
  )
  (alias core export $init-task-wrappers-instance "[async-lift-stackful]async-stackful" (core func $"#core-func8 [async-lift-stackful]async-stackful" (@name "[async-lift-stackful]async-stackful") (;8;)))
  (alias core export $init-task-wrappers-instance "[async-lift]async-callback" (core func $"#core-func9 [async-lift]async-callback" (@name "[async-lift]async-callback") (;9;)))
  (alias core export $init-task-wrappers-instance "sync" (core func $"#core-func10 sync" (@name "sync") (;10;)))
  (alias core export $init-task-wrappers-instance "[async-lift-stackful]async-stackful-argret" (core func $"#core-func11 [async-lift-stackful]async-stackful-argret" (@name "[async-lift-stackful]async-stackful-argret") (;11;)))
  (alias core export $init-task-wrappers-instance "[async-lift]async-callback-argret" (core func $"#core-func12 [async-lift]async-callback-argret" (@name "[async-lift]async-callback-argret") (;12;)))
  (alias core export $init-task-wrappers-instance "sync-argret" (core func $"#core-func13 sync-argret" (@name "sync-argret") (;13;)))
  (type (;0;) (func))
  (alias core export $main "cabi_realloc" (core func $cabi_realloc (;14;)))
  (func $async-stackful (;0;) (type 0) (canon lift (core func $"#core-func8 [async-lift-stackful]async-stackful") async))
  (export $"#func1 async-stackful" (@name "async-stackful") (;1;) "async-stackful" (func $async-stackful))
  (alias core export $main "[callback][async-lift]async-callback" (core func $"[callback][async-lift]async-callback" (;15;)))
  (func $async-callback (;2;) (type 0) (canon lift (core func $"#core-func9 [async-lift]async-callback") async (callback $"[callback][async-lift]async-callback")))
  (export $"#func3 async-callback" (@name "async-callback") (;3;) "async-callback" (func $async-callback))
  (func $sync (;4;) (type 0) (canon lift (core func $"#core-func10 sync")))
  (export $"#func5 sync" (@name "sync") (;5;) "sync" (func $sync))
  (type (;1;) (func (param "s" string) (result string)))
  (func $async-stackful-argret (;6;) (type 1) (canon lift (core func $"#core-func11 [async-lift-stackful]async-stackful-argret") (memory $memory) (realloc $cabi_realloc) string-encoding=utf8 async))
  (export $"#func7 async-stackful-argret" (@name "async-stackful-argret") (;7;) "async-stackful-argret" (func $async-stackful-argret))
  (alias core export $main "[callback][async-lift]async-callback-argret" (core func $"[callback][async-lift]async-callback-argret" (;16;)))
  (func $async-callback-argret (;8;) (type 1) (canon lift (core func $"#core-func12 [async-lift]async-callback-argret") (memory $memory) (realloc $cabi_realloc) string-encoding=utf8 async (callback $"[callback][async-lift]async-callback-argret")))
  (export $"#func9 async-callback-argret" (@name "async-callback-argret") (;9;) "async-callback-argret" (func $async-callback-argret))
  (func $sync-argret (;10;) (type 1) (canon lift (core func $"#core-func13 sync-argret") (memory $memory) (realloc $cabi_realloc) string-encoding=utf8))
  (export $"#func11 sync-argret" (@name "sync-argret") (;11;) "sync-argret" (func $sync-argret))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
