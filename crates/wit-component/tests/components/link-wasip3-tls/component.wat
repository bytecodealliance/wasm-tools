(component
  (core module $main (;0;)
    (type (;0;) (func (result i32)))
    (type (;1;) (func (result i32)))
    (type (;2;) (func (param i32)))
    (type (;3;) (func (result i32)))
    (type (;4;) (func (param i32)))
    (import "$root" "[context-get-1]" (func (;0;) (type 0)))
    (table (;0;) 1 funcref)
    (memory (;0;) 17)
    (global (;0;) (mut i32) i32.const 1048600)
    (global (;1;) i32 i32.const 1048624)
    (global (;2;) i32 i32.const 1)
    (global (;3;) i32 i32.const 1048640)
    (global (;4;) i32 i32.const 1)
    (global (;5;) i32 i32.const 1048656)
    (global (;6;) i32 i32.const 1)
    (export "__wasm_program_tls_info" (global 0))
    (export "c:memory_base" (global 1))
    (export "c:table_base" (global 2))
    (export "foo:memory_base" (global 3))
    (export "foo:table_base" (global 4))
    (export "none:memory_base" (global 5))
    (export "none:table_base" (global 6))
    (export "c:__wasm_get_tls_base" (func 1))
    (export "c:__wasm_set_tls_base" (func 2))
    (export "foo:__wasm_get_tls_base" (func 3))
    (export "foo:__wasm_set_tls_base" (func 4))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (func (;1;) (type 1) (result i32)
      call 0
      i32.load
    )
    (func (;2;) (type 2) (param i32)
      (local i32)
      call 0
      local.tee 1
      i32.eqz
      if ;; label = @1
        i32.const 1048584
        local.get 0
        i32.store
      else
        local.get 1
        local.get 0
        i32.store
      end
    )
    (func (;3;) (type 3) (result i32)
      call 0
      i32.load offset=4
    )
    (func (;4;) (type 4) (param i32)
      (local i32)
      call 0
      local.tee 1
      i32.eqz
      if ;; label = @1
        i32.const 1048584
        local.get 0
        i32.store offset=4
      else
        local.get 1
        local.get 0
        i32.store offset=4
      end
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module $c (;1;)
    (@dylink.0
      (mem-info (memory 16 4))
    )
    (type $void (;0;) (func))
    (type $get (;1;) (func (result i32)))
    (type $set (;2;) (func (param i32)))
    (type $spawn (;3;) (func (param i32 i32) (result i32)))
    (type (;4;) (func (param i32) (result i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "env" "__memory_base" (global $__memory_base (;0;) i32))
    (import "env" "__table_base" (global $__table_base (;1;) i32))
    (import "env" "__wasm_get_stack_pointer" (func $get_sp (;0;) (type $get)))
    (import "env" "__wasm_set_stack_pointer" (func $set_sp (;1;) (type $set)))
    (import "env" "__wasm_get_tls_base" (func $get_tls (;2;) (type $get)))
    (import "env" "__wasm_set_tls_base" (func $set_tls (;3;) (type $set)))
    (import "$root" "[context-get-1]" (func $get_ctx (;4;) (type $get)))
    (import "$root" "[context-set-1]" (func $set_ctx (;5;) (type $set)))
    (import "GOT.mem" "__wasm_program_tls_info" (global $tls_info (;2;) (mut i32)))
    (import "$root" "[thread-new-indirect-v0]" (func $spawn (;6;) (type $spawn)))
    (global $__tls_size (;3;) i32 i32.const 200)
    (global $__tls_align (;4;) i32 i32.const 8)
    (global (;5;) i32 i32.const 8)
    (export "__wasm_library_tls_info" (global 5))
    (export "__wasm_apply_data_relocs" (func 7))
    (export "abort" (func 10))
    (elem (;0;) (global.get $__table_base) func $get_size_and_align $__wasm_init_tls)
    (func (;7;) (type $void)
      global.get $__memory_base
      i32.const 0
      i32.add
      global.get $__table_base
      i32.const 0
      i32.add
      i32.store
      global.get $__memory_base
      i32.const 4
      i32.add
      global.get $__table_base
      i32.const 1
      i32.add
      i32.store
    )
    (func $__wasm_init_tls (;8;) (type $set) (param i32))
    (func $get_size_and_align (;9;) (type 4) (param i32) (result i32)
      local.get 0
      global.get $__tls_align
      i32.store
      global.get $__tls_size
    )
    (func (;10;) (type $void)
      unreachable
    )
  )
  (core module $none (;2;)
    (@dylink.0
      (mem-info (memory 4 4))
      (needed "c")
    )
    (type (;0;) (func (result i32)))
    (export "none_helper" (func 0))
    (func (;0;) (type 0) (result i32)
      i32.const 7
    )
  )
  (core module $foo (;3;)
    (@dylink.0
      (mem-info (memory 8 4))
      (needed "c")
      (needed "none")
    )
    (type $void (;0;) (func))
    (type $get (;1;) (func (result i32)))
    (type $set (;2;) (func (param i32)))
    (type (;3;) (func (param i32) (result i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "env" "__memory_base" (global $__memory_base (;0;) i32))
    (import "env" "__table_base" (global $__table_base (;1;) i32))
    (import "env" "__wasm_get_tls_base" (func $get_tls (;0;) (type $get)))
    (import "env" "__wasm_set_tls_base" (func $set_tls (;1;) (type $set)))
    (import "env" "abort" (func $abort (;2;) (type $void)))
    (import "env" "none_helper" (func $none_helper (;3;) (type $get)))
    (global $__tls_size (;2;) i32 i32.const 100)
    (global $__tls_align (;3;) i32 i32.const 4)
    (global (;4;) i32 i32.const 8)
    (export "__wasm_library_tls_info" (global 4))
    (export "__wasm_apply_data_relocs" (func 4))
    (export "test:test/test#foo" (func 7))
    (elem (;0;) (global.get $__table_base) func $get_size_and_align $__wasm_init_tls)
    (func (;4;) (type $void)
      global.get $__memory_base
      i32.const 0
      i32.add
      global.get $__table_base
      i32.const 0
      i32.add
      i32.store
      global.get $__memory_base
      i32.const 4
      i32.add
      global.get $__table_base
      i32.const 1
      i32.add
      i32.store
    )
    (func $__wasm_init_tls (;5;) (type $set) (param i32))
    (func $get_size_and_align (;6;) (type 3) (param i32) (result i32)
      local.get 0
      global.get $__tls_align
      i32.store
      global.get $__tls_size
    )
    (func (;7;) (type $get) (result i32)
      call $get_tls
      i32.load
      call $none_helper
      i32.add
    )
  )
  (core module $wit-component-shim-module (;4;)
    (type (;0;) (func (param i32 i32) (result i32)))
    (table (;0;) 1 1 funcref)
    (export "0" (func $thread.new-indirect))
    (export "$imports" (table 0))
    (func $thread.new-indirect (;0;) (type 0) (param i32 i32) (result i32)
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
  (core func $"context.get 1" (;0;) (canon context.get i32 1))
  (core instance $$root (;1;)
    (export "[context-get-1]" (func $"context.get 1"))
  )
  (core instance $main (;2;) (instantiate $main
      (with "$root" (instance $$root))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $main "__indirect_function_table" (core table $__indirect_function_table (;0;)))
  (alias core export $main "c:memory_base" (core global $c:memory_base (;0;)))
  (alias core export $main "c:table_base" (core global $c:table_base (;1;)))
  (core func $"context.get 0" (;1;) (canon context.get i32 0))
  (core func $"context.set 0" (;2;) (canon context.set i32 0))
  (alias core export $main "c:__wasm_get_tls_base" (core func $c:__wasm_get_tls_base (;3;)))
  (alias core export $main "c:__wasm_set_tls_base" (core func $c:__wasm_set_tls_base (;4;)))
  (core instance $env (;3;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__memory_base" (global $c:memory_base))
    (export "__table_base" (global $c:table_base))
    (export "__wasm_get_stack_pointer" (func $"context.get 0"))
    (export "__wasm_set_stack_pointer" (func $"context.set 0"))
    (export "__wasm_get_tls_base" (func $c:__wasm_get_tls_base))
    (export "__wasm_set_tls_base" (func $c:__wasm_set_tls_base))
  )
  (core func $"#core-func5 context.get 1" (@name "context.get 1") (;5;) (canon context.get i32 1))
  (core func $"context.set 1" (;6;) (canon context.set i32 1))
  (alias core export $wit-component-shim-instance "0" (core func $thread.new-indirect (;7;)))
  (core instance $"#core-instance4 $root" (@name "$root") (;4;)
    (export "[context-get-1]" (func $"#core-func5 context.get 1"))
    (export "[context-set-1]" (func $"context.set 1"))
    (export "[thread-new-indirect-v0]" (func $thread.new-indirect))
  )
  (alias core export $main "__wasm_program_tls_info" (core global $__wasm_program_tls_info (;2;)))
  (core instance $GOT.mem (;5;)
    (export "__wasm_program_tls_info" (global $__wasm_program_tls_info))
  )
  (core instance $c (;6;) (instantiate $c
      (with "env" (instance $env))
      (with "$root" (instance $"#core-instance4 $root"))
      (with "GOT.mem" (instance $GOT.mem))
    )
  )
  (core instance $none (;7;) (instantiate $none))
  (alias core export $main "foo:memory_base" (core global $foo:memory_base (;3;)))
  (alias core export $main "foo:table_base" (core global $foo:table_base (;4;)))
  (alias core export $main "foo:__wasm_get_tls_base" (core func $foo:__wasm_get_tls_base (;8;)))
  (alias core export $main "foo:__wasm_set_tls_base" (core func $foo:__wasm_set_tls_base (;9;)))
  (alias core export $c "abort" (core func $abort (;10;)))
  (alias core export $none "none_helper" (core func $none_helper (;11;)))
  (core instance $"#core-instance8 env" (@name "env") (;8;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__memory_base" (global $foo:memory_base))
    (export "__table_base" (global $foo:table_base))
    (export "__wasm_get_tls_base" (func $foo:__wasm_get_tls_base))
    (export "__wasm_set_tls_base" (func $foo:__wasm_set_tls_base))
    (export "abort" (func $abort))
    (export "none_helper" (func $none_helper))
  )
  (core instance $foo (;9;) (instantiate $foo
      (with "env" (instance $"#core-instance8 env"))
    )
  )
  (core module $wit-component-fixup (;5;)
    (type (;0;) (func (param i32 i32) (result i32)))
    (type (;1;) (func))
    (import "actual" "0" (func $0 (;0;) (type 0)))
    (import "main" "memory" (memory (;0;) 0))
    (import "main" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "c" "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs (;1;) (type 1)))
    (import "foo" "__wasm_apply_data_relocs" (func $"#func2 __wasm_apply_data_relocs" (@name "__wasm_apply_data_relocs") (;2;) (type 1)))
    (import "main" "c:memory_base" (global $c:memory_base (;0;) i32))
    (import "c" "__wasm_library_tls_info" (global $__wasm_library_tls_info (;1;) i32))
    (import "main" "foo:memory_base" (global $foo:memory_base (;2;) i32))
    (import "foo" "__wasm_library_tls_info" (global $"#global3 __wasm_library_tls_info" (@name "__wasm_library_tls_info") (;3;) i32))
    (import "shim" "$imports" (table (;1;) 1 1 funcref))
    (start $start)
    (elem (;0;) (i32.const 1) func)
    (elem (;1;) (i32.const 1) func)
    (elem (;2;) (table 1) (i32.const 0) func $0)
    (func $start (;3;) (type 1)
      call $__wasm_apply_data_relocs
      call $"#func2 __wasm_apply_data_relocs"
      i32.const 1048592
      global.get $c:memory_base
      global.get $__wasm_library_tls_info
      i32.add
      i32.store
      i32.const 1048596
      global.get $foo:memory_base
      global.get $"#global3 __wasm_library_tls_info"
      i32.add
      i32.store
    )
    (data (;0;) (i32.const 1048576) "\00\00\00\00\00\00\10\00")
    (data (;1;) (i32.const 1048600) "\02\00\00\00\10\00\10\00\08\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core type $thread-start (;0;) (func (param i32)))
  (core func $"#core-func12 thread.new-indirect" (@name "thread.new-indirect") (;12;) (canon thread.new-indirect $thread-start $__indirect_function_table))
  (core instance $actual (;10;)
    (export "0" (func $"#core-func12 thread.new-indirect"))
  )
  (core instance $fixup (;11;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "c" (instance $c))
      (with "foo" (instance $foo))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (type (;0;) (func (result s32)))
  (alias core export $foo "test:test/test#foo" (core func $test:test/test#foo (;13;)))
  (func $foo (;0;) (type 0) (canon lift (core func $test:test/test#foo)))
  (component $test:test/test-shim-component (;0;)
    (type (;0;) (func (result s32)))
    (import "import-func-foo" (func (;0;) (type 0)))
    (type (;1;) (func (result s32)))
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
