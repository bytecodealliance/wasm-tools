(component
  (core module $main (;0;)
    (type (;0;) (func (result i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i32)))
    (table (;0;) 3 funcref)
    (memory (;0;) 17)
    (global (;0;) i32 i32.const 1048592)
    (global (;1;) i32 i32.const 1)
    (global (;2;) i32 i32.const 1048608)
    (global (;3;) i32 i32.const 3)
    (export "c:memory_base" (global 0))
    (export "c:table_base" (global 1))
    (export "foo:memory_base" (global 2))
    (export "foo:table_base" (global 3))
    (export "c:__wasm_get_tls_base" (func 0))
    (export "c:__wasm_set_tls_base" (func 1))
    (export "foo:__wasm_get_tls_base" (func 2))
    (export "foo:__wasm_set_tls_base" (func 3))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (func (;0;) (type 0) (result i32)
      i32.const 1048584
      i32.load
    )
    (func (;1;) (type 1) (param i32)
      i32.const 1048584
      local.get 0
      i32.store
    )
    (func (;2;) (type 2) (result i32)
      i32.const 1048584
      i32.load offset=4
    )
    (func (;3;) (type 3) (param i32)
      i32.const 1048584
      local.get 0
      i32.store offset=4
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module $c (;1;)
    (@dylink.0
      (mem-info (memory 8 2) (table 2 0))
    )
    (type (;0;) (func (result i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func))
    (type (;3;) (func (param i32) (result i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 2 funcref))
    (import "env" "__memory_base" (global $__memory_base (;0;) i32))
    (import "env" "__table_base" (global $__table_base (;1;) i32))
    (import "env" "__wasm_get_stack_pointer" (func (;0;) (type 0)))
    (import "env" "__wasm_set_stack_pointer" (func (;1;) (type 1)))
    (import "env" "__wasm_get_tls_base" (func (;2;) (type 0)))
    (import "env" "__wasm_set_tls_base" (func (;3;) (type 1)))
    (global $__tls_size (;2;) i32 i32.const 200)
    (global $__tls_align (;3;) i32 i32.const 8)
    (global (;4;) i32 i32.const 8)
    (export "__wasm_library_tls_info" (global 4))
    (export "__wasm_apply_data_relocs" (func 4))
    (export "abort" (func 7))
    (elem (;0;) (global.get $__table_base) func $get_size_and_align $__wasm_init_tls)
    (func (;4;) (type 2)
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
    (func $__wasm_init_tls (;5;) (type 1) (param i32))
    (func $get_size_and_align (;6;) (type 3) (param i32) (result i32)
      local.get 0
      global.get $__tls_align
      i32.store
      global.get $__tls_size
    )
    (func (;7;) (type 2)
      unreachable
    )
  )
  (core module $foo (;2;)
    (@dylink.0
      (mem-info (memory 8 4))
      (needed "c")
    )
    (type (;0;) (func (result i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func))
    (type (;3;) (func (param i32) (result i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 2 funcref))
    (import "env" "__memory_base" (global $__memory_base (;0;) i32))
    (import "env" "__table_base" (global $__table_base (;1;) i32))
    (import "env" "__wasm_get_stack_pointer" (func (;0;) (type 0)))
    (import "env" "__wasm_set_stack_pointer" (func (;1;) (type 1)))
    (import "env" "__wasm_get_tls_base" (func $get_tls (;2;) (type 0)))
    (import "env" "__wasm_set_tls_base" (func (;3;) (type 1)))
    (import "env" "abort" (func $abort (;4;) (type 2)))
    (global $__tls_size (;2;) i32 i32.const 100)
    (global $__tls_align (;3;) i32 i32.const 4)
    (global (;4;) i32 i32.const 8)
    (export "__wasm_library_tls_info" (global 4))
    (export "__wasm_apply_data_relocs" (func 6))
    (export "test:test/test#foo" (func $foo))
    (elem (;0;) (global.get $__table_base) func $get_size_and_align $__wasm_init_tls)
    (func $foo (;5;) (type 0) (result i32)
      call $get_tls
      i32.load
    )
    (func (;6;) (type 2)
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
    (func $__wasm_init_tls (;7;) (type 1) (param i32))
    (func $get_size_and_align (;8;) (type 3) (param i32) (result i32)
      local.get 0
      global.get $__tls_align
      i32.store
      global.get $__tls_size
    )
  )
  (core module $__init (;3;)
    (type (;0;) (func))
    (type (;1;) (func (param i32)))
    (import "env" "memory" (memory (;0;) 0))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "c" "__wasm_apply_data_relocs" (func (;0;) (type 0)))
    (import "foo" "__wasm_apply_data_relocs" (func (;1;) (type 0)))
    (start 2)
    (elem (;0;) (i32.const 1) func)
    (elem (;1;) (i32.const 3) func)
    (func (;2;) (type 0)
      call 0
      call 1
    )
    (data (;0;) (i32.const 1048576) "\00\00\00\00\00\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $main (;0;) (instantiate $main))
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $main "__indirect_function_table" (core table $__indirect_function_table (;0;)))
  (alias core export $main "c:memory_base" (core global $c:memory_base (;0;)))
  (alias core export $main "c:table_base" (core global $c:table_base (;1;)))
  (core func $"context.get 0" (;0;) (canon context.get i32 0))
  (core func $"context.set 0" (;1;) (canon context.set i32 0))
  (alias core export $main "c:__wasm_get_tls_base" (core func $c:__wasm_get_tls_base (;2;)))
  (alias core export $main "c:__wasm_set_tls_base" (core func $c:__wasm_set_tls_base (;3;)))
  (core instance $env (;1;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__memory_base" (global $c:memory_base))
    (export "__table_base" (global $c:table_base))
    (export "__wasm_get_stack_pointer" (func $"context.get 0"))
    (export "__wasm_set_stack_pointer" (func $"context.set 0"))
    (export "__wasm_get_tls_base" (func $c:__wasm_get_tls_base))
    (export "__wasm_set_tls_base" (func $c:__wasm_set_tls_base))
  )
  (core instance $c (;2;) (instantiate $c
      (with "env" (instance $env))
    )
  )
  (alias core export $main "foo:memory_base" (core global $foo:memory_base (;2;)))
  (alias core export $main "foo:table_base" (core global $foo:table_base (;3;)))
  (core func $"#core-func4 context.get 0" (@name "context.get 0") (;4;) (canon context.get i32 0))
  (core func $"#core-func5 context.set 0" (@name "context.set 0") (;5;) (canon context.set i32 0))
  (alias core export $main "foo:__wasm_get_tls_base" (core func $foo:__wasm_get_tls_base (;6;)))
  (alias core export $main "foo:__wasm_set_tls_base" (core func $foo:__wasm_set_tls_base (;7;)))
  (alias core export $c "abort" (core func $abort (;8;)))
  (core instance $"#core-instance3 env" (@name "env") (;3;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__memory_base" (global $foo:memory_base))
    (export "__table_base" (global $foo:table_base))
    (export "__wasm_get_stack_pointer" (func $"#core-func4 context.get 0"))
    (export "__wasm_set_stack_pointer" (func $"#core-func5 context.set 0"))
    (export "__wasm_get_tls_base" (func $foo:__wasm_get_tls_base))
    (export "__wasm_set_tls_base" (func $foo:__wasm_set_tls_base))
    (export "abort" (func $abort))
  )
  (core instance $foo (;4;) (instantiate $foo
      (with "env" (instance $"#core-instance3 env"))
    )
  )
  (core instance $__init (;5;) (instantiate $__init
      (with "env" (instance $main))
      (with "c" (instance $c))
      (with "foo" (instance $foo))
    )
  )
  (type (;0;) (func (result s32)))
  (alias core export $foo "test:test/test#foo" (core func $test:test/test#foo (;9;)))
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
