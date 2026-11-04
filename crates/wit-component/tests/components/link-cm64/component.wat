(component
  (type $ty-test:test/test (;0;)
    (instance
      (type (;0;) (func (param "v" s32) (result s32)))
      (export (;0;) "bar" (func (type 0)))
    )
  )
  (import "test:test/test" (instance $test:test/test (;0;) (type $ty-test:test/test)))
  (core module $main (;0;)
    (type (;0;) (func (param i64 i64 i64 i64) (result i64)))
    (type (;1;) (func (param i32) (result i32)))
    (import "c" "cabi_realloc" (func (;0;) (type 0) (param i64 i64 i64 i64) (result i64)))
    (table (;0;) i64 4 funcref)
    (memory (;0;) i64 17)
    (global (;0;) (mut i64) i64.const 1048576)
    (global (;1;) i64 i64.const 1048720)
    (global (;2;) i64 i64.const 3)
    (global (;3;) i64 i64.const 1048720)
    (global (;4;) i64 i64.const 3)
    (global (;5;) (mut i64) i64.const 0)
    (global (;6;) i64 i64.const 1048752)
    (global (;7;) i64 i64.const 3)
    (global (;8;) i64 i64.const 1048752)
    (global (;9;) i64 i64.const 3)
    (global (;10;) (mut i64) i64.const 0)
    (global (;11;) (mut i64) i64.const 3)
    (global (;12;) (mut i64) i64.const 1048768)
    (global (;13;) (mut i64) i64.const 1114112)
    (export "cabi_realloc" (func 0))
    (export "__stack_pointer" (global 0))
    (export "dl:memory_base" (global 1))
    (export "dl:table_base" (global 2))
    (export "bar:memory_base" (global 3))
    (export "bar:table_base" (global 4))
    (export "bar:well" (global 5))
    (export "c:memory_base" (global 6))
    (export "c:table_base" (global 7))
    (export "foo:memory_base" (global 8))
    (export "foo:table_base" (global 9))
    (export "foo:um" (global 10))
    (export "foo:baz" (global 11))
    (export "__heap_base" (global 12))
    (export "__heap_end" (global 13))
    (export "baz" (func 1))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (func (;1;) (type 1) (param i32) (result i32)
      local.get 0
      i64.const 3
      call_indirect (type 1)
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module $dl (;1;)
    (@dylink.0
      (mem-info (memory 0 4))
      (target-arch "wasm64")
    )
    (type (;0;) (func (param i32) (result i32)))
    (global $what (;0;) i64 i64.const 42)
    (export "test:test/dl#f" (func $f))
    (export "g" (func $f))
    (export "what" (global $what))
    (func $f (;0;) (type 0) (param i32) (result i32)
      unreachable
    )
  )
  (core module $c (;2;)
    (@dylink.0
      (mem-info (memory 0 4))
    )
    (type (;0;) (func))
    (type (;1;) (func (param i64) (result i64)))
    (type (;2;) (func (param i64 i64 i64 i64) (result i64)))
    (import "GOT.mem" "__heap_base" (global $__heap_base (;0;) (mut i64)))
    (import "GOT.mem" "__heap_end" (global $__heap_end (;1;) (mut i64)))
    (global $heap (;2;) (mut i64) i64.const 0)
    (export "malloc" (func $malloc))
    (export "abort" (func $abort))
    (export "cabi_realloc" (func $cabi_realloc))
    (start $start)
    (func $start (;0;) (type 0)
      global.get $__heap_base
      global.set $heap
    )
    (func $malloc (;1;) (type 1) (param i64) (result i64)
      global.get $heap
      global.get $heap
      local.get 0
      i64.add
      global.set $heap
    )
    (func $cabi_realloc (;2;) (type 2) (param i64 i64 i64 i64) (result i64)
      local.get 3
      call $malloc
    )
    (func $abort (;3;) (type 0)
      unreachable
    )
  )
  (core module $foo (;3;)
    (@dylink.0
      (mem-info (memory 4 4))
      (needed "c")
    )
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (type (;2;) (func (param i64) (result i64)))
    (import "env" "memory" (memory (;0;) i64 1))
    (import "env" "__indirect_function_table" (table (;0;) i64 0 funcref))
    (import "env" "__stack_pointer" (global $__stack_pointer (;0;) (mut i64)))
    (import "env" "__memory_base" (global $__memory_base (;1;) i64))
    (import "env" "__table_base" (global $__table_base (;2;) i64))
    (import "env" "malloc" (func $malloc (;0;) (type 2) (param i64) (result i64)))
    (import "env" "abort" (func $abort (;1;) (type 0)))
    (import "GOT.mem" "um" (global $um (;3;) (mut i64)))
    (import "GOT.func" "baz" (global $baz (;4;) (mut i64)))
    (import "test:test/test" "bar" (func $bar (;2;) (type 1) (param i32) (result i32)))
    (global (;5;) i64 i64.const 0)
    (export "__wasm_call_ctors" (func $__wasm_call_ctors))
    (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
    (export "foo" (func $foo))
    (export "well" (global 5))
    (func $__wasm_call_ctors (;3;) (type 0))
    (func $__wasm_apply_data_relocs (;4;) (type 0)
      i64.const 8
      global.get $__memory_base
      i64.add
      global.get $baz
      i64.store
    )
    (func $foo (;5;) (type 1) (param i32) (result i32)
      global.get $__stack_pointer
      i64.const 16
      i64.sub
      global.set $__stack_pointer
      i64.const 4
      call $malloc
      i64.eqz
      if ;; label = @1
        call $abort
        unreachable
      end
      local.get 0
      global.get $um
      i32.load offset=16
      i32.add
      i32.const 42
      i32.add
      global.get $baz
      call_indirect (type 1)
      call $bar
      global.get $__stack_pointer
      i64.const 16
      i64.add
      global.set $__stack_pointer
    )
    (data $.data (;0;) (global.get $__memory_base) "\04\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
  )
  (core module $bar (;4;)
    (@dylink.0
      (mem-info (memory 20 4))
      (needed "foo")
    )
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func))
    (import "env" "memory" (memory (;0;) i64 1))
    (import "env" "__indirect_function_table" (table (;0;) i64 0 funcref))
    (import "env" "__memory_base" (global $__memory_base (;0;) i64))
    (import "env" "__table_base" (global $__table_base (;1;) i64))
    (import "env" "foo" (func $foo (;0;) (type 0) (param i32) (result i32)))
    (import "GOT.mem" "well" (global $well (;2;) (mut i64)))
    (global (;3;) i64 i64.const 0)
    (export "__wasm_call_ctors" (func $__wasm_call_ctors))
    (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
    (export "test:test/test#bar" (func $bar))
    (export "baz" (func $baz))
    (export "um" (global 3))
    (func $__wasm_call_ctors (;1;) (type 1))
    (func $__wasm_apply_data_relocs (;2;) (type 1))
    (func $bar (;3;) (type 0) (param i32) (result i32)
      local.get 0
      call $foo
      global.get $well
      i32.load
      i32.add
    )
    (func $baz (;4;) (type 0) (param i32) (result i32)
      local.get 0
    )
    (data $.data (;0;) (global.get $__memory_base) "\01\00\00\00\02\00\00\00\03\00\00\00\04\00\00\00\05\00\00\00")
  )
  (core module $wit-component-shim-module (;5;)
    (type (;0;) (func (param i64 i64 i64 i64) (result i64)))
    (table (;0;) 1 1 funcref)
    (export "0" (func $adapt-c-cabi_realloc))
    (export "$imports" (table 0))
    (func $adapt-c-cabi_realloc (;0;) (type 0) (param i64 i64 i64 i64) (result i64)
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
  (alias core export $wit-component-shim-instance "0" (core func $adapt-c-cabi_realloc (;0;)))
  (core instance $c (;1;)
    (export "cabi_realloc" (func $adapt-c-cabi_realloc))
  )
  (core instance $main (;2;) (instantiate $main
      (with "c" (instance $c))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core instance $dl (;3;) (instantiate $dl))
  (alias core export $main "__heap_base" (core global $__heap_base (;0;)))
  (alias core export $main "__heap_end" (core global $__heap_end (;1;)))
  (core instance $GOT.mem (;4;)
    (export "__heap_base" (global $__heap_base))
    (export "__heap_end" (global $__heap_end))
  )
  (core instance $"#core-instance5 c" (@name "c") (;5;) (instantiate $c
      (with "GOT.mem" (instance $GOT.mem))
    )
  )
  (alias core export $main "__indirect_function_table" (core table $__indirect_function_table (;0;)))
  (alias core export $main "__stack_pointer" (core global $__stack_pointer (;2;)))
  (alias core export $main "foo:memory_base" (core global $foo:memory_base (;3;)))
  (alias core export $main "foo:table_base" (core global $foo:table_base (;4;)))
  (alias core export $"#core-instance5 c" "malloc" (core func $malloc (;1;)))
  (alias core export $"#core-instance5 c" "abort" (core func $abort (;2;)))
  (core instance $env (;6;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__stack_pointer" (global $__stack_pointer))
    (export "__memory_base" (global $foo:memory_base))
    (export "__table_base" (global $foo:table_base))
    (export "malloc" (func $malloc))
    (export "abort" (func $abort))
  )
  (alias core export $main "foo:um" (core global $foo:um (;5;)))
  (core instance $"#core-instance7 GOT.mem" (@name "GOT.mem") (;7;)
    (export "um" (global $foo:um))
  )
  (alias core export $main "foo:baz" (core global $foo:baz (;6;)))
  (core instance $GOT.func (;8;)
    (export "baz" (global $foo:baz))
  )
  (alias export $test:test/test "bar" (func $bar (;0;)))
  (core func $bar (;3;) (canon lower (func $bar)))
  (core instance $test:test/test (;9;)
    (export "bar" (func $bar))
  )
  (core instance $foo (;10;) (instantiate $foo
      (with "env" (instance $env))
      (with "GOT.mem" (instance $"#core-instance7 GOT.mem"))
      (with "GOT.func" (instance $GOT.func))
      (with "test:test/test" (instance $test:test/test))
    )
  )
  (alias core export $main "bar:memory_base" (core global $bar:memory_base (;7;)))
  (alias core export $main "bar:table_base" (core global $bar:table_base (;8;)))
  (alias core export $foo "foo" (core func $foo (;4;)))
  (core instance $"#core-instance11 env" (@name "env") (;11;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__memory_base" (global $bar:memory_base))
    (export "__table_base" (global $bar:table_base))
    (export "foo" (func $foo))
  )
  (alias core export $main "bar:well" (core global $bar:well (;9;)))
  (core instance $"#core-instance12 GOT.mem" (@name "GOT.mem") (;12;)
    (export "well" (global $bar:well))
  )
  (core instance $bar (;13;) (instantiate $bar
      (with "env" (instance $"#core-instance11 env"))
      (with "GOT.mem" (instance $"#core-instance12 GOT.mem"))
    )
  )
  (core module $wit-component-fixup (;6;)
    (type (;0;) (func (param i64 i64 i64 i64) (result i64)))
    (type (;1;) (func))
    (type (;2;) (func (param i32) (result i32)))
    (import "actual" "0" (func $0 (;0;) (type 0) (param i64 i64 i64 i64) (result i64)))
    (import "main" "memory" (memory (;0;) i64 0))
    (import "main" "__indirect_function_table" (table (;0;) i64 0 funcref))
    (import "main" "dl:memory_base" (global $dl:memory_base (;0;) i64))
    (import "dl" "what" (global $what (;1;) i64))
    (import "main" "foo:memory_base" (global $foo:memory_base (;2;) i64))
    (import "foo" "well" (global $well (;3;) i64))
    (import "main" "bar:well" (global $bar:well (;4;) (mut i64)))
    (import "main" "bar:memory_base" (global $bar:memory_base (;5;) i64))
    (import "bar" "um" (global $um (;6;) i64))
    (import "main" "foo:um" (global $foo:um (;7;) (mut i64)))
    (import "bar" "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs (;1;) (type 1)))
    (import "foo" "__wasm_apply_data_relocs" (func $"#func2 __wasm_apply_data_relocs" (@name "__wasm_apply_data_relocs") (;2;) (type 1)))
    (import "bar" "__wasm_call_ctors" (func $__wasm_call_ctors (;3;) (type 1)))
    (import "foo" "__wasm_call_ctors" (func $"#func4 __wasm_call_ctors" (@name "__wasm_call_ctors") (;4;) (type 1)))
    (import "dl" "g" (func $g (;5;) (type 2) (param i32) (result i32)))
    (import "dl" "test:test/dl#f" (func $test:test/dl#f (;6;) (type 2) (param i32) (result i32)))
    (import "bar" "baz" (func $baz (;7;) (type 2) (param i32) (result i32)))
    (import "shim" "$imports" (table (;1;) 1 1 funcref))
    (start $start)
    (elem (;0;) (i64.const 1) func $g $test:test/dl#f)
    (elem (;1;) (i64.const 3) func $baz)
    (elem (;2;) (table 1) (i32.const 0) func $0)
    (func $start (;8;) (type 1)
      i64.const 1048664
      global.get $dl:memory_base
      global.get $what
      i64.add
      i64.store
      global.get $foo:memory_base
      global.get $well
      i64.add
      global.set $bar:well
      global.get $bar:memory_base
      global.get $um
      i64.add
      global.set $foo:um
      call $__wasm_apply_data_relocs
      call $"#func2 __wasm_apply_data_relocs"
      call $__wasm_call_ctors
      call $"#func4 __wasm_call_ctors"
    )
    (data (;0;) (i64.const 1048576) "dlgtest:test/dl#fwhat\00\00\00\01\00\00\00\00\00\00\00\02\00\10\00\00\00\00\00\01\00\00\00\00\00\00\00\0e\00\00\00\00\00\00\00\03\00\10\00\00\00\00\00\02\00\00\00\00\00\00\00\04\00\00\00\00\00\00\00\11\00\10\00\00\00\00\00\00\00\00\00\00\00\00\00\02\00\00\00\00\00\00\00\00\00\10\00\00\00\00\00\03\00\00\00\00\00\00\00\18\00\10\00\00\00\00\00\01\00\00\00\00\00\00\00`\00\10\00\00\00\00\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (alias core export $"#core-instance5 c" "cabi_realloc" (core func $cabi_realloc (;5;)))
  (core instance $actual (;14;)
    (export "0" (func $cabi_realloc))
  )
  (core instance $fixup (;15;) (instantiate $wit-component-fixup
      (with "actual" (instance $actual))
      (with "main" (instance $main))
      (with "dl" (instance $dl))
      (with "foo" (instance $foo))
      (with "bar" (instance $bar))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (type (;1;) (func (param "v" s32) (result s32)))
  (alias core export $dl "test:test/dl#f" (core func $test:test/dl#f (;6;)))
  (func $f (;1;) (type 1) (canon lift (core func $test:test/dl#f)))
  (component $test:test/dl-shim-component (;0;)
    (type (;0;) (func (param "v" s32) (result s32)))
    (import "import-func-f" (func (;0;) (type 0)))
    (type (;1;) (func (param "v" s32) (result s32)))
    (export (;1;) "f" (func 0) (func (type 1)))
  )
  (instance $test:test/dl-shim-instance (;1;) (instantiate $test:test/dl-shim-component
      (with "import-func-f" (func $f))
    )
  )
  (export $test:test/dl (;2;) "test:test/dl" (instance $test:test/dl-shim-instance))
  (alias core export $bar "test:test/test#bar" (core func $test:test/test#bar (;7;)))
  (func $"#func2 bar" (@name "bar") (;2;) (type 1) (canon lift (core func $test:test/test#bar)))
  (component $test:test/test-shim-component (;1;)
    (type (;0;) (func (param "v" s32) (result s32)))
    (import "import-func-bar" (func (;0;) (type 0)))
    (type (;1;) (func (param "v" s32) (result s32)))
    (export (;1;) "bar" (func 0) (func (type 1)))
  )
  (instance $test:test/test-shim-instance (;3;) (instantiate $test:test/test-shim-component
      (with "import-func-bar" (func $"#func2 bar"))
    )
  )
  (export $"#instance4 test:test/test" (@name "test:test/test") (;4;) "test:test/test" (instance $test:test/test-shim-instance))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
