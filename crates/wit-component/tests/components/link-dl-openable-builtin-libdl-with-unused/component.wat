(component
  (type $ty-test:test/test (;0;)
    (instance
      (type (;0;) (func (param "v" s32) (result s32)))
      (export (;0;) "foo" (func (type 0)))
    )
  )
  (import "test:test/test" (instance $test:test/test (;0;) (type $ty-test:test/test)))
  (core module $main (;0;)
    (table (;0;) 4 funcref)
    (memory (;0;) 17)
    (global (;0;) (mut i32) i32.const 1048692)
    (global (;1;) i32 i32.const 1048704)
    (global (;2;) i32 i32.const 4)
    (global (;3;) i32 i32.const 1048704)
    (global (;4;) i32 i32.const 4)
    (global (;5;) i32 i32.const 1048704)
    (global (;6;) i32 i32.const 4)
    (global (;7;) (mut i32) i32.const 1048864)
    (global (;8;) (mut i32) i32.const 1114112)
    (export "__wasm_libdl_libraries" (global 0))
    (export "foo:memory_base" (global 1))
    (export "foo:table_base" (global 2))
    (export "libc.so:memory_base" (global 3))
    (export "libc.so:table_base" (global 4))
    (export "libdl.so:memory_base" (global 5))
    (export "libdl.so:table_base" (global 6))
    (export "__heap_base" (global 7))
    (export "__heap_end" (global 8))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module $libc.so (;1;)
    (@dylink.0
      (mem-info (memory 0 4))
    )
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (type (;2;) (func (param i32 i32 i32) (result i32)))
    (import "GOT.mem" "__heap_base" (global $__heap_base (;0;) (mut i32)))
    (import "GOT.mem" "__heap_end" (global $__heap_end (;1;) (mut i32)))
    (global $errno (;2;) i32 i32.const 0)
    (global $heap (;3;) (mut i32) i32.const 0)
    (export "__wasi_init_tp" (func 5))
    (export "malloc" (func $malloc))
    (export "memcmp" (func $memcmp))
    (export "strlen" (func $strlen))
    (export "abort" (func $abort))
    (export "errno" (global $errno))
    (start $start)
    (func $start (;0;) (type 0)
      global.get $__heap_base
      global.set $heap
    )
    (func $malloc (;1;) (type 1) (param i32) (result i32)
      global.get $heap
      global.get $heap
      local.get 0
      i32.add
      global.set $heap
    )
    (func $memcmp (;2;) (type 2) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func $strlen (;3;) (type 1) (param i32) (result i32)
      unreachable
    )
    (func $abort (;4;) (type 0)
      unreachable
    )
    (func (;5;) (type 0))
  )
  (core module $libdl.so (;2;)
    (@dylink.0
      (mem-info (memory 148 2))
      (needed "libc.so")
    )
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (type (;2;) (func (param i32 i32 i32) (result i32)))
    (type (;3;) (func (result i32)))
    (type (;4;) (func (param i32 i32) (result i32)))
    (type (;5;) (func (param i32 i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "env" "__memory_base" (global (;0;) i32))
    (import "env" "__table_base" (global (;1;) i32))
    (import "env" "__wasi_init_tp" (func (;0;) (type 0)))
    (import "env" "strlen" (func (;1;) (type 1)))
    (import "env" "memcmp" (func (;2;) (type 2)))
    (import "GOT.mem" "__wasm_libdl_libraries" (global (;2;) (mut i32)))
    (export "_initialize" (func 5))
    (export "dlclose" (func 6))
    (export "dlerror" (func 7))
    (export "dlopen" (func 8))
    (export "dlsym" (func 10))
    (start 4)
    (func (;3;) (type 0))
    (func (;4;) (type 0)
      i32.const 140
      global.get 0
      i32.add
      i32.const 0
      i32.const 8
      memory.fill
    )
    (func (;5;) (type 0)
      block ;; label = @1
        global.get 0
        i32.const 140
        i32.add
        i32.load
        i32.eqz
        br_if 0 (;@1;)
        unreachable
      end
      global.get 0
      i32.const 140
      i32.add
      i32.const 1
      i32.store
      call 0
      call 3
    )
    (func (;6;) (type 1) (param i32) (result i32)
      (local i32 i32)
      global.get 2
      local.tee 1
      i32.load
      i32.const 1
      i32.add
      local.set 2
      local.get 1
      i32.load offset=4
      i32.const -16
      i32.add
      local.set 1
      block ;; label = @1
        loop ;; label = @2
          local.get 2
          i32.const -1
          i32.add
          local.tee 2
          i32.eqz
          br_if 1 (;@1;)
          local.get 1
          i32.const 16
          i32.add
          local.tee 1
          local.get 0
          i32.ne
          br_if 0 (;@2;)
        end
        i32.const 0
        return
      end
      global.get 0
      local.tee 2
      i32.const 144
      i32.add
      local.get 2
      i32.const 0
      i32.add
      i32.store
      i32.const -1
    )
    (func (;7;) (type 3) (result i32)
      (local i32 i32)
      global.get 0
      i32.const 144
      i32.add
      local.tee 0
      i32.load
      local.set 1
      local.get 0
      i32.const 0
      i32.store
      local.get 1
    )
    (func (;8;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32 i32)
      global.get 0
      local.set 2
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            local.get 1
            i32.const -260
            i32.and
            i32.eqz
            br_if 0 (;@3;)
            local.get 2
            i32.const 58
            i32.add
            local.set 3
            br 1 (;@2;)
          end
          global.get 2
          local.set 2
          local.get 0
          call 1
          local.set 1
          local.get 2
          i32.load
          local.set 4
          global.get 0
          i32.const 23
          i32.add
          local.set 3
          local.get 2
          i32.load offset=4
          local.set 5
          i32.const 0
          local.set 2
          block ;; label = @3
            block ;; label = @4
              local.get 4
              br_table 2 (;@2;) 1 (;@3;) 0 (;@4;)
            end
            local.get 4
            local.set 3
            i32.const 0
            local.set 2
            loop ;; label = @4
              local.get 3
              i32.const 1
              i32.shr_u
              local.tee 6
              local.get 2
              i32.add
              local.set 7
              local.get 2
              local.get 7
              local.get 5
              local.get 7
              i32.const 4
              i32.shl
              i32.add
              local.tee 8
              i32.const 4
              i32.add
              i32.load
              local.get 0
              local.get 8
              i32.load
              local.tee 8
              local.get 1
              local.get 8
              local.get 1
              i32.lt_u
              select
              call 2
              local.tee 9
              local.get 8
              local.get 1
              i32.sub
              local.get 9
              select
              i32.const 0
              i32.gt_s
              select
              local.set 2
              local.get 3
              local.get 6
              i32.sub
              local.tee 3
              i32.const 1
              i32.gt_u
              br_if 0 (;@4;)
            end
          end
          local.get 5
          local.get 2
          i32.const 4
          i32.shl
          i32.add
          local.tee 3
          i32.const 4
          i32.add
          i32.load
          local.get 0
          local.get 3
          i32.load
          local.tee 7
          local.get 1
          local.get 7
          local.get 1
          i32.lt_u
          select
          call 2
          local.set 8
          global.get 0
          local.set 6
          block ;; label = @3
            local.get 8
            local.get 7
            local.get 1
            i32.sub
            local.get 8
            select
            i32.eqz
            br_if 0 (;@3;)
            local.get 6
            i32.const 23
            i32.add
            local.set 3
            br 1 (;@2;)
          end
          local.get 2
          local.get 4
          i32.lt_u
          br_if 1 (;@1;)
          local.get 2
          local.get 4
          call 9
          unreachable
        end
        global.get 0
        i32.const 144
        i32.add
        local.get 3
        i32.store
        i32.const 0
        local.set 3
      end
      local.get 3
    )
    (func (;9;) (type 5) (param i32 i32)
      call 11
      unreachable
    )
    (func (;10;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32 i32)
      block ;; label = @1
        local.get 0
        i32.const -1
        i32.add
        i32.const -2
        i32.lt_u
        br_if 0 (;@1;)
        global.get 0
        local.tee 2
        i32.const 144
        i32.add
        local.get 2
        i32.const 89
        i32.add
        i32.store
        i32.const 0
        return
      end
      global.get 2
      local.tee 3
      i32.load
      i32.const 1
      i32.add
      local.set 2
      local.get 3
      i32.load offset=4
      i32.const -16
      i32.add
      local.set 3
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                loop ;; label = @6
                  local.get 2
                  i32.const -1
                  i32.add
                  local.tee 2
                  i32.eqz
                  br_if 1 (;@5;)
                  local.get 3
                  i32.const 16
                  i32.add
                  local.tee 3
                  local.get 0
                  i32.ne
                  br_if 0 (;@6;)
                end
                local.get 1
                call 1
                local.set 2
                local.get 0
                i32.load offset=12
                local.set 4
                i32.const 0
                local.set 3
                local.get 0
                i32.load offset=8
                local.tee 5
                br_table 3 (;@2;) 2 (;@3;) 1 (;@4;)
              end
              global.get 0
              local.tee 2
              i32.const 144
              i32.add
              local.get 2
              i32.const 0
              i32.add
              i32.store
              i32.const 0
              return
            end
            local.get 5
            local.set 0
            i32.const 0
            local.set 3
            loop ;; label = @4
              local.get 0
              i32.const 1
              i32.shr_u
              local.tee 6
              local.get 3
              i32.add
              local.set 7
              local.get 3
              local.get 7
              local.get 4
              local.get 7
              i32.const 12
              i32.mul
              i32.add
              local.tee 8
              i32.const 4
              i32.add
              i32.load
              local.get 1
              local.get 8
              i32.load
              local.tee 8
              local.get 2
              local.get 8
              local.get 2
              i32.lt_u
              select
              call 2
              local.tee 9
              local.get 8
              local.get 2
              i32.sub
              local.get 9
              select
              i32.const 0
              i32.gt_s
              select
              local.set 3
              local.get 0
              local.get 6
              i32.sub
              local.tee 0
              i32.const 1
              i32.gt_u
              br_if 0 (;@4;)
            end
          end
          local.get 4
          local.get 3
          i32.const 12
          i32.mul
          i32.add
          local.tee 7
          i32.const 4
          i32.add
          i32.load
          local.get 1
          local.get 7
          i32.load
          local.tee 0
          local.get 2
          local.get 0
          local.get 2
          i32.lt_u
          select
          call 2
          local.tee 8
          local.get 0
          local.get 2
          i32.sub
          local.get 8
          select
          i32.eqz
          br_if 1 (;@1;)
        end
        global.get 0
        local.tee 2
        i32.const 144
        i32.add
        local.get 2
        i32.const 41
        i32.add
        i32.store
        i32.const 0
        return
      end
      block ;; label = @1
        local.get 3
        local.get 5
        i32.ge_u
        br_if 0 (;@1;)
        local.get 7
        i32.load offset=8
        return
      end
      local.get 3
      local.get 5
      call 9
      unreachable
    )
    (func (;11;) (type 0)
      unreachable
    )
    (data (;0;) (global.get 0) "invalid library handle\00library not found\00symbol not found\00dlopen flags not yet supported\00dlsym RTLD_NEXT and RTLD_DEFAULT not yet supported\00")
  )
  (core module $foo (;3;)
    (@dylink.0
      (mem-info (memory 0 4))
    )
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (param i32 i32) (result i32)))
    (import "test:test/test" "foo" (func $import_foo (;0;) (type 0)))
    (import "env" "dlopen" (func $dlopen (;1;) (type 1)))
    (global $what (;0;) i32 i32.const 42)
    (global $um (;1;) i32 i32.const 0)
    (export "test:test/test#foo" (func $foo))
    (export "bar" (func $foo))
    (export "baz" (func $foo))
    (export "what" (global $what))
    (export "um" (global $um))
    (func $foo (;2;) (type 0) (param i32) (result i32)
      unreachable
    )
  )
  (core instance $main (;0;) (instantiate $main))
  (alias core export $main "memory" (core memory $memory (;0;)))
  (alias core export $main "__heap_base" (core global $__heap_base (;0;)))
  (alias core export $main "__heap_end" (core global $__heap_end (;1;)))
  (core instance $GOT.mem (;1;)
    (export "__heap_base" (global $__heap_base))
    (export "__heap_end" (global $__heap_end))
  )
  (core instance $libc.so (;2;) (instantiate $libc.so
      (with "GOT.mem" (instance $GOT.mem))
    )
  )
  (alias core export $main "__indirect_function_table" (core table $__indirect_function_table (;0;)))
  (alias core export $main "libdl.so:memory_base" (core global $libdl.so:memory_base (;2;)))
  (alias core export $main "libdl.so:table_base" (core global $libdl.so:table_base (;3;)))
  (alias core export $libc.so "__wasi_init_tp" (core func $__wasi_init_tp (;0;)))
  (alias core export $libc.so "strlen" (core func $strlen (;1;)))
  (alias core export $libc.so "memcmp" (core func $memcmp (;2;)))
  (core instance $env (;3;)
    (export "memory" (memory $memory))
    (export "__indirect_function_table" (table $__indirect_function_table))
    (export "__memory_base" (global $libdl.so:memory_base))
    (export "__table_base" (global $libdl.so:table_base))
    (export "__wasi_init_tp" (func $__wasi_init_tp))
    (export "strlen" (func $strlen))
    (export "memcmp" (func $memcmp))
  )
  (alias core export $main "__wasm_libdl_libraries" (core global $__wasm_libdl_libraries (;4;)))
  (core instance $"#core-instance4 GOT.mem" (@name "GOT.mem") (;4;)
    (export "__wasm_libdl_libraries" (global $__wasm_libdl_libraries))
  )
  (core instance $libdl.so (;5;) (instantiate $libdl.so
      (with "env" (instance $env))
      (with "GOT.mem" (instance $"#core-instance4 GOT.mem"))
    )
  )
  (alias export $test:test/test "foo" (func $foo (;0;)))
  (core func $foo (;3;) (canon lower (func $foo)))
  (core instance $test:test/test (;6;)
    (export "foo" (func $foo))
  )
  (alias core export $libdl.so "dlopen" (core func $dlopen (;4;)))
  (core instance $"#core-instance7 env" (@name "env") (;7;)
    (export "dlopen" (func $dlopen))
  )
  (core instance $foo (;8;) (instantiate $foo
      (with "test:test/test" (instance $test:test/test))
      (with "env" (instance $"#core-instance7 env"))
    )
  )
  (core module $wit-component-fixup (;4;)
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (import "main" "memory" (memory (;0;) 0))
    (import "main" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "main" "foo:memory_base" (global $foo:memory_base (;0;) i32))
    (import "foo" "um" (global $um (;1;) i32))
    (import "foo" "what" (global $what (;2;) i32))
    (import "libdl.so" "_initialize" (func $_initialize (;0;) (type 0)))
    (import "foo" "bar" (func $bar (;1;) (type 1)))
    (import "foo" "baz" (func $baz (;2;) (type 1)))
    (import "foo" "test:test/test#foo" (func $test:test/test#foo (;3;) (type 1)))
    (start $start)
    (elem (;0;) (i32.const 1) func $bar $baz $test:test/test#foo)
    (elem (;1;) (i32.const 4) func)
    (func $start (;4;) (type 0)
      i32.const 1048660
      global.get $foo:memory_base
      global.get $um
      i32.add
      i32.store
      i32.const 1048672
      global.get $foo:memory_base
      global.get $what
      i32.add
      i32.store
      call $_initialize
    )
    (data (;0;) (i32.const 1048576) "foo\00bar\00baz\00test:test/test#foo\00\00um\00\00what\03\00\00\00\04\00\10\00\01\00\00\00\03\00\00\00\08\00\10\00\02\00\00\00\12\00\00\00\0c\00\10\00\03\00\00\00\02\00\00\00 \00\10\00\00\00\00\00\04\00\00\00$\00\10\00\00\00\00\00\03\00\00\00\00\00\10\00\05\00\00\00(\00\10\00\01\00\00\00d\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $fixup (;9;) (instantiate $wit-component-fixup
      (with "main" (instance $main))
      (with "foo" (instance $foo))
      (with "libdl.so" (instance $libdl.so))
    )
  )
  (type (;1;) (func (param "v" s32) (result s32)))
  (alias core export $foo "test:test/test#foo" (core func $test:test/test#foo (;5;)))
  (func $"#func1 foo" (@name "foo") (;1;) (type 1) (canon lift (core func $test:test/test#foo)))
  (component $test:test/test-shim-component (;0;)
    (type (;0;) (func (param "v" s32) (result s32)))
    (import "import-func-foo" (func (;0;) (type 0)))
    (type (;1;) (func (param "v" s32) (result s32)))
    (export (;1;) "foo" (func 0) (func (type 1)))
  )
  (instance $test:test/test-shim-instance (;1;) (instantiate $test:test/test-shim-component
      (with "import-func-foo" (func $"#func1 foo"))
    )
  )
  (export $"#instance2 test:test/test" (@name "test:test/test") (;2;) "test:test/test" (instance $test:test/test-shim-instance))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
