(component
  (type (;0;)
    (instance
      (type (;0;) (func (param "v" s32) (result s32)))
      (export (;0;) "foo" (func (type 0)))
    )
  )
  (import "test:test/test" (instance (;0;) (type 0)))
  (core module (;0;)
    (table (;0;) 4 funcref)
    (memory (;0;) 17)
    (global (;0;) (mut i32) i32.const 1048576)
    (global (;1;) i32 i32.const 1048688)
    (global (;2;) i32 i32.const 4)
    (global (;3;) i32 i32.const 1048688)
    (global (;4;) i32 i32.const 4)
    (global (;5;) i32 i32.const 1048688)
    (global (;6;) i32 i32.const 4)
    (global (;7;) (mut i32) i32.const 1048688)
    (global (;8;) (mut i32) i32.const 1114112)
    (export "__stack_pointer" (global 0))
    (export "foo:memory_base" (global 1))
    (export "foo:table_base" (global 2))
    (export "libdl.so:memory_base" (global 3))
    (export "libdl.so:table_base" (global 4))
    (export "wit-component:stubs:memory_base" (global 5))
    (export "wit-component:stubs:table_base" (global 6))
    (export "__heap_base" (global 7))
    (export "__heap_end" (global 8))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;1;)
    (type (;0;) (func (param i32 i32 i32) (result i32)))
    (type (;1;) (func (param i32) (result i32)))
    (func (;0;) (type 0) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func (;1;) (type 1) (param i32) (result i32)
      unreachable
    )
    (export "memcmp" (func 0))
    (export "strlen" (func 1))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;2;)
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (param i32 i32 i32) (result i32)))
    (type (;2;) (func))
    (type (;3;) (func (result i32)))
    (type (;4;) (func (param i32 i32) (result i32)))
    (type (;5;) (func (param i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "env" "__memory_base" (global (;0;) i32))
    (import "env" "__table_base" (global (;1;) i32))
    (import "env" "strlen" (func (;0;) (type 0)))
    (import "env" "memcmp" (func (;1;) (type 1)))
    (func (;2;) (type 2))
    (func (;3;) (type 2))
    (func (;4;) (type 2)
      block ;; label = @1
        global.get 0
        i32.const 140
        i32.add
        i32.load
        i32.eqz
        br_if 0 (;@1;)
        unreachable
        unreachable
      end
      global.get 0
      i32.const 140
      i32.add
      i32.const 1
      i32.store
      call 2
    )
    (func (;5;) (type 0) (param i32) (result i32)
      (local i32 i32 i32 i32 i32)
      block ;; label = @1
        global.get 0
        i32.const 148
        i32.add
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@1;)
        local.get 1
        i32.load
        local.set 2
        i32.const 0
        local.set 3
        i32.const -16
        local.set 4
        block ;; label = @2
          block ;; label = @3
            loop ;; label = @4
              local.get 2
              local.get 3
              local.tee 5
              i32.eq
              br_if 1 (;@3;)
              local.get 5
              i32.const 1
              i32.add
              local.set 3
              local.get 1
              i32.load offset=4
              local.get 4
              i32.const 16
              i32.add
              local.tee 4
              i32.add
              local.get 0
              i32.ne
              br_if 0 (;@4;)
            end
            i32.const 0
            local.set 3
            local.get 5
            local.get 2
            i32.lt_u
            br_if 1 (;@2;)
          end
          global.get 0
          local.tee 3
          i32.const 144
          i32.add
          local.get 3
          i32.const 0
          i32.add
          i32.store
          i32.const -1
          local.set 3
        end
        local.get 3
        return
      end
      call 6
      unreachable
    )
    (func (;6;) (type 2)
      unreachable
      unreachable
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
      (local i32 i32 i32 i32 i32 i32 i32)
      block ;; label = @1
        global.get 0
        i32.const 148
        i32.add
        i32.load
        local.tee 2
        i32.eqz
        br_if 0 (;@1;)
        global.get 0
        local.set 3
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 1
              i32.const -260
              i32.and
              i32.eqz
              br_if 0 (;@4;)
              local.get 3
              i32.const 41
              i32.add
              local.set 1
              br 1 (;@3;)
            end
            global.get 0
            local.set 1
            local.get 0
            call 0
            local.set 3
            block ;; label = @4
              local.get 2
              i32.load
              local.tee 4
              br_if 0 (;@4;)
              local.get 1
              i32.const 23
              i32.add
              local.set 1
              br 1 (;@3;)
            end
            local.get 2
            i32.load offset=4
            local.set 5
            i32.const 0
            local.set 1
            local.get 4
            local.set 2
            loop ;; label = @4
              local.get 5
              local.get 4
              i32.const 1
              i32.shr_u
              local.get 1
              i32.add
              local.tee 6
              i32.const 4
              i32.shl
              i32.add
              local.tee 7
              i32.const 4
              i32.add
              i32.load
              local.get 0
              local.get 7
              i32.load
              local.tee 4
              local.get 3
              local.get 4
              local.get 3
              i32.lt_u
              select
              call 1
              local.tee 8
              local.get 4
              local.get 3
              i32.sub
              local.get 8
              select
              local.tee 4
              i32.eqz
              br_if 2 (;@2;)
              local.get 6
              local.get 2
              local.get 4
              i32.const 0
              i32.gt_s
              select
              local.tee 2
              local.get 6
              i32.const 1
              i32.add
              local.get 1
              local.get 4
              i32.const 0
              i32.lt_s
              select
              local.tee 1
              i32.sub
              local.set 4
              global.get 0
              local.set 6
              local.get 2
              local.get 1
              i32.gt_u
              br_if 0 (;@4;)
            end
            local.get 6
            i32.const 23
            i32.add
            local.set 1
          end
          global.get 0
          i32.const 144
          i32.add
          local.get 1
          i32.store
          i32.const 0
          local.set 7
        end
        local.get 7
        return
      end
      call 6
      unreachable
    )
    (func (;9;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32)
      block ;; label = @1
        local.get 0
        i32.const 1
        i32.add
        i32.const 1
        i32.gt_u
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
      block ;; label = @1
        global.get 0
        i32.const 148
        i32.add
        i32.load
        local.tee 3
        i32.eqz
        br_if 0 (;@1;)
        local.get 3
        i32.load
        local.set 4
        i32.const 0
        local.set 2
        i32.const -16
        local.set 5
        block ;; label = @2
          block ;; label = @3
            loop ;; label = @4
              local.get 4
              local.get 2
              local.tee 6
              i32.eq
              br_if 1 (;@3;)
              local.get 6
              i32.const 1
              i32.add
              local.set 2
              local.get 3
              i32.load offset=4
              local.get 5
              i32.const 16
              i32.add
              local.tee 5
              i32.add
              local.get 0
              i32.ne
              br_if 0 (;@4;)
            end
            local.get 6
            local.get 4
            i32.lt_u
            br_if 1 (;@2;)
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
        local.get 1
        call 0
        local.set 5
        block ;; label = @2
          block ;; label = @3
            local.get 0
            i32.load offset=8
            local.tee 6
            i32.eqz
            br_if 0 (;@3;)
            local.get 0
            i32.const 12
            i32.add
            i32.load
            local.set 7
            i32.const 0
            local.set 2
            local.get 6
            local.set 3
            loop ;; label = @4
              local.get 7
              local.get 6
              i32.const 1
              i32.shr_u
              local.get 2
              i32.add
              local.tee 0
              i32.const 12
              i32.mul
              i32.add
              local.tee 6
              i32.const 4
              i32.add
              i32.load
              local.get 1
              local.get 6
              i32.load
              local.tee 6
              local.get 5
              local.get 6
              local.get 5
              i32.lt_u
              select
              call 1
              local.tee 4
              local.get 6
              local.get 5
              i32.sub
              local.get 4
              select
              local.tee 6
              i32.eqz
              br_if 2 (;@2;)
              local.get 0
              local.get 3
              local.get 6
              i32.const 0
              i32.gt_s
              select
              local.tee 3
              local.get 0
              i32.const 1
              i32.add
              local.get 2
              local.get 6
              i32.const 0
              i32.lt_s
              select
              local.tee 2
              i32.sub
              local.set 6
              local.get 3
              local.get 2
              i32.gt_u
              br_if 0 (;@4;)
            end
          end
          global.get 0
          local.tee 2
          i32.const 144
          i32.add
          local.get 2
          i32.const 72
          i32.add
          i32.store
          i32.const 0
          return
        end
        local.get 7
        local.get 0
        i32.const 12
        i32.mul
        i32.add
        i32.load offset=8
        return
      end
      call 6
      unreachable
    )
    (func (;10;) (type 5) (param i32)
      global.get 0
      i32.const 148
      i32.add
      local.get 0
      i32.store
    )
    (export "__wasm_apply_data_relocs" (func 3))
    (export "_initialize" (func 4))
    (export "dlclose" (func 5))
    (export "dlerror" (func 7))
    (export "dlopen" (func 8))
    (export "dlsym" (func 9))
    (export "__wasm_set_libraries" (func 10))
    (data (;0;) (global.get 0) "invalid library handle\00library not found\00dlopen flags not yet supported\00symbol not found\00dlsym RTLD_NEXT and RTLD_DEFAULT not yet supported\00\00\00\00\00\00\00\00\00\00\00\00\00")
  )
  (core module (;3;)
    (@dylink.0
      (mem-info (memory 0 4))
    )
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (param i32 i32) (result i32)))
    (import "test:test/test" "foo" (func $import_foo (;0;) (type 0)))
    (import "env" "dlopen" (func $dlopen (;1;) (type 1)))
    (func $foo (;2;) (type 0) (param i32) (result i32)
      unreachable
    )
    (global $what (;0;) i32 i32.const 42)
    (export "test:test/test#foo" (func $foo))
    (export "bar" (func $foo))
    (export "baz" (func $foo))
    (export "what" (global $what))
  )
  (core module (;4;)
    (type (;0;) (func))
    (type (;1;) (func (param i32)))
    (type (;2;) (func (param i32) (result i32)))
    (type (;3;) (func (param i32) (result i32)))
    (type (;4;) (func (param i32) (result i32)))
    (import "env" "memory" (memory (;0;) 0))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (import "env" "foo:memory_base" (global (;0;) i32))
    (import "foo" "what" (global (;1;) i32))
    (import "libdl.so" "__wasm_apply_data_relocs" (func (;0;) (type 0)))
    (import "libdl.so" "_initialize" (func (;1;) (type 0)))
    (import "libdl.so" "__wasm_set_libraries" (func (;2;) (type 1)))
    (import "foo" "bar" (func (;3;) (type 2)))
    (import "foo" "baz" (func (;4;) (type 3)))
    (import "foo" "test:test/test#foo" (func (;5;) (type 4)))
    (func (;6;) (type 0)
      i32.const 1048656
      global.get 0
      global.get 1
      i32.add
      i32.store
      call 0
      call 1
      i32.const 1048676
      call 2
    )
    (start 6)
    (elem (;0;) (i32.const 1) func 3 4 5)
    (elem (;1;) (i32.const 4) func)
    (data (;0;) (i32.const 1048576) "foo\00bar\00baz\00test:test/test#foo\00\00what\03\00\00\00\04\00\10\00\01\00\00\00\03\00\00\00\08\00\10\00\02\00\00\00\12\00\00\00\0c\00\10\00\03\00\00\00\04\00\00\00 \00\10\00\00\00\00\00\03\00\00\00\00\00\10\00\04\00\00\00$\00\10\00\01\00\00\00T\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance (;0;) (instantiate 0))
  (alias core export 0 "memory" (core memory (;0;)))
  (alias core export 0 "__heap_base" (core global (;0;)))
  (alias core export 0 "__heap_end" (core global (;1;)))
  (core instance (;1;)
    (export "__heap_base" (global 0))
    (export "__heap_end" (global 1))
  )
  (core instance (;2;))
  (alias core export 0 "memory" (core memory (;1;)))
  (alias core export 0 "__indirect_function_table" (core table (;0;)))
  (alias core export 0 "__stack_pointer" (core global (;2;)))
  (alias core export 0 "wit-component:stubs:memory_base" (core global (;3;)))
  (alias core export 0 "wit-component:stubs:table_base" (core global (;4;)))
  (core instance (;3;)
    (export "memory" (memory 1))
    (export "__indirect_function_table" (table 0))
    (export "__stack_pointer" (global 2))
    (export "__memory_base" (global 3))
    (export "__table_base" (global 4))
  )
  (core instance (;4;) (instantiate 1
      (with "GOT.mem" (instance 1))
      (with "GOT.func" (instance 2))
      (with "env" (instance 3))
    )
  )
  (alias core export 0 "__heap_base" (core global (;5;)))
  (alias core export 0 "__heap_end" (core global (;6;)))
  (core instance (;5;)
    (export "__heap_base" (global 5))
    (export "__heap_end" (global 6))
  )
  (core instance (;6;))
  (alias core export 0 "memory" (core memory (;2;)))
  (alias core export 0 "__indirect_function_table" (core table (;1;)))
  (alias core export 0 "__stack_pointer" (core global (;7;)))
  (alias core export 0 "libdl.so:memory_base" (core global (;8;)))
  (alias core export 0 "libdl.so:table_base" (core global (;9;)))
  (alias core export 4 "memcmp" (core func (;0;)))
  (alias core export 4 "strlen" (core func (;1;)))
  (core instance (;7;)
    (export "memory" (memory 2))
    (export "__indirect_function_table" (table 1))
    (export "__stack_pointer" (global 7))
    (export "__memory_base" (global 8))
    (export "__table_base" (global 9))
    (export "memcmp" (func 0))
    (export "strlen" (func 1))
  )
  (core instance (;8;) (instantiate 2
      (with "GOT.mem" (instance 5))
      (with "GOT.func" (instance 6))
      (with "env" (instance 7))
    )
  )
  (alias core export 0 "__heap_base" (core global (;10;)))
  (alias core export 0 "__heap_end" (core global (;11;)))
  (core instance (;9;)
    (export "__heap_base" (global 10))
    (export "__heap_end" (global 11))
  )
  (core instance (;10;))
  (alias core export 0 "memory" (core memory (;3;)))
  (alias core export 0 "__indirect_function_table" (core table (;2;)))
  (alias core export 0 "__stack_pointer" (core global (;12;)))
  (alias core export 0 "foo:memory_base" (core global (;13;)))
  (alias core export 0 "foo:table_base" (core global (;14;)))
  (alias core export 8 "dlopen" (core func (;2;)))
  (core instance (;11;)
    (export "memory" (memory 3))
    (export "__indirect_function_table" (table 2))
    (export "__stack_pointer" (global 12))
    (export "__memory_base" (global 13))
    (export "__table_base" (global 14))
    (export "dlopen" (func 2))
  )
  (alias export 0 "foo" (func (;0;)))
  (core func (;3;) (canon lower (func 0)))
  (core instance (;12;)
    (export "foo" (func 3))
  )
  (core instance (;13;) (instantiate 3
      (with "GOT.mem" (instance 9))
      (with "GOT.func" (instance 10))
      (with "env" (instance 11))
      (with "test:test/test" (instance 12))
    )
  )
  (core instance (;14;) (instantiate 4
      (with "env" (instance 0))
      (with "foo" (instance 13))
      (with "libdl.so" (instance 8))
      (with "wit-component:stubs" (instance 4))
    )
  )
  (type (;1;) (func (param "v" s32) (result s32)))
  (alias core export 13 "test:test/test#foo" (core func (;4;)))
  (func (;1;) (type 1) (canon lift (core func 4)))
  (component (;0;)
    (type (;0;) (func (param "v" s32) (result s32)))
    (import "import-func-foo" (func (;0;) (type 0)))
    (type (;1;) (func (param "v" s32) (result s32)))
    (export (;1;) "foo" (func 0) (func (type 1)))
  )
  (instance (;1;) (instantiate 0
      (with "import-func-foo" (func 1))
    )
  )
  (export (;2;) "test:test/test" (instance 1))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
