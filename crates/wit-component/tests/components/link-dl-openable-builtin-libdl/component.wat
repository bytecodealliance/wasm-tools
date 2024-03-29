(component
  (type (;0;)
    (instance
      (type (;0;) (func (param "v" s32) (result s32)))
      (export (;0;) "foo" (func (type 0)))
    )
  )
  (import "test:test/test" (instance (;0;) (type 0)))
  (core module (;0;)
    (table (;0;) 6 funcref)
    (memory (;0;) 17)
    (global (;0;) (mut i32) i32.const 1048576)
    (global (;1;) i32 i32.const 1048688)
    (global (;2;) i32 i32.const 4)
    (global (;3;) i32 i32.const 1048688)
    (global (;4;) i32 i32.const 4)
    (global (;5;) i32 i32.const 1048688)
    (global (;6;) i32 i32.const 4)
    (global (;7;) i32 i32.const 1049020)
    (global (;8;) i32 i32.const 6)
    (global (;9;) (mut i32) i32.const 1049024)
    (global (;10;) (mut i32) i32.const 1114112)
    (export "__stack_pointer" (global 0))
    (export "foo:memory_base" (global 1))
    (export "foo:table_base" (global 2))
    (export "libc.so:memory_base" (global 3))
    (export "libc.so:table_base" (global 4))
    (export "libdl.so:memory_base" (global 5))
    (export "libdl.so:table_base" (global 6))
    (export "wit-component:stubs:memory_base" (global 7))
    (export "wit-component:stubs:table_base" (global 8))
    (export "__heap_base" (global 9))
    (export "__heap_end" (global 10))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;1;)
    (@dylink.0
      (mem-info (memory 0 4))
    )
    (type (;0;) (func))
    (type (;1;) (func (param i32) (result i32)))
    (import "GOT.mem" "__heap_base" (global $__heap_base (;0;) (mut i32)))
    (import "GOT.mem" "__heap_end" (global $__heap_end (;1;) (mut i32)))
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
    (func $abort (;2;) (type 0)
      unreachable
    )
    (global $errno (;2;) i32 i32.const 0)
    (global $heap (;3;) (mut i32) i32.const 0)
    (export "malloc" (func $malloc))
    (export "abort" (func $abort))
    (export "errno" (global $errno))
    (start $start)
  )
  (core module (;2;)
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
  (core module (;3;)
    (@dylink.0
      (mem-info (memory 332 2) (table 2 0))
      (needed "libc.so")
    )
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (param i32 i32 i32) (result i32)))
    (type (;2;) (func))
    (type (;3;) (func (result i32)))
    (type (;4;) (func (param i32 i32) (result i32)))
    (type (;5;) (func (param i32)))
    (type (;6;) (func (param i32 i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 2 funcref))
    (import "env" "__stack_pointer" (global $__stack_pointer (;0;) (mut i32)))
    (import "env" "__memory_base" (global $__memory_base (;1;) i32))
    (import "env" "__table_base" (global $__table_base (;2;) i32))
    (import "env" "strlen" (func $strlen (;0;) (type 0)))
    (import "env" "memcmp" (func $memcmp (;1;) (type 1)))
    (func $__wasm_call_ctors (;2;) (type 2))
    (func $__wasm_apply_data_relocs (;3;) (type 2)
      i32.const 264
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 0
      i32.add
      i32.store
      i32.const 272
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 89
      i32.add
      i32.store
      i32.const 288
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 89
      i32.add
      i32.store
      i32.const 304
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 0
      i32.add
      i32.store
      i32.const 316
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 1
      i32.add
      i32.store
    )
    (func $_initialize (;4;) (type 2)
      block ;; label = @1
        global.get $__memory_base
        i32.const 320
        i32.add
        i32.load
        i32.eqz
        br_if 0 (;@1;)
        unreachable
        unreachable
      end
      global.get $__memory_base
      i32.const 320
      i32.add
      i32.const 1
      i32.store
      call $__wasm_call_ctors
    )
    (func $dlclose (;5;) (type 0) (param i32) (result i32)
      (local i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 1
      global.set $__stack_pointer
      block ;; label = @1
        global.get $__memory_base
        i32.const 328
        i32.add
        i32.load
        local.tee 2
        i32.eqz
        br_if 0 (;@1;)
        local.get 2
        i32.load
        i32.const 1
        i32.add
        local.set 3
        i32.const -16
        local.set 4
        block ;; label = @2
          block ;; label = @3
            loop ;; label = @4
              local.get 3
              i32.const -1
              i32.add
              local.tee 3
              i32.eqz
              br_if 1 (;@3;)
              local.get 2
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
            br 1 (;@2;)
          end
          global.get $__memory_base
          local.tee 3
          i32.const 324
          i32.add
          local.get 3
          i32.const 123
          i32.add
          i32.store
          i32.const -1
          local.set 3
        end
        local.get 1
        i32.const 32
        i32.add
        global.set $__stack_pointer
        local.get 3
        return
      end
      local.get 1
      i32.const 1
      i32.store offset=12
      local.get 1
      i64.const 0
      i64.store offset=20 align=4
      local.get 1
      global.get $__memory_base
      local.tee 3
      i32.const 264
      i32.add
      i32.store offset=8
      local.get 1
      local.get 3
      i32.const 0
      i32.add
      i32.store offset=16
      local.get 1
      i32.const 8
      i32.add
      local.get 3
      i32.const 272
      i32.add
      call $core::panicking::panic_fmt::h2d01e6ec47ea65ad
      unreachable
    )
    (func $dlerror (;6;) (type 3) (result i32)
      (local i32 i32)
      global.get $__memory_base
      i32.const 324
      i32.add
      local.tee 0
      i32.load
      local.set 1
      local.get 0
      i32.const 0
      i32.store
      local.get 1
    )
    (func $dlopen (;7;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        global.get $__memory_base
        i32.const 328
        i32.add
        i32.load
        local.tee 3
        i32.eqz
        br_if 0 (;@1;)
        global.get $__memory_base
        local.set 4
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 1
              i32.const 3
              i32.le_u
              br_if 0 (;@4;)
              local.get 4
              i32.const 164
              i32.add
              local.set 1
              br 1 (;@3;)
            end
            global.get $__memory_base
            local.set 1
            local.get 0
            call $strlen
            local.set 4
            block ;; label = @4
              local.get 3
              i32.load
              local.tee 5
              br_if 0 (;@4;)
              local.get 1
              i32.const 146
              i32.add
              local.set 1
              br 1 (;@3;)
            end
            local.get 3
            i32.load offset=4
            local.set 6
            i32.const 0
            local.set 1
            local.get 5
            local.set 3
            loop ;; label = @4
              local.get 6
              local.get 5
              i32.const 1
              i32.shr_u
              local.get 1
              i32.add
              local.tee 7
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
              local.tee 5
              local.get 4
              local.get 5
              local.get 4
              i32.lt_u
              select
              call $memcmp
              local.tee 9
              local.get 5
              local.get 4
              i32.sub
              local.get 9
              select
              local.tee 5
              i32.eqz
              br_if 2 (;@2;)
              local.get 7
              local.get 3
              local.get 5
              i32.const 0
              i32.gt_s
              select
              local.tee 3
              local.get 7
              i32.const 1
              i32.add
              local.get 1
              local.get 5
              i32.const 0
              i32.lt_s
              select
              local.tee 1
              i32.sub
              local.set 5
              global.get $__memory_base
              local.set 7
              local.get 3
              local.get 1
              i32.gt_u
              br_if 0 (;@4;)
            end
            local.get 7
            i32.const 146
            i32.add
            local.set 1
          end
          global.get $__memory_base
          i32.const 324
          i32.add
          local.get 1
          i32.store
          i32.const 0
          local.set 8
        end
        local.get 2
        i32.const 32
        i32.add
        global.set $__stack_pointer
        local.get 8
        return
      end
      local.get 2
      i32.const 1
      i32.store offset=12
      local.get 2
      i64.const 0
      i64.store offset=20 align=4
      local.get 2
      global.get $__memory_base
      local.tee 1
      i32.const 264
      i32.add
      i32.store offset=8
      local.get 2
      local.get 1
      i32.const 0
      i32.add
      i32.store offset=16
      local.get 2
      i32.const 8
      i32.add
      local.get 1
      i32.const 288
      i32.add
      call $core::panicking::panic_fmt::h2d01e6ec47ea65ad
      unreachable
    )
    (func $dlsym (;8;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i32.const 1
          i32.add
          i32.const 1
          i32.gt_u
          br_if 0 (;@2;)
          global.get $__memory_base
          local.tee 3
          i32.const 324
          i32.add
          local.get 3
          i32.const 212
          i32.add
          i32.store
          i32.const 0
          local.set 3
          br 1 (;@1;)
        end
        block ;; label = @2
          block ;; label = @3
            global.get $__memory_base
            i32.const 328
            i32.add
            i32.load
            local.tee 4
            i32.eqz
            br_if 0 (;@3;)
            local.get 4
            i32.load
            i32.const 1
            i32.add
            local.set 3
            i32.const -16
            local.set 5
            loop ;; label = @4
              local.get 3
              i32.const -1
              i32.add
              local.tee 3
              i32.eqz
              br_if 2 (;@2;)
              local.get 4
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
            local.get 1
            call $strlen
            local.set 5
            block ;; label = @4
              block ;; label = @5
                local.get 0
                i32.load offset=8
                local.tee 4
                i32.eqz
                br_if 0 (;@5;)
                local.get 0
                i32.load offset=12
                local.set 6
                i32.const 0
                local.set 3
                local.get 4
                local.set 7
                loop ;; label = @6
                  local.get 6
                  local.get 4
                  i32.const 1
                  i32.shr_u
                  local.get 3
                  i32.add
                  local.tee 4
                  i32.const 12
                  i32.mul
                  i32.add
                  local.tee 0
                  i32.const 4
                  i32.add
                  i32.load
                  local.get 1
                  local.get 0
                  i32.load
                  local.tee 0
                  local.get 5
                  local.get 0
                  local.get 5
                  i32.lt_u
                  select
                  call $memcmp
                  local.tee 8
                  local.get 0
                  local.get 5
                  i32.sub
                  local.get 8
                  select
                  local.tee 0
                  i32.eqz
                  br_if 2 (;@4;)
                  local.get 4
                  local.get 7
                  local.get 0
                  i32.const 0
                  i32.gt_s
                  select
                  local.tee 7
                  local.get 4
                  i32.const 1
                  i32.add
                  local.get 3
                  local.get 0
                  i32.const 0
                  i32.lt_s
                  select
                  local.tee 3
                  i32.sub
                  local.set 4
                  local.get 7
                  local.get 3
                  i32.gt_u
                  br_if 0 (;@6;)
                end
              end
              global.get $__memory_base
              local.tee 3
              i32.const 324
              i32.add
              local.get 3
              i32.const 195
              i32.add
              i32.store
              i32.const 0
              local.set 3
              br 3 (;@1;)
            end
            local.get 6
            local.get 4
            i32.const 12
            i32.mul
            i32.add
            i32.load offset=8
            local.set 3
            br 2 (;@1;)
          end
          local.get 2
          i32.const 1
          i32.store offset=12
          local.get 2
          i64.const 0
          i64.store offset=20 align=4
          local.get 2
          global.get $__memory_base
          local.tee 3
          i32.const 264
          i32.add
          i32.store offset=8
          local.get 2
          local.get 3
          i32.const 0
          i32.add
          i32.store offset=16
          local.get 2
          i32.const 8
          i32.add
          local.get 3
          i32.const 272
          i32.add
          call $core::panicking::panic_fmt::h2d01e6ec47ea65ad
          unreachable
        end
        global.get $__memory_base
        local.tee 3
        i32.const 324
        i32.add
        local.get 3
        i32.const 123
        i32.add
        i32.store
        i32.const 0
        local.set 3
      end
      local.get 2
      i32.const 32
      i32.add
      global.set $__stack_pointer
      local.get 3
    )
    (func $__wasm_set_libraries (;9;) (type 5) (param i32)
      global.get $__memory_base
      i32.const 328
      i32.add
      local.get 0
      i32.store
    )
    (func $rust_begin_unwind (;10;) (type 5) (param i32)
      unreachable
      unreachable
    )
    (func $core::ptr::drop_in_place$LT$core..panic..panic_info..PanicInfo..internal_constructor..NoPayload$GT$::h302fcc2d49a4d5a6 (;11;) (type 5) (param i32))
    (func $core::panicking::panic_fmt::h2d01e6ec47ea65ad (;12;) (type 6) (param i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      local.get 2
      i32.const 1
      i32.store16 offset=28
      local.get 2
      local.get 1
      i32.store offset=24
      local.get 2
      local.get 0
      i32.store offset=20
      local.get 2
      global.get $__memory_base
      local.tee 1
      i32.const 304
      i32.add
      i32.store offset=16
      local.get 2
      local.get 1
      i32.const 264
      i32.add
      i32.store offset=12
      local.get 2
      i32.const 12
      i32.add
      call $rust_begin_unwind
      unreachable
    )
    (func $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h0b7909c35b053797 (;13;) (type 6) (param i32 i32)
      local.get 0
      i64.const -8254804618240050161
      i64.store offset=8
      local.get 0
      i64.const 3461009080841530644
      i64.store
    )
    (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
    (export "_initialize" (func $_initialize))
    (export "dlclose" (func $dlclose))
    (export "dlerror" (func $dlerror))
    (export "dlopen" (func $dlopen))
    (export "dlsym" (func $dlsym))
    (export "__wasm_set_libraries" (func $__wasm_set_libraries))
    (elem (;0;) (global.get $__table_base) func $core::ptr::drop_in_place$LT$core..panic..panic_info..PanicInfo..internal_constructor..NoPayload$GT$::h302fcc2d49a4d5a6 $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h0b7909c35b053797)
    (data $.data (;0;) (global.get $__memory_base) "`__wasm_set_libraries` should have been called during instantiation with a non-NULL valuecrates/wit-component/dl/src/lib.rsinvalid library handle\00library not found\00dlopen flags not yet supported\00symbol not found\00dlsym RTLD_NEXT and RTLD_DEFAULT not yet supported\00\00\00\00\00\00Y\00\00\00Y\00\00\00\22\00\00\006\00\00\00\09\00\00\00Y\00\00\00\22\00\00\00c\00\00\00\09\00\00\00\00\00\00\00\00\00\00\00\01\00\00\00\01\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
    (@producers
      (processed-by "clang" "17.0.6 (https://github.com/llvm/llvm-project 6009708b4367171ccdbf4b5905cb6a803753fe18)")
      (processed-by "rustc" "1.79.0-nightly (c9f8f3438 2024-03-27)")
    )
    (@custom "target_features" (after data) "\02+\0fmutable-globals+\08sign-ext")
  )
  (core module (;4;)
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
  (core module (;5;)
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
    (elem (;1;) (i32.const 6) func)
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
  (alias core export 0 "libc.so:memory_base" (core global (;3;)))
  (alias core export 0 "libc.so:table_base" (core global (;4;)))
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
  (alias core export 0 "wit-component:stubs:memory_base" (core global (;8;)))
  (alias core export 0 "wit-component:stubs:table_base" (core global (;9;)))
  (core instance (;7;)
    (export "memory" (memory 2))
    (export "__indirect_function_table" (table 1))
    (export "__stack_pointer" (global 7))
    (export "__memory_base" (global 8))
    (export "__table_base" (global 9))
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
  (alias core export 0 "libdl.so:memory_base" (core global (;13;)))
  (alias core export 0 "libdl.so:table_base" (core global (;14;)))
  (alias core export 8 "memcmp" (core func (;0;)))
  (alias core export 8 "strlen" (core func (;1;)))
  (core instance (;11;)
    (export "memory" (memory 3))
    (export "__indirect_function_table" (table 2))
    (export "__stack_pointer" (global 12))
    (export "__memory_base" (global 13))
    (export "__table_base" (global 14))
    (export "memcmp" (func 0))
    (export "strlen" (func 1))
  )
  (core instance (;12;) (instantiate 3
      (with "GOT.mem" (instance 9))
      (with "GOT.func" (instance 10))
      (with "env" (instance 11))
    )
  )
  (alias core export 0 "__heap_base" (core global (;15;)))
  (alias core export 0 "__heap_end" (core global (;16;)))
  (core instance (;13;)
    (export "__heap_base" (global 15))
    (export "__heap_end" (global 16))
  )
  (core instance (;14;))
  (alias core export 0 "memory" (core memory (;4;)))
  (alias core export 0 "__indirect_function_table" (core table (;3;)))
  (alias core export 0 "__stack_pointer" (core global (;17;)))
  (alias core export 0 "foo:memory_base" (core global (;18;)))
  (alias core export 0 "foo:table_base" (core global (;19;)))
  (alias core export 12 "dlopen" (core func (;2;)))
  (core instance (;15;)
    (export "memory" (memory 4))
    (export "__indirect_function_table" (table 3))
    (export "__stack_pointer" (global 17))
    (export "__memory_base" (global 18))
    (export "__table_base" (global 19))
    (export "dlopen" (func 2))
  )
  (alias export 0 "foo" (func (;0;)))
  (core func (;3;) (canon lower (func 0)))
  (core instance (;16;)
    (export "foo" (func 3))
  )
  (core instance (;17;) (instantiate 4
      (with "GOT.mem" (instance 13))
      (with "GOT.func" (instance 14))
      (with "env" (instance 15))
      (with "test:test/test" (instance 16))
    )
  )
  (core instance (;18;) (instantiate 5
      (with "env" (instance 0))
      (with "foo" (instance 17))
      (with "libc.so" (instance 4))
      (with "libdl.so" (instance 12))
      (with "wit-component:stubs" (instance 8))
    )
  )
  (type (;1;) (func (param "v" s32) (result s32)))
  (alias core export 17 "test:test/test#foo" (core func (;4;)))
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
