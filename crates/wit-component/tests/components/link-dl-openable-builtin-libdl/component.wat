(component
  (type (;0;)
    (instance
      (type (;0;) (func (param "v" s32) (result s32)))
      (export (;0;) "foo" (func (type 0)))
    )
  )
  (import "test:test/test" (instance (;0;) (type 0)))
  (core module (;0;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (import "wasi_snapshot_preview1" "fd_write" (func (;0;) (type 0)))
    (table (;0;) 64 funcref)
    (memory (;0;) 17)
    (global (;0;) (mut i32) i32.const 1048576)
    (global (;1;) i32 i32.const 1048688)
    (global (;2;) i32 i32.const 4)
    (global (;3;) i32 i32.const 1048688)
    (global (;4;) i32 i32.const 4)
    (global (;5;) i32 i32.const 1048688)
    (global (;6;) i32 i32.const 4)
    (global (;7;) (mut i32) i32.const 0)
    (global (;8;) i32 i32.const 1053512)
    (global (;9;) i32 i32.const 64)
    (global (;10;) (mut i32) i32.const 1053520)
    (global (;11;) (mut i32) i32.const 1114112)
    (export "__stack_pointer" (global 0))
    (export "foo:memory_base" (global 1))
    (export "foo:table_base" (global 2))
    (export "libc.so:memory_base" (global 3))
    (export "libc.so:table_base" (global 4))
    (export "libdl.so:memory_base" (global 5))
    (export "libdl.so:table_base" (global 6))
    (export "libdl.so:errno" (global 7))
    (export "wit-component:stubs:memory_base" (global 8))
    (export "wit-component:stubs:table_base" (global 9))
    (export "__heap_base" (global 10))
    (export "__heap_end" (global 11))
    (export "wasi_snapshot_preview1:fd_write" (func 0))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;1;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (func $fd_write (;0;) (type 0) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (export "fd_write" (func $fd_write))
  )
  (core module (;2;)
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
  (core module (;3;)
    (type (;0;) (func (param i32 i32) (result i32)))
    (type (;1;) (func (param i32)))
    (type (;2;) (func (param i32 i32) (result i32)))
    (type (;3;) (func (param i32) (result i32)))
    (type (;4;) (func (param i32 i32 i32) (result i32)))
    (type (;5;) (func (param i32 i32 i32) (result i32)))
    (type (;6;) (func (param i32 i32) (result i32)))
    (type (;7;) (func (param i32) (result i32)))
    (func (;0;) (type 0) (param i32 i32) (result i32)
      unreachable
    )
    (func (;1;) (type 1) (param i32)
      unreachable
    )
    (func (;2;) (type 2) (param i32 i32) (result i32)
      unreachable
    )
    (func (;3;) (type 3) (param i32) (result i32)
      unreachable
    )
    (func (;4;) (type 4) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func (;5;) (type 5) (param i32 i32 i32) (result i32)
      unreachable
    )
    (func (;6;) (type 6) (param i32 i32) (result i32)
      unreachable
    )
    (func (;7;) (type 7) (param i32) (result i32)
      unreachable
    )
    (export "aligned_alloc" (func 0))
    (export "free" (func 1))
    (export "getcwd" (func 2))
    (export "getenv" (func 3))
    (export "memcmp" (func 4))
    (export "memcpy" (func 5))
    (export "realloc" (func 6))
    (export "strlen" (func 7))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;4;)
    (@dylink.0
      (mem-info (memory 4824 3) (table 59 0))
      (needed "libc.so")
    )
    (type (;0;) (func (param i32)))
    (type (;1;) (func (param i32 i32 i32)))
    (type (;2;) (func (param i32 i32)))
    (type (;3;) (func (param i32 i32 i32) (result i32)))
    (type (;4;) (func (param i32 i32) (result i32)))
    (type (;5;) (func (param i32) (result i32)))
    (type (;6;) (func))
    (type (;7;) (func (param i32 i32 i32 i32) (result i32)))
    (type (;8;) (func (result i32)))
    (type (;9;) (func (param i32 i32 i32 i32 i32)))
    (type (;10;) (func (param i32 i32 i32 i32)))
    (type (;11;) (func (param i32 i32 i32 i32 i32 i32)))
    (type (;12;) (func (param i32 i32 i32 i32 i32 i32) (result i32)))
    (type (;13;) (func (param i32 i32 i32 i32 i32) (result i32)))
    (type (;14;) (func (param i64 i32 i32) (result i32)))
    (type (;15;) (func (param i32 i32 i32 i32 i32 i32 i32)))
    (import "env" "memory" (memory (;0;) 1))
    (import "env" "__indirect_function_table" (table (;0;) 59 funcref))
    (import "env" "__stack_pointer" (global $__stack_pointer (;0;) (mut i32)))
    (import "env" "__memory_base" (global $__memory_base (;1;) i32))
    (import "env" "__table_base" (global $__table_base (;2;) i32))
    (import "env" "strlen" (func $strlen (;0;) (type 5)))
    (import "env" "memcmp" (func $memcmp (;1;) (type 3)))
    (import "env" "memcpy" (func $memcpy (;2;) (type 3)))
    (import "env" "getenv" (func $getenv (;3;) (type 5)))
    (import "env" "getcwd" (func $getcwd (;4;) (type 4)))
    (import "env" "aligned_alloc" (func $aligned_alloc (;5;) (type 4)))
    (import "env" "malloc" (func $malloc (;6;) (type 5)))
    (import "env" "free" (func $free (;7;) (type 0)))
    (import "env" "realloc" (func $realloc (;8;) (type 4)))
    (import "env" "abort" (func $abort (;9;) (type 6)))
    (import "wasi_snapshot_preview1" "fd_write" (func $wasi::lib_generated::wasi_snapshot_preview1::fd_write::h1fa8da50c0f41235 (;10;) (type 7)))
    (import "GOT.mem" "errno" (global $errno (;3;) (mut i32)))
    (func $__wasm_call_ctors (;11;) (type 6))
    (func $__wasm_apply_data_relocs (;12;) (type 6)
      i32.const 3584
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 0
      i32.add
      i32.store
      i32.const 3592
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 89
      i32.add
      i32.store
      i32.const 3608
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 89
      i32.add
      i32.store
      i32.const 3624
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 239
      i32.add
      i32.store
      i32.const 3644
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3624
      i32.add
      i32.store
      i32.const 3648
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 281
      i32.add
      i32.store
      i32.const 3668
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3648
      i32.add
      i32.store
      i32.const 3672
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 0
      i32.add
      i32.store
      i32.const 3684
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 1
      i32.add
      i32.store
      i32.const 3688
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 2
      i32.add
      i32.store
      i32.const 3692
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 3
      i32.add
      i32.store
      i32.const 3696
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 0
      i32.add
      i32.store
      i32.const 3708
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 4
      i32.add
      i32.store
      i32.const 3712
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 5
      i32.add
      i32.store
      i32.const 3716
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 6
      i32.add
      i32.store
      i32.const 3720
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 7
      i32.add
      i32.store
      i32.const 3732
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 8
      i32.add
      i32.store
      i32.const 3736
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 9
      i32.add
      i32.store
      i32.const 3748
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 10
      i32.add
      i32.store
      i32.const 3752
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 11
      i32.add
      i32.store
      i32.const 3756
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 12
      i32.add
      i32.store
      i32.const 3760
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 324
      i32.add
      i32.store
      i32.const 3768
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 338
      i32.add
      i32.store
      i32.const 3784
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 560
      i32.add
      i32.store
      i32.const 3792
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 449
      i32.add
      i32.store
      i32.const 3808
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 615
      i32.add
      i32.store
      i32.const 3816
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 654
      i32.add
      i32.store
      i32.const 3832
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 654
      i32.add
      i32.store
      i32.const 3848
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 761
      i32.add
      i32.store
      i32.const 3860
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 13
      i32.add
      i32.store
      i32.const 3872
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 1
      i32.add
      i32.store
      i32.const 3876
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 2
      i32.add
      i32.store
      i32.const 3880
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 3
      i32.add
      i32.store
      i32.const 3884
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 789
      i32.add
      i32.store
      i32.const 3896
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 804
      i32.add
      i32.store
      i32.const 3904
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 839
      i32.add
      i32.store
      i32.const 3920
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 14
      i32.add
      i32.store
      i32.const 3932
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 15
      i32.add
      i32.store
      i32.const 3936
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1032
      i32.add
      i32.store
      i32.const 3952
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1199
      i32.add
      i32.store
      i32.const 3968
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1308
      i32.add
      i32.store
      i32.const 3976
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1347
      i32.add
      i32.store
      i32.const 3992
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1347
      i32.add
      i32.store
      i32.const 4008
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1454
      i32.add
      i32.store
      i32.const 4020
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1347
      i32.add
      i32.store
      i32.const 4036
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 16
      i32.add
      i32.store
      i32.const 4048
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 4
      i32.add
      i32.store
      i32.const 4052
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 5
      i32.add
      i32.store
      i32.const 4056
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 6
      i32.add
      i32.store
      i32.const 4060
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1482
      i32.add
      i32.store
      i32.const 4072
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1497
      i32.add
      i32.store
      i32.const 4080
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1532
      i32.add
      i32.store
      i32.const 4096
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1644
      i32.add
      i32.store
      i32.const 4104
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1644
      i32.add
      i32.store
      i32.const 4112
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1661
      i32.add
      i32.store
      i32.const 4120
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1749
      i32.add
      i32.store
      i32.const 4128
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1770
      i32.add
      i32.store
      i32.const 4136
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1749
      i32.add
      i32.store
      i32.const 4144
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1784
      i32.add
      i32.store
      i32.const 4152
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1797
      i32.add
      i32.store
      i32.const 4168
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 20
      i32.add
      i32.store
      i32.const 4180
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 21
      i32.add
      i32.store
      i32.const 4184
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 20
      i32.add
      i32.store
      i32.const 4196
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 22
      i32.add
      i32.store
      i32.const 4200
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 23
      i32.add
      i32.store
      i32.const 4204
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 24
      i32.add
      i32.store
      i32.const 4216
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 25
      i32.add
      i32.store
      i32.const 4220
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 26
      i32.add
      i32.store
      i32.const 4224
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1903
      i32.add
      i32.store
      i32.const 4232
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 1936
      i32.add
      i32.store
      i32.const 4248
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 30
      i32.add
      i32.store
      i32.const 4260
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 31
      i32.add
      i32.store
      i32.const 4264
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 32
      i32.add
      i32.store
      i32.const 4268
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 33
      i32.add
      i32.store
      i32.const 4272
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 34
      i32.add
      i32.store
      i32.const 4276
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 35
      i32.add
      i32.store
      i32.const 4280
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 36
      i32.add
      i32.store
      i32.const 4284
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 37
      i32.add
      i32.store
      i32.const 4288
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 38
      i32.add
      i32.store
      i32.const 4300
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 39
      i32.add
      i32.store
      i32.const 4304
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 40
      i32.add
      i32.store
      i32.const 4308
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 41
      i32.add
      i32.store
      i32.const 4312
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 42
      i32.add
      i32.store
      i32.const 4316
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 43
      i32.add
      i32.store
      i32.const 4320
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 44
      i32.add
      i32.store
      i32.const 4324
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 45
      i32.add
      i32.store
      i32.const 4328
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2203
      i32.add
      i32.store
      i32.const 4336
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2211
      i32.add
      i32.store
      i32.const 4344
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2225
      i32.add
      i32.store
      i32.const 4352
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2227
      i32.add
      i32.store
      i32.const 4360
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2228
      i32.add
      i32.store
      i32.const 4372
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2072
      i32.add
      i32.store
      i32.const 4388
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 30
      i32.add
      i32.store
      i32.const 4400
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 46
      i32.add
      i32.store
      i32.const 4404
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 47
      i32.add
      i32.store
      i32.const 4408
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 12
      i32.add
      i32.store
      i32.const 4412
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 30
      i32.add
      i32.store
      i32.const 4424
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 48
      i32.add
      i32.store
      i32.const 4428
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 49
      i32.add
      i32.store
      i32.const 4440
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 21
      i32.add
      i32.store
      i32.const 4444
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 38
      i32.add
      i32.store
      i32.const 4456
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 50
      i32.add
      i32.store
      i32.const 4460
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2072
      i32.add
      i32.store
      i32.const 4468
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2349
      i32.add
      i32.store
      i32.const 4476
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2398
      i32.add
      i32.store
      i32.const 4484
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2448
      i32.add
      i32.store
      i32.const 4492
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2493
      i32.add
      i32.store
      i32.const 4500
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2227
      i32.add
      i32.store
      i32.const 4508
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2546
      i32.add
      i32.store
      i32.const 4516
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2579
      i32.add
      i32.store
      i32.const 4532
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2715
      i32.add
      i32.store
      i32.const 4540
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2805
      i32.add
      i32.store
      i32.const 4556
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3030
      i32.add
      i32.store
      i32.const 4564
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 2920
      i32.add
      i32.store
      i32.const 4580
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 51
      i32.add
      i32.store
      i32.const 4592
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 52
      i32.add
      i32.store
      i32.const 4596
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3101
      i32.add
      i32.store
      i32.const 4612
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3420
      i32.add
      i32.store
      i32.const 4620
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3420
      i32.add
      i32.store
      i32.const 4628
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 57
      i32.add
      i32.store
      i32.const 4640
      global.get $__memory_base
      i32.add
      global.get $__table_base
      i32.const 58
      i32.add
      i32.store
      i32.const 4644
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3435
      i32.add
      i32.store
      i32.const 4652
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3451
      i32.add
      i32.store
      i32.const 4660
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3474
      i32.add
      i32.store
      i32.const 4668
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3435
      i32.add
      i32.store
      i32.const 4676
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3483
      i32.add
      i32.store
      i32.const 4684
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3499
      i32.add
      i32.store
      i32.const 4692
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3474
      i32.add
      i32.store
      i32.const 4700
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3512
      i32.add
      i32.store
      i32.const 4708
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3512
      i32.add
      i32.store
      i32.const 4716
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3512
      i32.add
      i32.store
      i32.const 4724
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3513
      i32.add
      i32.store
      i32.const 4732
      global.get $__memory_base
      i32.add
      global.get $__memory_base
      i32.const 3531
      i32.add
      i32.store
    )
    (func $__wasm_apply_global_relocs (;13;) (type 6)
      global.get $__memory_base
      i32.const 4757
      i32.add
      global.set $GOT.data.internal.__rust_no_alloc_shim_is_unstable
      global.get $__memory_base
      i32.const 4756
      i32.add
      global.set $GOT.data.internal.__rust_alloc_error_handler_should_panic
      global.get $__table_base
      i32.const 18
      i32.add
      global.set $GOT.func.internal.core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$::fmt::hbfd83f0499f79957
      global.get $__table_base
      i32.const 29
      i32.add
      global.set $GOT.func.internal._$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$::fmt::haaf6d012a733bd8d
      global.get $__table_base
      i32.const 18
      i32.add
      global.set $GOT.func.internal.core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$::fmt::h98edb2ad46049cf7
    )
    (func $_initialize (;14;) (type 6)
      block ;; label = @1
        global.get $__memory_base
        i32.const 4744
        i32.add
        i32.load
        i32.eqz
        br_if 0 (;@1;)
        unreachable
        unreachable
      end
      global.get $__memory_base
      i32.const 4744
      i32.add
      i32.const 1
      i32.store
      call $__wasm_call_ctors
    )
    (func $dlclose (;15;) (type 5) (param i32) (result i32)
      (local i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 1
      global.set $__stack_pointer
      block ;; label = @1
        global.get $__memory_base
        i32.const 4752
        i32.add
        i32.load
        local.tee 2
        i32.eqz
        br_if 0 (;@1;)
        local.get 2
        i32.load
        local.set 3
        i32.const 0
        local.set 4
        i32.const -16
        local.set 5
        block ;; label = @2
          block ;; label = @3
            loop ;; label = @4
              local.get 3
              local.get 4
              i32.eq
              br_if 1 (;@3;)
              local.get 4
              i32.const 1
              i32.add
              local.set 4
              local.get 2
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
            i32.const 0
            local.set 5
            local.get 4
            i32.const -1
            i32.add
            local.get 3
            i32.lt_u
            br_if 1 (;@2;)
          end
          global.get $__memory_base
          local.tee 4
          i32.const 4748
          i32.add
          local.get 4
          i32.const 99
          i32.add
          i32.store
          i32.const -1
          local.set 5
        end
        local.get 1
        i32.const 32
        i32.add
        global.set $__stack_pointer
        local.get 5
        return
      end
      local.get 1
      i32.const 20
      i32.add
      i64.const 0
      i64.store align=4
      local.get 1
      i32.const 1
      i32.store offset=12
      local.get 1
      global.get $__memory_base
      local.tee 4
      i32.const 3584
      i32.add
      i32.store offset=8
      local.get 1
      local.get 4
      i32.const 0
      i32.add
      i32.store offset=16
      local.get 1
      i32.const 8
      i32.add
      local.get 4
      i32.const 3592
      i32.add
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $dlerror (;16;) (type 8) (result i32)
      (local i32 i32)
      global.get $__memory_base
      i32.const 4748
      i32.add
      local.tee 0
      i32.load
      local.set 1
      local.get 0
      i32.const 0
      i32.store
      local.get 1
    )
    (func $dlopen (;17;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        global.get $__memory_base
        i32.const 4752
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
              i32.const 140
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
              i32.const 122
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
            i32.const 122
            i32.add
            local.set 1
          end
          global.get $__memory_base
          i32.const 4748
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
      i32.const 20
      i32.add
      i64.const 0
      i64.store align=4
      local.get 2
      i32.const 1
      i32.store offset=12
      local.get 2
      global.get $__memory_base
      local.tee 1
      i32.const 3584
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
      i32.const 3608
      i32.add
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $dlsym (;18;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            local.get 0
            i32.const 1
            i32.add
            i32.const 1
            i32.gt_u
            br_if 0 (;@3;)
            global.get $__memory_base
            local.tee 3
            i32.const 4748
            i32.add
            local.get 3
            i32.const 188
            i32.add
            i32.store
            i32.const 0
            local.set 3
            br 1 (;@2;)
          end
          global.get $__memory_base
          i32.const 4752
          i32.add
          i32.load
          local.tee 4
          i32.eqz
          br_if 1 (;@1;)
          local.get 4
          i32.load
          local.set 5
          i32.const 0
          local.set 3
          i32.const -16
          local.set 6
          block ;; label = @3
            block ;; label = @4
              loop ;; label = @5
                local.get 5
                local.get 3
                i32.eq
                br_if 1 (;@4;)
                local.get 3
                i32.const 1
                i32.add
                local.set 3
                local.get 4
                i32.load offset=4
                local.get 6
                i32.const 16
                i32.add
                local.tee 6
                i32.add
                local.get 0
                i32.ne
                br_if 0 (;@5;)
              end
              local.get 3
              i32.const -1
              i32.add
              local.get 5
              i32.lt_u
              br_if 1 (;@3;)
            end
            global.get $__memory_base
            local.tee 3
            i32.const 4748
            i32.add
            local.get 3
            i32.const 99
            i32.add
            i32.store
            i32.const 0
            local.set 3
            br 1 (;@2;)
          end
          local.get 1
          call $strlen
          local.set 6
          block ;; label = @3
            block ;; label = @4
              local.get 0
              i32.load offset=8
              local.tee 4
              i32.eqz
              br_if 0 (;@4;)
              local.get 0
              i32.const 12
              i32.add
              i32.load
              local.set 7
              i32.const 0
              local.set 3
              local.get 4
              local.set 5
              loop ;; label = @5
                local.get 7
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
                local.get 6
                local.get 0
                local.get 6
                i32.lt_u
                select
                call $memcmp
                local.tee 8
                local.get 0
                local.get 6
                i32.sub
                local.get 8
                select
                local.tee 0
                i32.eqz
                br_if 2 (;@3;)
                local.get 4
                local.get 5
                local.get 0
                i32.const 0
                i32.gt_s
                select
                local.tee 5
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
                local.get 5
                local.get 3
                i32.gt_u
                br_if 0 (;@5;)
              end
            end
            global.get $__memory_base
            local.tee 3
            i32.const 4748
            i32.add
            local.get 3
            i32.const 171
            i32.add
            i32.store
            i32.const 0
            local.set 3
            br 1 (;@2;)
          end
          local.get 7
          local.get 4
          i32.const 12
          i32.mul
          i32.add
          i32.load offset=8
          local.set 3
        end
        local.get 2
        i32.const 32
        i32.add
        global.set $__stack_pointer
        local.get 3
        return
      end
      local.get 2
      i32.const 20
      i32.add
      i64.const 0
      i64.store align=4
      local.get 2
      i32.const 1
      i32.store offset=12
      local.get 2
      global.get $__memory_base
      local.tee 3
      i32.const 3584
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
      i32.const 3592
      i32.add
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $__wasm_set_libraries (;19;) (type 0) (param i32)
      global.get $__memory_base
      i32.const 4752
      i32.add
      local.get 0
      i32.store
    )
    (func $__rust_alloc (;20;) (type 4) (param i32 i32) (result i32)
      (local i32)
      local.get 0
      local.get 1
      call $__rdl_alloc
      local.set 2
      local.get 2
      return
    )
    (func $__rust_dealloc (;21;) (type 1) (param i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      call $__rdl_dealloc
      return
    )
    (func $__rust_realloc (;22;) (type 7) (param i32 i32 i32 i32) (result i32)
      (local i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call $__rdl_realloc
      local.set 4
      local.get 4
      return
    )
    (func $__rust_alloc_error_handler (;23;) (type 2) (param i32 i32)
      local.get 0
      local.get 1
      call $__rg_oom
      return
    )
    (func $#func24<std::io::stdio::OUTPUT_CAPTURE::__getit::h13738c54a5c306a3__.llvm.9582321433186723839_> (@name "std::io::stdio::OUTPUT_CAPTURE::__getit::h13738c54a5c306a3 (.llvm.9582321433186723839)") (;24;) (type 5) (param i32) (result i32)
      (local i32)
      block ;; label = @1
        global.get $__memory_base
        i32.const 4760
        i32.add
        i32.load
        br_if 0 (;@1;)
        block ;; label = @2
          block ;; label = @3
            local.get 0
            br_if 0 (;@3;)
            i32.const 0
            local.set 1
            br 1 (;@2;)
          end
          local.get 0
          i32.load
          local.set 1
          local.get 0
          i32.const 0
          i32.store
          local.get 0
          i32.load offset=4
          i32.const 0
          local.get 1
          select
          local.set 1
        end
        global.get $__memory_base
        i32.const 4760
        i32.add
        local.tee 0
        local.get 1
        i32.store offset=4
        local.get 0
        i32.const 1
        i32.store
      end
      global.get $__memory_base
      i32.const 4760
      i32.add
      i32.const 4
      i32.add
    )
    (func $std::sys::common::small_c_string::run_with_cstr_allocating::h3821304ded5a7ddb (;25;) (type 1) (param i32 i32 i32)
      (local i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      local.get 3
      local.get 1
      local.get 2
      call $_$LT$$RF$str$u20$as$u20$alloc..ffi..c_str..CString..new..SpecNewImpl$GT$::spec_new_impl::hd80596fd10e06640
      i32.const -2147483648
      local.set 2
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 3
              i32.load
              local.tee 1
              i32.const -2147483648
              i32.ne
              br_if 0 (;@4;)
              local.get 3
              i32.const 8
              i32.add
              i32.load
              local.set 1
              block ;; label = @5
                local.get 3
                i32.load offset=4
                local.tee 4
                call $getenv
                local.tee 5
                i32.eqz
                br_if 0 (;@5;)
                block ;; label = @6
                  block ;; label = @7
                    local.get 5
                    call $strlen
                    local.tee 2
                    br_if 0 (;@7;)
                    i32.const 1
                    local.set 6
                    br 1 (;@6;)
                  end
                  local.get 2
                  i32.const -1
                  i32.le_s
                  br_if 3 (;@3;)
                  global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
                  i32.load8_u
                  drop
                  local.get 2
                  i32.const 1
                  call $__rust_alloc
                  local.tee 6
                  i32.eqz
                  br_if 4 (;@2;)
                end
                local.get 6
                local.get 5
                local.get 2
                call $memcpy
                local.set 5
                local.get 0
                local.get 2
                i32.store offset=8
                local.get 0
                local.get 5
                i32.store offset=4
              end
              local.get 4
              i32.const 0
              i32.store8
              local.get 0
              local.get 2
              i32.store
              local.get 1
              i32.eqz
              br_if 3 (;@1;)
              local.get 4
              local.get 1
              i32.const 1
              call $__rust_dealloc
              br 3 (;@1;)
            end
            local.get 0
            i32.const -2147483647
            i32.store
            local.get 0
            global.get $__memory_base
            i32.const 3640
            i32.add
            i64.load
            i64.store offset=4 align=4
            local.get 1
            i32.eqz
            br_if 2 (;@1;)
            local.get 3
            i32.load offset=4
            local.get 1
            i32.const 1
            call $__rust_dealloc
            br 2 (;@1;)
          end
          call $alloc::raw_vec::capacity_overflow::h9441e1e81ed4591e
          unreachable
        end
        i32.const 1
        local.get 2
        call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
        unreachable
      end
      local.get 3
      i32.const 16
      i32.add
      global.set $__stack_pointer
    )
    (func $alloc::sync::Arc$LT$T$C$A$GT$::drop_slow::ha2345d2e0183cb9b (;26;) (type 0) (param i32)
      (local i32)
      block ;; label = @1
        local.get 0
        i32.load
        local.tee 0
        i32.const 12
        i32.add
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@1;)
        local.get 0
        i32.const 16
        i32.add
        i32.load
        local.get 1
        i32.const 1
        call $__rust_dealloc
      end
      block ;; label = @1
        local.get 0
        i32.const -1
        i32.eq
        br_if 0 (;@1;)
        local.get 0
        local.get 0
        i32.load offset=4
        local.tee 1
        i32.const -1
        i32.add
        i32.store offset=4
        local.get 1
        i32.const 1
        i32.ne
        br_if 0 (;@1;)
        local.get 0
        i32.const 24
        i32.const 4
        call $__rust_dealloc
      end
    )
    (func $alloc::sync::Arc$LT$T$C$A$GT$::drop_slow::hcedfc111557afb49 (;27;) (type 0) (param i32)
      (local i32 i32)
      block ;; label = @1
        local.get 0
        i32.load
        local.tee 0
        i32.const 16
        i32.add
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@1;)
        local.get 0
        i32.const 20
        i32.add
        i32.load
        local.set 2
        local.get 1
        i32.const 0
        i32.store8
        local.get 2
        i32.eqz
        br_if 0 (;@1;)
        local.get 1
        local.get 2
        i32.const 1
        call $__rust_dealloc
      end
      block ;; label = @1
        local.get 0
        i32.const -1
        i32.eq
        br_if 0 (;@1;)
        local.get 0
        local.get 0
        i32.load offset=4
        local.tee 1
        i32.const -1
        i32.add
        i32.store offset=4
        local.get 1
        i32.const 1
        i32.ne
        br_if 0 (;@1;)
        local.get 0
        i32.const 24
        i32.const 8
        call $__rust_dealloc
      end
    )
    (func $std::sys::wasi::os::getcwd::h513006697dbc1ac1 (;28;) (type 0) (param i32)
      (local i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 1
      global.set $__stack_pointer
      global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
      i32.load8_u
      drop
      i32.const 512
      local.set 2
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            i32.const 512
            i32.const 1
            call $__rust_alloc
            local.tee 3
            i32.eqz
            br_if 0 (;@3;)
            local.get 1
            local.get 3
            i32.store offset=8
            local.get 1
            i32.const 512
            i32.store offset=4
            block ;; label = @4
              block ;; label = @5
                local.get 3
                i32.const 512
                call $getcwd
                br_if 0 (;@5;)
                i32.const 512
                local.set 2
                global.get $errno
                i32.load
                local.tee 4
                i32.const 68
                i32.ne
                br_if 1 (;@4;)
                i32.const 512
                local.set 2
                loop ;; label = @6
                  local.get 1
                  local.get 2
                  i32.store offset=12
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 2
                  i32.const 1
                  call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
                  local.get 1
                  i32.load offset=8
                  local.tee 3
                  local.get 1
                  i32.load offset=4
                  local.tee 2
                  call $getcwd
                  br_if 1 (;@5;)
                  global.get $errno
                  i32.load
                  local.tee 4
                  i32.const 68
                  i32.ne
                  br_if 2 (;@4;)
                  br 0 (;@6;)
                end
              end
              local.get 1
              local.get 3
              call $strlen
              local.tee 4
              i32.store offset=12
              block ;; label = @5
                local.get 2
                local.get 4
                i32.le_u
                br_if 0 (;@5;)
                block ;; label = @6
                  block ;; label = @7
                    local.get 4
                    br_if 0 (;@7;)
                    i32.const 1
                    local.set 5
                    local.get 3
                    local.get 2
                    i32.const 1
                    call $__rust_dealloc
                    br 1 (;@6;)
                  end
                  local.get 3
                  local.get 2
                  i32.const 1
                  local.get 4
                  call $__rust_realloc
                  local.tee 5
                  i32.eqz
                  br_if 4 (;@2;)
                end
                local.get 1
                local.get 4
                i32.store offset=4
                local.get 1
                local.get 5
                i32.store offset=8
              end
              local.get 0
              local.get 1
              i64.load offset=4 align=4
              i64.store align=4
              local.get 0
              i32.const 8
              i32.add
              local.get 1
              i32.const 4
              i32.add
              i32.const 8
              i32.add
              i32.load
              i32.store
              br 3 (;@1;)
            end
            local.get 0
            i64.const 2147483648
            i64.store align=4
            local.get 0
            i32.const 8
            i32.add
            local.get 4
            i32.store
            local.get 2
            i32.eqz
            br_if 2 (;@1;)
            local.get 3
            local.get 2
            i32.const 1
            call $__rust_dealloc
            br 2 (;@1;)
          end
          i32.const 1
          i32.const 512
          call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
          unreachable
        end
        i32.const 1
        local.get 4
        call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
        unreachable
      end
      local.get 1
      i32.const 16
      i32.add
      global.set $__stack_pointer
    )
    (func $std::sys::wasi::os::getenv::h007efdd02afe9fc9 (;29;) (type 1) (param i32 i32 i32)
      (local i32 i32 i32)
      global.get $__stack_pointer
      i32.const 416
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 2
              i32.const 383
              i32.gt_u
              br_if 0 (;@4;)
              local.get 3
              i32.const 20
              i32.add
              local.get 1
              local.get 2
              call $memcpy
              drop
              local.get 3
              i32.const 20
              i32.add
              local.get 2
              i32.add
              i32.const 0
              i32.store8
              local.get 3
              i32.const 404
              i32.add
              local.get 3
              i32.const 20
              i32.add
              local.get 2
              i32.const 1
              i32.add
              call $core::ffi::c_str::CStr::from_bytes_with_nul::he3131b60d7cc549c
              block ;; label = @5
                local.get 3
                i32.load offset=404
                br_if 0 (;@5;)
                block ;; label = @6
                  local.get 3
                  i32.load offset=408
                  call $getenv
                  local.tee 1
                  br_if 0 (;@6;)
                  i32.const -2147483648
                  local.set 2
                  br 5 (;@1;)
                end
                block ;; label = @6
                  block ;; label = @7
                    local.get 1
                    call $strlen
                    local.tee 2
                    br_if 0 (;@7;)
                    i32.const 1
                    local.set 4
                    br 1 (;@6;)
                  end
                  local.get 2
                  i32.const -1
                  i32.le_s
                  br_if 3 (;@3;)
                  global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
                  i32.load8_u
                  drop
                  local.get 2
                  i32.const 1
                  call $__rust_alloc
                  local.tee 4
                  i32.eqz
                  br_if 4 (;@2;)
                end
                local.get 4
                local.get 1
                local.get 2
                call $memcpy
                local.set 1
                local.get 3
                local.get 2
                i32.store offset=16
                local.get 3
                local.get 1
                i32.store offset=12
                br 4 (;@1;)
              end
              local.get 3
              global.get $__memory_base
              i32.const 3664
              i32.add
              i64.load
              i64.store offset=12 align=4
              i32.const -2147483647
              local.set 2
              br 3 (;@1;)
            end
            local.get 3
            i32.const 8
            i32.add
            local.get 1
            local.get 2
            call $std::sys::common::small_c_string::run_with_cstr_allocating::h3821304ded5a7ddb
            local.get 3
            i32.load offset=8
            local.set 2
            br 2 (;@1;)
          end
          call $alloc::raw_vec::capacity_overflow::h9441e1e81ed4591e
          unreachable
        end
        i32.const 1
        local.get 2
        call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
        unreachable
      end
      block ;; label = @1
        block ;; label = @2
          local.get 2
          i32.const -2147483647
          i32.ne
          br_if 0 (;@2;)
          block ;; label = @3
            local.get 3
            i32.load8_u offset=12
            i32.const 3
            i32.ne
            br_if 0 (;@3;)
            local.get 3
            i32.const 16
            i32.add
            i32.load
            local.tee 2
            i32.load
            local.tee 4
            local.get 2
            i32.const 4
            i32.add
            i32.load
            local.tee 1
            i32.load
            call_indirect (type 0)
            block ;; label = @4
              local.get 1
              i32.load offset=4
              local.tee 5
              i32.eqz
              br_if 0 (;@4;)
              local.get 4
              local.get 5
              local.get 1
              i32.load offset=8
              call $__rust_dealloc
            end
            local.get 2
            i32.const 12
            i32.const 4
            call $__rust_dealloc
          end
          local.get 0
          i32.const -2147483648
          i32.store
          br 1 (;@1;)
        end
        local.get 0
        local.get 3
        i64.load offset=12 align=4
        i64.store offset=4 align=4
        local.get 0
        local.get 2
        i32.store
      end
      local.get 3
      i32.const 416
      i32.add
      global.set $__stack_pointer
    )
    (func $core::fmt::Write::write_char::h835ac4d37f70aa8f (;30;) (type 4) (param i32 i32) (result i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      local.get 2
      i32.const 0
      i32.store offset=12
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 1
              i32.const 128
              i32.lt_u
              br_if 0 (;@4;)
              local.get 1
              i32.const 2048
              i32.lt_u
              br_if 1 (;@3;)
              local.get 1
              i32.const 65536
              i32.ge_u
              br_if 2 (;@2;)
              local.get 2
              local.get 1
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=14
              local.get 2
              local.get 1
              i32.const 12
              i32.shr_u
              i32.const 224
              i32.or
              i32.store8 offset=12
              local.get 2
              local.get 1
              i32.const 6
              i32.shr_u
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=13
              i32.const 3
              local.set 3
              br 3 (;@1;)
            end
            local.get 2
            local.get 1
            i32.store8 offset=12
            i32.const 1
            local.set 3
            br 2 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 192
          i32.or
          i32.store8 offset=12
          i32.const 2
          local.set 3
          br 1 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=15
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=14
        local.get 2
        local.get 1
        i32.const 12
        i32.shr_u
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 18
        i32.shr_u
        i32.const 7
        i32.and
        i32.const 240
        i32.or
        i32.store8 offset=12
        i32.const 4
        local.set 3
      end
      block ;; label = @1
        local.get 0
        i32.load offset=8
        local.tee 1
        i32.load
        local.get 1
        i32.load offset=8
        local.tee 0
        i32.sub
        local.get 3
        i32.ge_u
        br_if 0 (;@1;)
        local.get 1
        local.get 0
        local.get 3
        call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
        local.get 1
        i32.load offset=8
        local.set 0
      end
      local.get 1
      i32.load offset=4
      local.get 0
      i32.add
      local.get 2
      i32.const 12
      i32.add
      local.get 3
      call $memcpy
      drop
      local.get 1
      local.get 0
      local.get 3
      i32.add
      i32.store offset=8
      local.get 2
      i32.const 16
      i32.add
      global.set $__stack_pointer
      i32.const 0
    )
    (func $core::fmt::Write::write_char::h9db6317e31914445 (;31;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      local.get 2
      i32.const 0
      i32.store offset=12
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 1
              i32.const 128
              i32.lt_u
              br_if 0 (;@4;)
              local.get 1
              i32.const 2048
              i32.lt_u
              br_if 1 (;@3;)
              local.get 1
              i32.const 65536
              i32.ge_u
              br_if 2 (;@2;)
              local.get 2
              local.get 1
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=14
              local.get 2
              local.get 1
              i32.const 12
              i32.shr_u
              i32.const 224
              i32.or
              i32.store8 offset=12
              local.get 2
              local.get 1
              i32.const 6
              i32.shr_u
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=13
              i32.const 3
              local.set 1
              br 3 (;@1;)
            end
            local.get 2
            local.get 1
            i32.store8 offset=12
            i32.const 1
            local.set 1
            br 2 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 192
          i32.or
          i32.store8 offset=12
          i32.const 2
          local.set 1
          br 1 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=15
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=14
        local.get 2
        local.get 1
        i32.const 12
        i32.shr_u
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 18
        i32.shr_u
        i32.const 7
        i32.and
        i32.const 240
        i32.or
        i32.store8 offset=12
        i32.const 4
        local.set 1
      end
      local.get 2
      i32.const 12
      i32.add
      local.set 3
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              loop ;; label = @5
                local.get 2
                local.get 1
                i32.store offset=20
                local.get 2
                local.get 3
                i32.store offset=16
                local.get 2
                i32.const 24
                i32.add
                i32.const 2
                local.get 2
                i32.const 16
                i32.add
                i32.const 1
                call $wasi::lib_generated::fd_write::h23a65eddaa0c426a
                block ;; label = @6
                  block ;; label = @7
                    local.get 2
                    i32.load16_u offset=24
                    br_if 0 (;@7;)
                    global.get $__memory_base
                    local.set 4
                    block ;; label = @8
                      local.get 2
                      i32.load offset=28
                      local.tee 5
                      br_if 0 (;@8;)
                      local.get 4
                      i32.const 4008
                      i32.add
                      local.set 5
                      i32.const 2
                      local.set 1
                      br 5 (;@3;)
                    end
                    local.get 1
                    local.get 5
                    i32.lt_u
                    br_if 6 (;@1;)
                    local.get 3
                    local.get 5
                    i32.add
                    local.set 3
                    local.get 1
                    local.get 5
                    i32.sub
                    local.set 1
                    br 1 (;@6;)
                  end
                  local.get 2
                  i32.load16_u offset=26
                  local.tee 5
                  i32.const 27
                  i32.ne
                  br_if 2 (;@4;)
                end
                local.get 1
                br_if 0 (;@5;)
              end
              i32.const 0
              local.set 1
              br 2 (;@2;)
            end
            i32.const 0
            local.set 1
          end
          local.get 0
          i32.load offset=4
          local.set 4
          block ;; label = @3
            block ;; label = @4
              local.get 0
              i32.load8_u
              local.tee 3
              i32.const 4
              i32.gt_u
              br_if 0 (;@4;)
              local.get 3
              i32.const 3
              i32.ne
              br_if 1 (;@3;)
            end
            local.get 4
            i32.load
            local.tee 6
            local.get 4
            i32.const 4
            i32.add
            i32.load
            local.tee 3
            i32.load
            call_indirect (type 0)
            block ;; label = @4
              local.get 3
              i32.load offset=4
              local.tee 7
              i32.eqz
              br_if 0 (;@4;)
              local.get 6
              local.get 7
              local.get 3
              i32.load offset=8
              call $__rust_dealloc
            end
            local.get 4
            i32.const 12
            i32.const 4
            call $__rust_dealloc
          end
          local.get 0
          local.get 5
          i32.store offset=4
          local.get 0
          local.get 1
          i32.store
          i32.const 1
          local.set 1
        end
        local.get 2
        i32.const 32
        i32.add
        global.set $__stack_pointer
        local.get 1
        return
      end
      local.get 5
      local.get 1
      global.get $__memory_base
      i32.const 4020
      i32.add
      call $core::slice::index::slice_start_index_len_fail::hea1c763acd9dfde3
      unreachable
    )
    (func $core::fmt::Write::write_fmt::h0bb0a982cbee5fbf (;32;) (type 4) (param i32 i32) (result i32)
      local.get 0
      global.get $__memory_base
      i32.const 3672
      i32.add
      local.get 1
      call $core::fmt::write::h9a3c09758e76bf05
    )
    (func $core::fmt::Write::write_fmt::hf0f6500f70dd74b1 (;33;) (type 4) (param i32 i32) (result i32)
      local.get 0
      global.get $__memory_base
      i32.const 3696
      i32.add
      local.get 1
      call $core::fmt::write::h9a3c09758e76bf05
    )
    (func $core::ptr::drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$::hf998f532c50cb705 (;34;) (type 0) (param i32)
      (local i32 i32 i32)
      local.get 0
      i32.load offset=4
      local.set 1
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i32.load8_u
          local.tee 0
          i32.const 4
          i32.gt_u
          br_if 0 (;@2;)
          local.get 0
          i32.const 3
          i32.ne
          br_if 1 (;@1;)
        end
        local.get 1
        i32.load
        local.tee 2
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.tee 0
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 0
          i32.load offset=4
          local.tee 3
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          local.get 3
          local.get 0
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
    )
    (func $_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$::write_str::h0cc9ffee36cd937f (;35;) (type 3) (param i32 i32 i32) (result i32)
      (local i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      i32.const 0
      local.set 4
      block ;; label = @1
        block ;; label = @2
          local.get 2
          i32.eqz
          br_if 0 (;@2;)
          block ;; label = @3
            loop ;; label = @4
              local.get 3
              local.get 2
              i32.store offset=4
              local.get 3
              local.get 1
              i32.store
              local.get 3
              i32.const 8
              i32.add
              i32.const 2
              local.get 3
              i32.const 1
              call $wasi::lib_generated::fd_write::h23a65eddaa0c426a
              block ;; label = @5
                local.get 3
                i32.load16_u offset=8
                br_if 0 (;@5;)
                global.get $__memory_base
                local.set 5
                block ;; label = @6
                  local.get 3
                  i32.load offset=12
                  local.tee 6
                  br_if 0 (;@6;)
                  local.get 5
                  i32.const 4008
                  i32.add
                  local.set 6
                  i32.const 2
                  local.set 2
                  br 3 (;@3;)
                end
                local.get 2
                local.get 6
                i32.lt_u
                br_if 4 (;@1;)
                local.get 1
                local.get 6
                i32.add
                local.set 1
                local.get 2
                local.get 6
                i32.sub
                local.tee 2
                br_if 1 (;@4;)
                br 3 (;@2;)
              end
              block ;; label = @5
                local.get 3
                i32.load16_u offset=10
                local.tee 6
                i32.const 27
                i32.ne
                br_if 0 (;@5;)
                local.get 2
                br_if 1 (;@4;)
                br 3 (;@2;)
              end
            end
            i32.const 0
            local.set 2
          end
          local.get 0
          i32.load offset=4
          local.set 5
          block ;; label = @3
            block ;; label = @4
              local.get 0
              i32.load8_u
              local.tee 1
              i32.const 4
              i32.gt_u
              br_if 0 (;@4;)
              local.get 1
              i32.const 3
              i32.ne
              br_if 1 (;@3;)
            end
            local.get 5
            i32.load
            local.tee 4
            local.get 5
            i32.const 4
            i32.add
            i32.load
            local.tee 1
            i32.load
            call_indirect (type 0)
            block ;; label = @4
              local.get 1
              i32.load offset=4
              local.tee 7
              i32.eqz
              br_if 0 (;@4;)
              local.get 4
              local.get 7
              local.get 1
              i32.load offset=8
              call $__rust_dealloc
            end
            local.get 5
            i32.const 12
            i32.const 4
            call $__rust_dealloc
          end
          local.get 0
          local.get 6
          i32.store offset=4
          local.get 0
          local.get 2
          i32.store
          i32.const 1
          local.set 4
        end
        local.get 3
        i32.const 16
        i32.add
        global.set $__stack_pointer
        local.get 4
        return
      end
      local.get 6
      local.get 2
      global.get $__memory_base
      i32.const 4020
      i32.add
      call $core::slice::index::slice_start_index_len_fail::hea1c763acd9dfde3
      unreachable
    )
    (func $_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$::write_str::h341120c3903b9bf2 (;36;) (type 3) (param i32 i32 i32) (result i32)
      (local i32)
      block ;; label = @1
        local.get 0
        i32.load offset=8
        local.tee 0
        i32.load
        local.get 0
        i32.load offset=8
        local.tee 3
        i32.sub
        local.get 2
        i32.ge_u
        br_if 0 (;@1;)
        local.get 0
        local.get 3
        local.get 2
        call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
        local.get 0
        i32.load offset=8
        local.set 3
      end
      local.get 0
      i32.load offset=4
      local.get 3
      i32.add
      local.get 1
      local.get 2
      call $memcpy
      drop
      local.get 0
      local.get 3
      local.get 2
      i32.add
      i32.store offset=8
      i32.const 0
    )
    (func $std::panicking::panic_count::is_zero_slow_path::h30fc47034d17437a (;37;) (type 8) (result i32)
      global.get $__memory_base
      i32.const 4768
      i32.add
      i32.load
      i32.eqz
    )
    (func $#func38<std::panicking::panic_count::LOCAL_PANIC_COUNT::__getit::hc13cf437609f1ede__.llvm.5565787320568768541_> (@name "std::panicking::panic_count::LOCAL_PANIC_COUNT::__getit::hc13cf437609f1ede (.llvm.5565787320568768541)") (;38;) (type 5) (param i32) (result i32)
      global.get $__memory_base
      i32.const 4768
      i32.add
    )
    (func $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h6af49548f242c026 (;39;) (type 2) (param i32 i32)
      local.get 0
      i64.const 4907860379700932464
      i64.store offset=8
      local.get 0
      i64.const -1960977461435455207
      i64.store
    )
    (func $core::fmt::Write::write_fmt::h31a2c20220c957e6 (;40;) (type 4) (param i32 i32) (result i32)
      local.get 0
      global.get $__memory_base
      i32.const 3736
      i32.add
      local.get 1
      call $core::fmt::write::h9a3c09758e76bf05
    )
    (func $core::ptr::drop_in_place$LT$$LT$alloc..boxed..Box$LT$dyn$u20$core..error..Error$u2b$core..marker..Sync$u2b$core..marker..Send$GT$$u20$as$u20$core..convert..From$LT$alloc..string..String$GT$$GT$..from..StringError$GT$::ha9469d5d85838b71 (;41;) (type 0) (param i32)
      (local i32)
      block ;; label = @1
        local.get 0
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@1;)
        local.get 0
        i32.load offset=4
        local.get 1
        i32.const 1
        call $__rust_dealloc
      end
    )
    (func $#func42<core::ptr::drop_in_place$LT$$RF$u8$GT$::h1255938afe5b5108__.llvm.15332286852275299187_> (@name "core::ptr::drop_in_place$LT$$RF$u8$GT$::h1255938afe5b5108 (.llvm.15332286852275299187)") (;42;) (type 0) (param i32))
    (func $core::panicking::assert_failed::h2389905535db148b (;43;) (type 9) (param i32 i32 i32 i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 5
      global.set $__stack_pointer
      local.get 5
      local.get 2
      i32.store offset=12
      local.get 5
      local.get 1
      i32.store offset=8
      local.get 0
      local.get 5
      i32.const 8
      i32.add
      global.get $__memory_base
      i32.const 3720
      i32.add
      local.tee 2
      local.get 5
      i32.const 12
      i32.add
      local.get 2
      local.get 3
      local.get 4
      call $core::panicking::assert_failed_inner::hda4f32f75bbd9c3b
      unreachable
    )
    (func $_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_char::hbccd3dd660ab8f35 (;44;) (type 4) (param i32 i32) (result i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 1
              i32.const 128
              i32.lt_u
              br_if 0 (;@4;)
              local.get 2
              i32.const 0
              i32.store offset=12
              local.get 1
              i32.const 2048
              i32.lt_u
              br_if 1 (;@3;)
              block ;; label = @5
                local.get 1
                i32.const 65536
                i32.ge_u
                br_if 0 (;@5;)
                local.get 2
                local.get 1
                i32.const 63
                i32.and
                i32.const 128
                i32.or
                i32.store8 offset=14
                local.get 2
                local.get 1
                i32.const 12
                i32.shr_u
                i32.const 224
                i32.or
                i32.store8 offset=12
                local.get 2
                local.get 1
                i32.const 6
                i32.shr_u
                i32.const 63
                i32.and
                i32.const 128
                i32.or
                i32.store8 offset=13
                i32.const 3
                local.set 1
                br 3 (;@2;)
              end
              local.get 2
              local.get 1
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=15
              local.get 2
              local.get 1
              i32.const 6
              i32.shr_u
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=14
              local.get 2
              local.get 1
              i32.const 12
              i32.shr_u
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=13
              local.get 2
              local.get 1
              i32.const 18
              i32.shr_u
              i32.const 7
              i32.and
              i32.const 240
              i32.or
              i32.store8 offset=12
              i32.const 4
              local.set 1
              br 2 (;@2;)
            end
            block ;; label = @4
              local.get 0
              i32.load offset=8
              local.tee 3
              local.get 0
              i32.load
              i32.ne
              br_if 0 (;@4;)
              local.get 0
              local.get 3
              call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve_for_push::h6b611529b178451b
              local.get 0
              i32.load offset=8
              local.set 3
            end
            local.get 0
            i32.load offset=4
            local.get 3
            i32.add
            local.get 1
            i32.store8
            local.get 0
            local.get 0
            i32.load offset=8
            i32.const 1
            i32.add
            i32.store offset=8
            br 2 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 192
          i32.or
          i32.store8 offset=12
          i32.const 2
          local.set 1
        end
        block ;; label = @2
          local.get 0
          i32.load
          local.get 0
          i32.load offset=8
          local.tee 3
          i32.sub
          local.get 1
          i32.ge_u
          br_if 0 (;@2;)
          local.get 0
          local.get 3
          local.get 1
          call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
          local.get 0
          i32.load offset=8
          local.set 3
        end
        local.get 0
        i32.load offset=4
        local.get 3
        i32.add
        local.get 2
        i32.const 12
        i32.add
        local.get 1
        call $memcpy
        drop
        local.get 0
        local.get 3
        local.get 1
        i32.add
        i32.store offset=8
      end
      local.get 2
      i32.const 16
      i32.add
      global.set $__stack_pointer
      i32.const 0
    )
    (func $_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_str::ha232be781f4501bb (;45;) (type 3) (param i32 i32 i32) (result i32)
      (local i32)
      block ;; label = @1
        local.get 0
        i32.load
        local.get 0
        i32.load offset=8
        local.tee 3
        i32.sub
        local.get 2
        i32.ge_u
        br_if 0 (;@1;)
        local.get 0
        local.get 3
        local.get 2
        call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
        local.get 0
        i32.load offset=8
        local.set 3
      end
      local.get 0
      i32.load offset=4
      local.get 3
      i32.add
      local.get 1
      local.get 2
      call $memcpy
      drop
      local.get 0
      local.get 3
      local.get 2
      i32.add
      i32.store offset=8
      i32.const 0
    )
    (func $std::process::abort::h601f51430a891ccd (;46;) (type 6)
      call $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7
      unreachable
    )
    (func $#func47<core::ptr::drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$::hf998f532c50cb705__.llvm.16956913488488451105_> (@name "core::ptr::drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$::hf998f532c50cb705 (.llvm.16956913488488451105)") (;47;) (type 0) (param i32)
      (local i32 i32 i32)
      local.get 0
      i32.load offset=4
      local.set 1
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i32.load8_u
          local.tee 0
          i32.const 4
          i32.gt_u
          br_if 0 (;@2;)
          local.get 0
          i32.const 3
          i32.ne
          br_if 1 (;@1;)
        end
        local.get 1
        i32.load
        local.tee 2
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.tee 0
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 0
          i32.load offset=4
          local.tee 3
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          local.get 3
          local.get 0
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
    )
    (func $#func48<core::cell::once::OnceCell$LT$T$GT$::get_or_try_init::outlined_call::hd352954db2dff7b1__.llvm.16956913488488451105_> (@name "core::cell::once::OnceCell$LT$T$GT$::get_or_try_init::outlined_call::hd352954db2dff7b1 (.llvm.16956913488488451105)") (;48;) (type 8) (result i32)
      (local i32 i32 i32 i32 i64 i64 i64)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 0
      global.set $__stack_pointer
      local.get 0
      i32.const 8
      i32.add
      i32.const 8
      i32.const 16
      call $alloc::sync::arcinner_layout_for_value_layout::he4ff1f6789a3f57f
      local.get 0
      i32.load offset=8
      local.set 1
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i32.load offset=12
          local.tee 2
          br_if 0 (;@2;)
          local.get 1
          local.set 3
          br 1 (;@1;)
        end
        global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
        i32.load8_u
        drop
        local.get 2
        local.get 1
        call $__rust_alloc
        local.set 3
      end
      block ;; label = @1
        block ;; label = @2
          local.get 3
          i32.eqz
          br_if 0 (;@2;)
          local.get 3
          i64.const 4294967297
          i64.store
          local.get 3
          i32.const 16
          i32.add
          i32.const 0
          i32.store
          global.get $__memory_base
          i32.const 4784
          i32.add
          i64.load
          local.set 4
          loop ;; label = @3
            local.get 4
            i64.const 1
            i64.add
            local.tee 5
            i64.eqz
            br_if 2 (;@1;)
            global.get $__memory_base
            i32.const 4784
            i32.add
            local.tee 1
            local.get 5
            local.get 1
            i64.load
            local.tee 6
            local.get 6
            local.get 4
            i64.eq
            local.tee 1
            select
            i64.store
            local.get 6
            local.set 4
            local.get 1
            i32.eqz
            br_if 0 (;@3;)
          end
          local.get 3
          local.get 5
          i64.store offset=8
          local.get 0
          i32.const 16
          i32.add
          global.set $__stack_pointer
          local.get 3
          return
        end
        local.get 1
        local.get 2
        call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
        unreachable
      end
      call $#func49<std::thread::ThreadId::new::exhausted::hb98b6d6d1e9bc475__.llvm.16956913488488451105_>
      unreachable
    )
    (func $#func49<std::thread::ThreadId::new::exhausted::hb98b6d6d1e9bc475__.llvm.16956913488488451105_> (@name "std::thread::ThreadId::new::exhausted::hb98b6d6d1e9bc475 (.llvm.16956913488488451105)") (;49;) (type 6)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 0
      global.set $__stack_pointer
      local.get 0
      i32.const 20
      i32.add
      i64.const 0
      i64.store align=4
      local.get 0
      i32.const 1
      i32.store offset=12
      local.get 0
      global.get $__memory_base
      local.tee 1
      i32.const 3784
      i32.add
      i32.store offset=8
      local.get 0
      local.get 1
      i32.const 324
      i32.add
      i32.store offset=16
      local.get 0
      i32.const 8
      i32.add
      local.get 1
      i32.const 3792
      i32.add
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $std::io::Write::write_all_vectored::ha7c695d0a1fd5e82 (;50;) (type 10) (param i32 i32 i32 i32)
      (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 4
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 3
              i32.eqz
              br_if 0 (;@4;)
              local.get 2
              i32.const 4
              i32.add
              local.set 5
              local.get 3
              i32.const 3
              i32.shl
              local.set 6
              local.get 3
              i32.const -1
              i32.add
              i32.const 536870911
              i32.and
              i32.const 1
              i32.add
              local.set 7
              i32.const 0
              local.set 8
              block ;; label = @5
                loop ;; label = @6
                  local.get 5
                  i32.load
                  br_if 1 (;@5;)
                  local.get 5
                  i32.const 8
                  i32.add
                  local.set 5
                  local.get 8
                  i32.const 1
                  i32.add
                  local.set 8
                  local.get 6
                  i32.const -8
                  i32.add
                  local.tee 6
                  br_if 0 (;@6;)
                end
                local.get 7
                local.set 8
              end
              local.get 3
              local.get 8
              i32.lt_u
              br_if 2 (;@2;)
              local.get 3
              local.get 8
              i32.sub
              local.tee 9
              i32.eqz
              br_if 0 (;@4;)
              local.get 2
              local.get 8
              i32.const 3
              i32.shl
              i32.add
              local.set 10
              loop ;; label = @5
                i32.const 0
                local.set 6
                i32.const 0
                local.set 3
                block ;; label = @6
                  local.get 9
                  i32.const -1
                  i32.add
                  local.tee 11
                  i32.const 3
                  i32.lt_u
                  br_if 0 (;@6;)
                  local.get 10
                  i32.const 28
                  i32.add
                  local.set 5
                  local.get 9
                  i32.const -4
                  i32.and
                  local.set 8
                  i32.const 0
                  local.set 6
                  i32.const 0
                  local.set 3
                  loop ;; label = @7
                    local.get 5
                    i32.load
                    local.get 5
                    i32.const -8
                    i32.add
                    i32.load
                    local.get 5
                    i32.const -16
                    i32.add
                    i32.load
                    local.get 5
                    i32.const -24
                    i32.add
                    i32.load
                    local.get 6
                    i32.add
                    i32.add
                    i32.add
                    i32.add
                    local.set 6
                    local.get 5
                    i32.const 32
                    i32.add
                    local.set 5
                    local.get 8
                    local.get 3
                    i32.const 4
                    i32.add
                    local.tee 3
                    i32.ne
                    br_if 0 (;@7;)
                  end
                end
                block ;; label = @6
                  local.get 9
                  i32.const 3
                  i32.and
                  local.tee 8
                  i32.eqz
                  br_if 0 (;@6;)
                  local.get 10
                  local.get 3
                  i32.const 3
                  i32.shl
                  i32.add
                  i32.const 4
                  i32.add
                  local.set 5
                  loop ;; label = @7
                    local.get 5
                    i32.load
                    local.get 6
                    i32.add
                    local.set 6
                    local.get 5
                    i32.const 8
                    i32.add
                    local.set 5
                    local.get 8
                    i32.const -1
                    i32.add
                    local.tee 8
                    br_if 0 (;@7;)
                  end
                end
                block ;; label = @6
                  local.get 1
                  i32.load
                  local.get 1
                  i32.load offset=8
                  local.tee 5
                  i32.sub
                  local.get 6
                  i32.ge_u
                  br_if 0 (;@6;)
                  local.get 1
                  local.get 5
                  local.get 6
                  call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
                  local.get 1
                  i32.load offset=8
                  local.set 5
                end
                local.get 10
                local.get 9
                i32.const 3
                i32.shl
                local.tee 12
                i32.add
                local.set 7
                local.get 10
                local.set 8
                loop ;; label = @6
                  local.get 8
                  i32.load
                  local.set 2
                  block ;; label = @7
                    local.get 1
                    i32.load
                    local.get 5
                    i32.sub
                    local.get 8
                    i32.load offset=4
                    local.tee 3
                    i32.ge_u
                    br_if 0 (;@7;)
                    local.get 1
                    local.get 5
                    local.get 3
                    call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
                    local.get 1
                    i32.load offset=8
                    local.set 5
                  end
                  local.get 1
                  i32.load offset=4
                  local.get 5
                  i32.add
                  local.get 2
                  local.get 3
                  call $memcpy
                  drop
                  local.get 1
                  local.get 5
                  local.get 3
                  i32.add
                  local.tee 5
                  i32.store offset=8
                  local.get 8
                  i32.const 8
                  i32.add
                  local.tee 8
                  local.get 7
                  i32.ne
                  br_if 0 (;@6;)
                end
                block ;; label = @6
                  local.get 6
                  br_if 0 (;@6;)
                  local.get 0
                  i32.const 2
                  i32.store8
                  local.get 0
                  global.get $__memory_base
                  i32.const 3848
                  i32.add
                  i32.store offset=4
                  br 5 (;@1;)
                end
                local.get 10
                i32.const 4
                i32.add
                local.set 5
                local.get 11
                i32.const 536870911
                i32.and
                i32.const 1
                i32.add
                local.set 2
                i32.const 0
                local.set 8
                block ;; label = @6
                  loop ;; label = @7
                    local.get 6
                    local.get 5
                    i32.load
                    local.tee 3
                    i32.lt_u
                    br_if 1 (;@6;)
                    local.get 5
                    i32.const 8
                    i32.add
                    local.set 5
                    local.get 8
                    i32.const 1
                    i32.add
                    local.set 8
                    local.get 6
                    local.get 3
                    i32.sub
                    local.set 6
                    local.get 12
                    i32.const -8
                    i32.add
                    local.tee 12
                    br_if 0 (;@7;)
                  end
                  local.get 2
                  local.set 8
                end
                block ;; label = @6
                  block ;; label = @7
                    block ;; label = @8
                      local.get 9
                      local.get 8
                      i32.lt_u
                      br_if 0 (;@8;)
                      local.get 10
                      local.get 8
                      i32.const 3
                      i32.shl
                      i32.add
                      local.set 10
                      local.get 9
                      local.get 8
                      i32.ne
                      br_if 1 (;@7;)
                      local.get 6
                      i32.eqz
                      br_if 2 (;@6;)
                      local.get 4
                      i32.const 20
                      i32.add
                      i64.const 0
                      i64.store align=4
                      local.get 4
                      i32.const 1
                      i32.store offset=12
                      local.get 4
                      global.get $__memory_base
                      local.tee 5
                      i32.const 3808
                      i32.add
                      i32.store offset=8
                      local.get 4
                      local.get 5
                      i32.const 324
                      i32.add
                      i32.store offset=16
                      local.get 4
                      i32.const 8
                      i32.add
                      local.get 5
                      i32.const 3816
                      i32.add
                      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
                      unreachable
                    end
                    local.get 8
                    local.get 9
                    global.get $__memory_base
                    i32.const 3832
                    i32.add
                    call $core::slice::index::slice_start_index_len_fail::hea1c763acd9dfde3
                    unreachable
                  end
                  local.get 10
                  i32.load offset=4
                  local.tee 5
                  local.get 6
                  i32.lt_u
                  br_if 3 (;@3;)
                  local.get 10
                  local.get 5
                  local.get 6
                  i32.sub
                  i32.store offset=4
                  local.get 10
                  local.get 10
                  i32.load
                  local.get 6
                  i32.add
                  i32.store
                end
                local.get 9
                local.get 8
                i32.sub
                local.tee 9
                br_if 0 (;@5;)
              end
            end
            local.get 0
            i32.const 4
            i32.store8
            br 2 (;@1;)
          end
          local.get 4
          i32.const 20
          i32.add
          i64.const 0
          i64.store align=4
          local.get 4
          i32.const 1
          i32.store offset=12
          local.get 4
          global.get $__memory_base
          local.tee 5
          i32.const 3896
          i32.add
          i32.store offset=8
          local.get 4
          local.get 5
          i32.const 324
          i32.add
          i32.store offset=16
          local.get 4
          i32.const 8
          i32.add
          local.get 5
          i32.const 3904
          i32.add
          call $core::panicking::panic_fmt::hf3388a9334a7d4c4
          unreachable
        end
        local.get 8
        local.get 3
        global.get $__memory_base
        i32.const 3832
        i32.add
        call $core::slice::index::slice_start_index_len_fail::hea1c763acd9dfde3
        unreachable
      end
      local.get 4
      i32.const 32
      i32.add
      global.set $__stack_pointer
    )
    (func $std::io::Write::write_fmt::h1462c39bcd355635 (;51;) (type 1) (param i32 i32 i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      local.get 3
      i32.const 4
      i32.store8
      local.get 3
      local.get 1
      i32.store offset=8
      block ;; label = @1
        block ;; label = @2
          local.get 3
          global.get $__memory_base
          i32.const 3860
          i32.add
          local.get 2
          call $core::fmt::write::h9a3c09758e76bf05
          i32.eqz
          br_if 0 (;@2;)
          block ;; label = @3
            local.get 3
            i32.load8_u
            i32.const 4
            i32.ne
            br_if 0 (;@3;)
            local.get 0
            i32.const 2
            i32.store8
            local.get 0
            global.get $__memory_base
            i32.const 3884
            i32.add
            i32.store offset=4
            br 2 (;@1;)
          end
          local.get 0
          local.get 3
          i64.load
          i64.store align=4
          br 1 (;@1;)
        end
        local.get 0
        i32.const 4
        i32.store8
        local.get 3
        i32.load offset=4
        local.set 1
        block ;; label = @2
          local.get 3
          i32.load8_u
          local.tee 0
          i32.const 4
          i32.gt_u
          br_if 0 (;@2;)
          local.get 0
          i32.const 3
          i32.ne
          br_if 1 (;@1;)
        end
        local.get 1
        i32.load
        local.tee 2
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.tee 0
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 0
          i32.load offset=4
          local.tee 4
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          local.get 4
          local.get 0
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      local.get 3
      i32.const 16
      i32.add
      global.set $__stack_pointer
    )
    (func $__rdl_alloc (;52;) (type 4) (param i32 i32) (result i32)
      (local i32)
      block ;; label = @1
        block ;; label = @2
          local.get 1
          i32.const 8
          i32.gt_u
          br_if 0 (;@2;)
          local.get 1
          local.get 0
          i32.le_u
          br_if 1 (;@1;)
        end
        local.get 1
        local.get 1
        local.get 0
        local.get 1
        i32.rem_u
        local.tee 2
        i32.sub
        i32.const 0
        local.get 2
        select
        local.get 0
        i32.add
        call $aligned_alloc
        return
      end
      local.get 0
      call $malloc
    )
    (func $__rdl_dealloc (;53;) (type 1) (param i32 i32 i32)
      local.get 0
      call $free
    )
    (func $__rdl_realloc (;54;) (type 7) (param i32 i32 i32 i32) (result i32)
      (local i32 i32)
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            local.get 2
            i32.const 8
            i32.gt_u
            br_if 0 (;@3;)
            local.get 2
            local.get 3
            i32.le_u
            br_if 1 (;@2;)
          end
          i32.const 0
          local.set 4
          local.get 2
          local.get 2
          local.get 3
          local.get 2
          i32.rem_u
          local.tee 5
          i32.sub
          i32.const 0
          local.get 5
          select
          local.get 3
          i32.add
          call $aligned_alloc
          local.tee 2
          i32.eqz
          br_if 1 (;@1;)
          local.get 2
          local.get 0
          local.get 1
          local.get 3
          local.get 1
          local.get 3
          i32.lt_u
          select
          call $memcpy
          local.set 2
          local.get 0
          call $free
          local.get 2
          return
        end
        local.get 0
        local.get 3
        call $realloc
        local.set 4
      end
      local.get 4
    )
    (func $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h46e973aefffffd83 (;55;) (type 2) (param i32 i32)
      local.get 0
      i64.const -163230743173927068
      i64.store offset=8
      local.get 0
      i64.const -4493808902380553279
      i64.store
    )
    (func $#func56<core::ptr::drop_in_place$LT$std..thread..local..AccessError$GT$::h2b846ef7bba63faf__.llvm.11225111681125628113_> (@name "core::ptr::drop_in_place$LT$std..thread..local..AccessError$GT$::h2b846ef7bba63faf (.llvm.11225111681125628113)") (;56;) (type 0) (param i32))
    (func $_$LT$std..thread..local..AccessError$u20$as$u20$core..fmt..Debug$GT$::fmt::h894d1eb0ce9c3529 (;57;) (type 4) (param i32 i32) (result i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      local.get 2
      i32.const 8
      i32.add
      local.get 1
      global.get $__memory_base
      i32.const 951
      i32.add
      i32.const 11
      call $core::fmt::Formatter::debug_struct::ha9d8a8b5657b52dc
      local.get 2
      i32.const 8
      i32.add
      call $core::fmt::builders::DebugStruct::finish::hf1e218cbc9531c50
      local.set 1
      local.get 2
      i32.const 16
      i32.add
      global.set $__stack_pointer
      local.get 1
    )
    (func $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7 (;58;) (type 6)
      call $abort
      unreachable
    )
    (func $std::panic::get_backtrace_style::h42fdd7217493e6d2 (;59;) (type 8) (result i32)
      (local i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 0
      global.set $__stack_pointer
      i32.const 0
      local.set 1
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                global.get $__memory_base
                i32.const 4796
                i32.add
                i32.load
                br_table 3 (;@2;) 4 (;@1;) 2 (;@3;) 1 (;@4;) 0 (;@5;)
              end
              global.get $__memory_base
              local.tee 1
              i32.const 1159
              i32.add
              i32.const 40
              local.get 1
              i32.const 3952
              i32.add
              call $core::panicking::panic::h6b5f2399fab9b82b
              unreachable
            end
            i32.const 2
            local.set 1
            br 2 (;@1;)
          end
          i32.const 1
          local.set 1
          br 1 (;@1;)
        end
        local.get 0
        i32.const 4
        i32.add
        global.get $__memory_base
        i32.const 1145
        i32.add
        i32.const 14
        call $std::sys::wasi::os::getenv::h007efdd02afe9fc9
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                local.get 0
                i32.load offset=4
                local.tee 2
                i32.const -2147483648
                i32.eq
                br_if 0 (;@5;)
                i32.const 0
                local.set 1
                local.get 0
                i32.load offset=8
                local.set 3
                block ;; label = @6
                  local.get 0
                  i32.load offset=12
                  i32.const -1
                  i32.add
                  br_table 0 (;@6;) 3 (;@3;) 3 (;@3;) 2 (;@4;) 3 (;@3;)
                end
                local.get 3
                i32.load8_u
                i32.const 48
                i32.ne
                br_if 2 (;@3;)
                local.get 2
                i32.eqz
                br_if 0 (;@5;)
                local.get 3
                local.get 2
                i32.const 1
                call $__rust_dealloc
              end
              i32.const 3
              local.set 2
              i32.const 2
              local.set 1
              br 2 (;@2;)
            end
            local.get 3
            i32.load align=1
            i32.const 1819047270
            i32.eq
            local.set 1
          end
          block ;; label = @3
            local.get 2
            i32.eqz
            br_if 0 (;@3;)
            local.get 3
            local.get 2
            i32.const 1
            call $__rust_dealloc
          end
          i32.const 2
          i32.const 1
          local.get 1
          select
          local.set 2
        end
        global.get $__memory_base
        i32.const 4796
        i32.add
        local.get 2
        i32.store
      end
      local.get 0
      i32.const 16
      i32.add
      global.set $__stack_pointer
      local.get 1
    )
    (func $_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$::fmt::h5885aa12204b115c (;60;) (type 4) (param i32 i32) (result i32)
      local.get 0
      i32.load
      local.get 1
      call $_$LT$core..panic..location..Location$u20$as$u20$core..fmt..Display$GT$::fmt::hcf775eb1a6630495
    )
    (func $#func61<core::ptr::drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$std..sys..wasi..stdio..Stderr$GT$$GT$::hdaf8233befe1c34a__.llvm.9557612308607314467_> (@name "core::ptr::drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$std..sys..wasi..stdio..Stderr$GT$$GT$::hdaf8233befe1c34a (.llvm.9557612308607314467)") (;61;) (type 0) (param i32)
      (local i32 i32 i32)
      local.get 0
      i32.load offset=4
      local.set 1
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i32.load8_u
          local.tee 0
          i32.const 4
          i32.gt_u
          br_if 0 (;@2;)
          local.get 0
          i32.const 3
          i32.ne
          br_if 1 (;@1;)
        end
        local.get 1
        i32.load
        local.tee 2
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.tee 0
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 0
          i32.load offset=4
          local.tee 3
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          local.get 3
          local.get 0
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
    )
    (func $std::io::Write::write_all::he1386585803e5b4d (;62;) (type 10) (param i32 i32 i32 i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 4
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 3
              i32.eqz
              br_if 0 (;@4;)
              loop ;; label = @5
                local.get 4
                local.get 3
                i32.store offset=4
                local.get 4
                local.get 2
                i32.store
                local.get 4
                i32.const 8
                i32.add
                i32.const 2
                local.get 4
                i32.const 1
                call $wasi::lib_generated::fd_write::h23a65eddaa0c426a
                block ;; label = @6
                  block ;; label = @7
                    local.get 4
                    i32.load16_u offset=8
                    br_if 0 (;@7;)
                    block ;; label = @8
                      local.get 4
                      i32.load offset=12
                      local.tee 5
                      br_if 0 (;@8;)
                      local.get 0
                      i32.const 2
                      i32.store8
                      local.get 0
                      global.get $__memory_base
                      i32.const 4008
                      i32.add
                      i32.store offset=4
                      br 7 (;@1;)
                    end
                    local.get 3
                    local.get 5
                    i32.lt_u
                    br_if 4 (;@3;)
                    local.get 2
                    local.get 5
                    i32.add
                    local.set 2
                    local.get 3
                    local.get 5
                    i32.sub
                    local.set 3
                    br 1 (;@6;)
                  end
                  local.get 4
                  i32.load16_u offset=10
                  local.tee 5
                  i32.const 27
                  i32.ne
                  br_if 4 (;@2;)
                end
                local.get 3
                br_if 0 (;@5;)
              end
            end
            local.get 0
            i32.const 4
            i32.store8
            br 2 (;@1;)
          end
          local.get 5
          local.get 3
          global.get $__memory_base
          i32.const 4020
          i32.add
          call $core::slice::index::slice_start_index_len_fail::hea1c763acd9dfde3
          unreachable
        end
        local.get 0
        local.get 5
        i32.store offset=4
        local.get 0
        i32.const 0
        i32.store
      end
      local.get 4
      i32.const 16
      i32.add
      global.set $__stack_pointer
    )
    (func $std::io::Write::write_all_vectored::h7c465c96bfe57a1f (;63;) (type 10) (param i32 i32 i32 i32)
      (local i32 i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 4
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 3
              i32.eqz
              br_if 0 (;@4;)
              local.get 2
              i32.const 4
              i32.add
              local.set 5
              local.get 3
              i32.const 3
              i32.shl
              local.set 6
              local.get 3
              i32.const -1
              i32.add
              i32.const 536870911
              i32.and
              i32.const 1
              i32.add
              local.set 7
              i32.const 0
              local.set 8
              block ;; label = @5
                loop ;; label = @6
                  local.get 5
                  i32.load
                  br_if 1 (;@5;)
                  local.get 5
                  i32.const 8
                  i32.add
                  local.set 5
                  local.get 8
                  i32.const 1
                  i32.add
                  local.set 8
                  local.get 6
                  i32.const -8
                  i32.add
                  local.tee 6
                  br_if 0 (;@6;)
                end
                local.get 7
                local.set 8
              end
              local.get 3
              local.get 8
              i32.lt_u
              br_if 3 (;@1;)
              local.get 3
              local.get 8
              i32.sub
              local.tee 7
              i32.eqz
              br_if 0 (;@4;)
              local.get 2
              local.get 8
              i32.const 3
              i32.shl
              i32.add
              local.set 9
              loop ;; label = @5
                local.get 4
                i32.const 8
                i32.add
                i32.const 2
                local.get 9
                local.get 7
                call $wasi::lib_generated::fd_write::h23a65eddaa0c426a
                block ;; label = @6
                  block ;; label = @7
                    block ;; label = @8
                      local.get 4
                      i32.load16_u offset=8
                      br_if 0 (;@8;)
                      block ;; label = @9
                        local.get 4
                        i32.load offset=12
                        local.tee 5
                        br_if 0 (;@9;)
                        local.get 0
                        i32.const 2
                        i32.store8
                        local.get 0
                        global.get $__memory_base
                        i32.const 4008
                        i32.add
                        i32.store offset=4
                        br 7 (;@2;)
                      end
                      local.get 9
                      i32.const 4
                      i32.add
                      local.set 8
                      local.get 7
                      i32.const 3
                      i32.shl
                      local.set 3
                      local.get 7
                      i32.const -1
                      i32.add
                      i32.const 536870911
                      i32.and
                      i32.const 1
                      i32.add
                      local.set 10
                      i32.const 0
                      local.set 6
                      loop ;; label = @9
                        local.get 5
                        local.get 8
                        i32.load
                        local.tee 2
                        i32.lt_u
                        br_if 2 (;@7;)
                        local.get 8
                        i32.const 8
                        i32.add
                        local.set 8
                        local.get 6
                        i32.const 1
                        i32.add
                        local.set 6
                        local.get 5
                        local.get 2
                        i32.sub
                        local.set 5
                        local.get 3
                        i32.const -8
                        i32.add
                        local.tee 3
                        br_if 0 (;@9;)
                      end
                      local.get 10
                      local.set 6
                      br 1 (;@7;)
                    end
                    local.get 4
                    i32.load16_u offset=10
                    local.tee 5
                    i32.const 27
                    i32.eq
                    br_if 1 (;@6;)
                    local.get 0
                    local.get 5
                    i32.store offset=4
                    local.get 0
                    i32.const 0
                    i32.store
                    br 5 (;@2;)
                  end
                  block ;; label = @7
                    block ;; label = @8
                      local.get 7
                      local.get 6
                      i32.lt_u
                      br_if 0 (;@8;)
                      local.get 7
                      local.get 6
                      i32.sub
                      local.set 8
                      local.get 9
                      local.get 6
                      i32.const 3
                      i32.shl
                      i32.add
                      local.set 9
                      local.get 7
                      local.get 6
                      i32.ne
                      br_if 1 (;@7;)
                      local.get 8
                      local.set 7
                      local.get 5
                      i32.eqz
                      br_if 2 (;@6;)
                      local.get 4
                      i32.const 20
                      i32.add
                      i64.const 0
                      i64.store align=4
                      local.get 4
                      i32.const 1
                      i32.store offset=12
                      local.get 4
                      global.get $__memory_base
                      local.tee 5
                      i32.const 3968
                      i32.add
                      i32.store offset=8
                      local.get 4
                      local.get 5
                      i32.const 1308
                      i32.add
                      i32.store offset=16
                      local.get 4
                      i32.const 8
                      i32.add
                      local.get 5
                      i32.const 3976
                      i32.add
                      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
                      unreachable
                    end
                    local.get 6
                    local.get 7
                    global.get $__memory_base
                    i32.const 3992
                    i32.add
                    call $core::slice::index::slice_start_index_len_fail::hea1c763acd9dfde3
                    unreachable
                  end
                  local.get 9
                  i32.load offset=4
                  local.tee 6
                  local.get 5
                  i32.lt_u
                  br_if 3 (;@3;)
                  local.get 9
                  local.get 6
                  local.get 5
                  i32.sub
                  i32.store offset=4
                  local.get 9
                  local.get 9
                  i32.load
                  local.get 5
                  i32.add
                  i32.store
                  local.get 8
                  local.set 7
                end
                local.get 7
                br_if 0 (;@5;)
              end
            end
            local.get 0
            i32.const 4
            i32.store8
            br 1 (;@2;)
          end
          local.get 4
          i32.const 20
          i32.add
          i64.const 0
          i64.store align=4
          local.get 4
          i32.const 1
          i32.store offset=12
          local.get 4
          global.get $__memory_base
          local.tee 5
          i32.const 4072
          i32.add
          i32.store offset=8
          local.get 4
          local.get 5
          i32.const 1308
          i32.add
          i32.store offset=16
          local.get 4
          i32.const 8
          i32.add
          local.get 5
          i32.const 4080
          i32.add
          call $core::panicking::panic_fmt::hf3388a9334a7d4c4
          unreachable
        end
        local.get 4
        i32.const 32
        i32.add
        global.set $__stack_pointer
        return
      end
      local.get 8
      local.get 3
      global.get $__memory_base
      i32.const 3992
      i32.add
      call $core::slice::index::slice_start_index_len_fail::hea1c763acd9dfde3
      unreachable
    )
    (func $std::io::Write::write_fmt::h0b43c07e3174d5bc (;64;) (type 1) (param i32 i32 i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      local.get 3
      i32.const 4
      i32.store8
      local.get 3
      local.get 1
      i32.store offset=8
      block ;; label = @1
        block ;; label = @2
          local.get 3
          global.get $__memory_base
          i32.const 4036
          i32.add
          local.get 2
          call $core::fmt::write::h9a3c09758e76bf05
          i32.eqz
          br_if 0 (;@2;)
          block ;; label = @3
            local.get 3
            i32.load8_u
            i32.const 4
            i32.ne
            br_if 0 (;@3;)
            local.get 0
            i32.const 2
            i32.store8
            local.get 0
            global.get $__memory_base
            i32.const 4060
            i32.add
            i32.store offset=4
            br 2 (;@1;)
          end
          local.get 0
          local.get 3
          i64.load
          i64.store align=4
          br 1 (;@1;)
        end
        local.get 0
        i32.const 4
        i32.store8
        local.get 3
        i32.load offset=4
        local.set 1
        block ;; label = @2
          local.get 3
          i32.load8_u
          local.tee 0
          i32.const 4
          i32.gt_u
          br_if 0 (;@2;)
          local.get 0
          i32.const 3
          i32.ne
          br_if 1 (;@1;)
        end
        local.get 1
        i32.load
        local.tee 2
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.tee 0
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 0
          i32.load offset=4
          local.tee 4
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          local.get 4
          local.get 0
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      local.get 3
      i32.const 16
      i32.add
      global.set $__stack_pointer
    )
    (func $_$LT$std..sys..wasi..stdio..Stderr$u20$as$u20$std..io..Write$GT$::write::had566fb5db304fce (;65;) (type 10) (param i32 i32 i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 4
      global.set $__stack_pointer
      local.get 4
      local.get 3
      i32.store offset=4
      local.get 4
      local.get 2
      i32.store
      local.get 4
      i32.const 8
      i32.add
      i32.const 2
      local.get 4
      i32.const 1
      call $wasi::lib_generated::fd_write::h23a65eddaa0c426a
      block ;; label = @1
        block ;; label = @2
          local.get 4
          i32.load16_u offset=8
          br_if 0 (;@2;)
          local.get 0
          local.get 4
          i32.load offset=12
          i32.store offset=4
          local.get 0
          i32.const 4
          i32.store8
          br 1 (;@1;)
        end
        local.get 0
        local.get 4
        i64.load16_u offset=10
        i64.const 32
        i64.shl
        i64.store align=4
      end
      local.get 4
      i32.const 16
      i32.add
      global.set $__stack_pointer
    )
    (func $_$LT$std..sys..wasi..stdio..Stderr$u20$as$u20$std..io..Write$GT$::write_vectored::ha0d7307e4f52640c (;66;) (type 10) (param i32 i32 i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 4
      global.set $__stack_pointer
      local.get 4
      i32.const 8
      i32.add
      i32.const 2
      local.get 2
      local.get 3
      call $wasi::lib_generated::fd_write::h23a65eddaa0c426a
      block ;; label = @1
        block ;; label = @2
          local.get 4
          i32.load16_u offset=8
          br_if 0 (;@2;)
          local.get 0
          local.get 4
          i32.load offset=12
          i32.store offset=4
          local.get 0
          i32.const 4
          i32.store8
          br 1 (;@1;)
        end
        local.get 0
        local.get 4
        i64.load16_u offset=10
        i64.const 32
        i64.shl
        i64.store align=4
      end
      local.get 4
      i32.const 16
      i32.add
      global.set $__stack_pointer
    )
    (func $core::ptr::drop_in_place$LT$$RF$str$GT$::h6b1dc4f502fe29b8 (;67;) (type 0) (param i32))
    (func $core::ptr::drop_in_place$LT$std..panicking..begin_panic_handler..FormatStringPayload$GT$::h5dc476b836162c7e (;68;) (type 0) (param i32)
      (local i32)
      block ;; label = @1
        local.get 0
        i32.load
        local.tee 1
        i32.const -2147483648
        i32.eq
        br_if 0 (;@1;)
        local.get 1
        i32.eqz
        br_if 0 (;@1;)
        local.get 0
        i32.load offset=4
        local.get 1
        i32.const 1
        call $__rust_dealloc
      end
    )
    (func $std::sys_common::backtrace::print::h54d309d45860f40c (;69;) (type 10) (param i32 i32 i32 i32)
      (local i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 48
      i32.sub
      local.tee 4
      global.set $__stack_pointer
      global.get $__memory_base
      i32.const 4800
      i32.add
      local.tee 5
      i32.load8_u
      local.set 6
      i32.const 1
      local.set 7
      local.get 5
      i32.const 1
      i32.store8
      local.get 4
      local.get 6
      i32.store8 offset=36
      block ;; label = @1
        local.get 6
        br_if 0 (;@1;)
        block ;; label = @2
          global.get $__memory_base
          i32.const 4776
          i32.add
          i32.load
          i32.const 2147483647
          i32.and
          i32.eqz
          br_if 0 (;@2;)
          call $std::panicking::panic_count::is_zero_slow_path::h30fc47034d17437a
          local.set 7
        end
        local.get 2
        i32.const 36
        i32.add
        i32.load
        local.set 6
        local.get 4
        i32.const 24
        i32.add
        i64.const 1
        i64.store align=4
        local.get 4
        i32.const 1
        i32.store offset=16
        local.get 4
        global.get $__memory_base
        i32.const 4096
        i32.add
        i32.store offset=12
        local.get 4
        global.get $__table_base
        i32.const 17
        i32.add
        i32.store offset=40
        local.get 4
        local.get 3
        i32.store8 offset=47
        local.get 4
        local.get 4
        i32.const 36
        i32.add
        i32.store offset=20
        local.get 4
        local.get 4
        i32.const 47
        i32.add
        i32.store offset=36
        local.get 0
        local.get 1
        local.get 4
        i32.const 12
        i32.add
        local.get 6
        call_indirect (type 1)
        block ;; label = @2
          local.get 7
          i32.eqz
          br_if 0 (;@2;)
          global.get $__memory_base
          i32.const 4776
          i32.add
          i32.load
          i32.const 2147483647
          i32.and
          i32.eqz
          br_if 0 (;@2;)
          call $std::panicking::panic_count::is_zero_slow_path::h30fc47034d17437a
          br_if 0 (;@2;)
          global.get $__memory_base
          i32.const 4800
          i32.add
          i32.const 1
          i32.store8 offset=1
        end
        global.get $__memory_base
        i32.const 4800
        i32.add
        i32.const 0
        i32.store8
        local.get 4
        i32.const 48
        i32.add
        global.set $__stack_pointer
        return
      end
      local.get 4
      i64.const 0
      i64.store offset=24 align=4
      local.get 4
      i32.const 1
      i32.store offset=16
      local.get 4
      global.get $__memory_base
      local.tee 6
      i32.const 1644
      i32.add
      i32.store offset=20
      local.get 4
      local.get 6
      i32.const 4224
      i32.add
      i32.store offset=12
      i32.const 0
      local.get 4
      i32.const 36
      i32.add
      local.get 6
      i32.const 1935
      i32.add
      local.get 4
      i32.const 12
      i32.add
      local.get 6
      i32.const 4232
      i32.add
      call $core::panicking::assert_failed::h2389905535db148b
      unreachable
    )
    (func $_$LT$std..sys_common..backtrace.._print..DisplayBacktrace$u20$as$u20$core..fmt..Display$GT$::fmt::h048cf0e1272391fd (;70;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i64 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      local.get 0
      i32.load8_u
      local.set 3
      local.get 2
      i32.const 8
      i32.add
      call $std::sys::wasi::os::getcwd::h513006697dbc1ac1
      local.get 2
      i64.load offset=12 align=4
      local.set 4
      block ;; label = @1
        local.get 2
        i32.load offset=8
        local.tee 5
        i32.const -2147483648
        i32.ne
        br_if 0 (;@1;)
        local.get 4
        i64.const 255
        i64.and
        i64.const 3
        i64.ne
        br_if 0 (;@1;)
        local.get 4
        i64.const 32
        i64.shr_u
        i32.wrap_i64
        local.tee 0
        i32.load
        local.tee 6
        local.get 0
        i32.const 4
        i32.add
        i32.load
        local.tee 7
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 7
          i32.load offset=4
          local.tee 8
          i32.eqz
          br_if 0 (;@2;)
          local.get 6
          local.get 8
          local.get 7
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 0
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      local.get 2
      i32.const 20
      i32.add
      i64.const 0
      i64.store align=4
      i32.const 1
      local.set 0
      local.get 2
      i32.const 1
      i32.store offset=12
      local.get 2
      global.get $__memory_base
      local.tee 7
      i32.const 4104
      i32.add
      i32.store offset=8
      local.get 2
      local.get 7
      i32.const 1644
      i32.add
      i32.store offset=16
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            local.get 1
            local.get 2
            i32.const 8
            i32.add
            call $core::fmt::Formatter::write_fmt::hbd0ea213d4da4bc3
            br_if 0 (;@3;)
            block ;; label = @4
              local.get 3
              i32.const 255
              i32.and
              br_if 0 (;@4;)
              local.get 2
              i32.const 20
              i32.add
              i64.const 0
              i64.store align=4
              local.get 2
              i32.const 1
              i32.store offset=12
              local.get 2
              global.get $__memory_base
              local.tee 7
              i32.const 4112
              i32.add
              i32.store offset=8
              local.get 2
              local.get 7
              i32.const 1644
              i32.add
              i32.store offset=16
              local.get 1
              local.get 2
              i32.const 8
              i32.add
              call $core::fmt::Formatter::write_fmt::hbd0ea213d4da4bc3
              br_if 1 (;@3;)
            end
            i32.const 0
            local.set 0
            local.get 5
            i32.const -2147483648
            i32.or
            i32.const -2147483648
            i32.ne
            br_if 1 (;@2;)
            br 2 (;@1;)
          end
          local.get 5
          i32.const -2147483648
          i32.or
          i32.const -2147483648
          i32.eq
          br_if 1 (;@1;)
        end
        local.get 4
        i32.wrap_i64
        local.get 5
        i32.const 1
        call $__rust_dealloc
      end
      local.get 2
      i32.const 32
      i32.add
      global.set $__stack_pointer
      local.get 0
    )
    (func $std::sys_common::backtrace::__rust_end_short_backtrace::hcc5467269754a10b (;71;) (type 0) (param i32)
      local.get 0
      call $std::panicking::begin_panic_handler::_$u7b$$u7b$closure$u7d$$u7d$::h8e076e01b051ecab
      unreachable
    )
    (func $std::panicking::begin_panic_handler::_$u7b$$u7b$closure$u7d$$u7d$::h8e076e01b051ecab (;72;) (type 0) (param i32)
      (local i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 1
      global.set $__stack_pointer
      local.get 0
      i32.load
      local.tee 2
      i32.const 12
      i32.add
      i32.load
      local.set 3
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 2
              i32.load offset=4
              br_table 0 (;@4;) 1 (;@3;) 3 (;@1;)
            end
            global.get $__memory_base
            local.set 4
            local.get 3
            br_if 2 (;@1;)
            local.get 4
            i32.const 1644
            i32.add
            local.set 2
            i32.const 0
            local.set 3
            br 1 (;@2;)
          end
          local.get 3
          br_if 1 (;@1;)
          local.get 2
          i32.load
          local.tee 2
          i32.load offset=4
          local.set 3
          local.get 2
          i32.load
          local.set 2
        end
        local.get 1
        local.get 3
        i32.store offset=4
        local.get 1
        local.get 2
        i32.store
        local.get 1
        global.get $__memory_base
        i32.const 4184
        i32.add
        local.get 0
        i32.load offset=4
        local.tee 2
        i32.load offset=8
        local.get 0
        i32.load offset=8
        local.get 2
        i32.load8_u offset=16
        local.get 2
        i32.load8_u offset=17
        call $std::panicking::rust_panic_with_hook::hc6002f2b9824cb39
        unreachable
      end
      local.get 1
      local.get 2
      i32.store offset=12
      local.get 1
      i32.const -2147483648
      i32.store
      local.get 1
      global.get $__memory_base
      i32.const 4204
      i32.add
      local.get 0
      i32.load offset=4
      local.tee 2
      i32.load offset=8
      local.get 0
      i32.load offset=8
      local.get 2
      i32.load8_u offset=16
      local.get 2
      i32.load8_u offset=17
      call $std::panicking::rust_panic_with_hook::hc6002f2b9824cb39
      unreachable
    )
    (func $std::alloc::default_alloc_error_hook::h0be6e654dfab6bd4 (;73;) (type 2) (param i32 i32)
      (local i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 64
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        global.get $GOT.data.internal.__rust_alloc_error_handler_should_panic
        i32.load8_u
        br_if 0 (;@1;)
        local.get 2
        i32.const 24
        i32.add
        i64.const 1
        i64.store align=4
        local.get 2
        i32.const 2
        i32.store offset=16
        local.get 2
        global.get $GOT.func.internal.core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$::fmt::hbfd83f0499f79957
        i32.store offset=40
        local.get 2
        global.get $__memory_base
        local.tee 3
        i32.const 4120
        i32.add
        i32.store offset=12
        local.get 2
        local.get 1
        i32.store offset=44
        local.get 2
        local.get 2
        i32.const 36
        i32.add
        i32.store offset=20
        local.get 2
        local.get 2
        i32.const 44
        i32.add
        i32.store offset=36
        local.get 2
        i32.const 4
        i32.store8 offset=48
        local.get 2
        local.get 2
        i32.const 63
        i32.add
        i32.store offset=56
        local.get 2
        i32.const 48
        i32.add
        local.get 3
        i32.const 4036
        i32.add
        local.get 2
        i32.const 12
        i32.add
        call $core::fmt::write::h9a3c09758e76bf05
        local.set 3
        local.get 2
        i32.load8_u offset=48
        local.set 1
        block ;; label = @2
          block ;; label = @3
            local.get 3
            i32.eqz
            br_if 0 (;@3;)
            local.get 1
            i32.const 4
            i32.eq
            br_if 1 (;@2;)
            local.get 2
            i32.load offset=52
            local.set 3
            block ;; label = @4
              local.get 2
              i32.load8_u offset=48
              local.tee 1
              i32.const 4
              i32.gt_u
              br_if 0 (;@4;)
              local.get 1
              i32.const 3
              i32.ne
              br_if 2 (;@2;)
            end
            local.get 3
            i32.load
            local.tee 4
            local.get 3
            i32.const 4
            i32.add
            i32.load
            local.tee 1
            i32.load
            call_indirect (type 0)
            block ;; label = @4
              local.get 1
              i32.load offset=4
              local.tee 5
              i32.eqz
              br_if 0 (;@4;)
              local.get 4
              local.get 5
              local.get 1
              i32.load offset=8
              call $__rust_dealloc
            end
            local.get 3
            i32.const 12
            i32.const 4
            call $__rust_dealloc
            br 1 (;@2;)
          end
          local.get 2
          i32.load offset=52
          local.set 3
          block ;; label = @3
            local.get 1
            i32.const 4
            i32.gt_u
            br_if 0 (;@3;)
            local.get 1
            i32.const 3
            i32.ne
            br_if 1 (;@2;)
          end
          local.get 3
          i32.load
          local.tee 4
          local.get 3
          i32.const 4
          i32.add
          i32.load
          local.tee 1
          i32.load
          call_indirect (type 0)
          block ;; label = @3
            local.get 1
            i32.load offset=4
            local.tee 5
            i32.eqz
            br_if 0 (;@3;)
            local.get 4
            local.get 5
            local.get 1
            i32.load offset=8
            call $__rust_dealloc
          end
          local.get 3
          i32.const 12
          i32.const 4
          call $__rust_dealloc
        end
        local.get 2
        i32.const 64
        i32.add
        global.set $__stack_pointer
        return
      end
      local.get 2
      i32.const 24
      i32.add
      i64.const 1
      i64.store align=4
      local.get 2
      i32.const 2
      i32.store offset=16
      local.get 2
      global.get $GOT.func.internal.core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$::fmt::hbfd83f0499f79957
      i32.store offset=52
      local.get 2
      global.get $__memory_base
      local.tee 3
      i32.const 4136
      i32.add
      i32.store offset=12
      local.get 2
      local.get 1
      i32.store offset=36
      local.get 2
      local.get 2
      i32.const 48
      i32.add
      i32.store offset=20
      local.get 2
      local.get 2
      i32.const 36
      i32.add
      i32.store offset=48
      local.get 2
      i32.const 12
      i32.add
      local.get 3
      i32.const 4152
      i32.add
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $_$LT$std..panicking..begin_panic_handler..StaticStrPayload$u20$as$u20$core..panic..PanicPayload$GT$::get::h501156f013b749a6 (;74;) (type 2) (param i32 i32)
      local.get 0
      global.get $__memory_base
      i32.const 4168
      i32.add
      i32.store offset=4
      local.get 0
      local.get 1
      i32.store
    )
    (func $std::alloc::rust_oom::h286b032f57f63045 (;75;) (type 2) (param i32 i32)
      (local i32 i32)
      global.get $__table_base
      local.set 2
      local.get 0
      local.get 1
      global.get $__memory_base
      i32.const 4804
      i32.add
      i32.load
      local.tee 3
      local.get 2
      i32.const 19
      i32.add
      local.get 3
      select
      call_indirect (type 2)
      call $std::process::abort::h601f51430a891ccd
      unreachable
    )
    (func $__rg_oom (;76;) (type 2) (param i32 i32)
      local.get 1
      local.get 0
      call $std::alloc::rust_oom::h286b032f57f63045
      unreachable
    )
    (func $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h45e6f7a5d52f9492 (;77;) (type 2) (param i32 i32)
      local.get 0
      i64.const -6273870583586163964
      i64.store offset=8
      local.get 0
      i64.const -2479189555965818429
      i64.store
    )
    (func $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::h38d29057232e3175 (;78;) (type 4) (param i32 i32) (result i32)
      local.get 0
      i32.load
      local.get 1
      call $_$LT$bool$u20$as$u20$core..fmt..Display$GT$::fmt::h5c6bb3c83a350620
    )
    (func $_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$::fmt::h94e43c46ea1ae9fc (;79;) (type 4) (param i32 i32) (result i32)
      local.get 0
      i32.load
      local.get 0
      i32.load offset=4
      local.get 1
      call $_$LT$str$u20$as$u20$core..fmt..Display$GT$::fmt::h7a939288e0f0136c
    )
    (func $std::panicking::default_hook::h63833b397d84e308 (;80;) (type 0) (param i32)
      (local i32 i32 i32 i64 i64)
      global.get $__stack_pointer
      i32.const 112
      i32.sub
      local.tee 1
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i32.load8_u offset=17
          br_if 0 (;@2;)
          block ;; label = @3
            i32.const 0
            call $#func38<std::panicking::panic_count::LOCAL_PANIC_COUNT::__getit::hc13cf437609f1ede__.llvm.5565787320568768541_>
            i32.load
            i32.const 1
            i32.gt_u
            br_if 0 (;@3;)
            local.get 1
            call $std::panic::get_backtrace_style::h42fdd7217493e6d2
            i32.store8 offset=35
            br 2 (;@1;)
          end
          local.get 1
          i32.const 1
          i32.store8 offset=35
          br 1 (;@1;)
        end
        local.get 1
        i32.const 3
        i32.store8 offset=35
      end
      local.get 1
      local.get 0
      i32.load offset=12
      i32.store offset=36
      i32.const 12
      local.set 2
      local.get 1
      i32.const 16
      i32.add
      local.get 0
      i32.load
      local.tee 3
      local.get 0
      i32.load offset=4
      i32.const 12
      i32.add
      local.tee 0
      i32.load
      call_indirect (type 2)
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 1
              i64.load offset=16
              i64.const -4493808902380553279
              i64.xor
              local.get 1
              i32.const 16
              i32.add
              i32.const 8
              i32.add
              i64.load
              i64.const -163230743173927068
              i64.xor
              i64.or
              i64.eqz
              br_if 0 (;@4;)
              local.get 1
              local.get 3
              local.get 0
              i32.load
              call_indirect (type 2)
              local.get 1
              i32.const 8
              i32.add
              i64.load
              local.set 4
              local.get 1
              i64.load
              local.set 5
              global.get $__memory_base
              local.set 0
              local.get 5
              i64.const -1960977461435455207
              i64.xor
              local.get 4
              i64.const 4907860379700932464
              i64.xor
              i64.or
              i64.eqz
              br_if 1 (;@3;)
              local.get 0
              i32.const 2182
              i32.add
              local.set 0
              br 3 (;@1;)
            end
            local.get 3
            i32.const 4
            i32.add
            local.set 0
            br 1 (;@2;)
          end
          local.get 3
          i32.const 8
          i32.add
          local.set 0
          local.get 3
          i32.const 4
          i32.add
          local.set 3
        end
        local.get 0
        i32.load
        local.set 2
        local.get 3
        i32.load
        local.set 0
      end
      local.get 1
      local.get 2
      i32.store offset=44
      local.get 1
      local.get 0
      i32.store offset=40
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                block ;; label = @6
                  block ;; label = @7
                    global.get $__memory_base
                    i32.const 4792
                    i32.add
                    i32.load
                    local.tee 0
                    br_if 0 (;@7;)
                    global.get $__memory_base
                    local.set 3
                    call $#func48<core::cell::once::OnceCell$LT$T$GT$::get_or_try_init::outlined_call::hd352954db2dff7b1__.llvm.16956913488488451105_>
                    local.set 0
                    local.get 3
                    i32.const 4792
                    i32.add
                    i32.load
                    br_if 1 (;@6;)
                    global.get $__memory_base
                    i32.const 4792
                    i32.add
                    local.get 0
                    i32.store
                  end
                  local.get 0
                  local.get 0
                  i32.load
                  local.tee 3
                  i32.const 1
                  i32.add
                  i32.store
                  local.get 3
                  i32.const -1
                  i32.le_s
                  br_if 1 (;@5;)
                  global.get $__memory_base
                  local.set 3
                  local.get 1
                  local.get 0
                  i32.store offset=48
                  local.get 1
                  local.get 0
                  i32.const 16
                  i32.add
                  i32.load
                  local.tee 2
                  local.get 3
                  i32.const 2194
                  i32.add
                  local.get 2
                  select
                  i32.store offset=52
                  local.get 1
                  local.get 0
                  i32.const 20
                  i32.add
                  i32.load
                  i32.const -1
                  i32.add
                  i32.const 9
                  local.get 2
                  select
                  i32.store offset=56
                  local.get 3
                  i32.const 4758
                  i32.add
                  i32.load8_u
                  local.set 0
                  local.get 1
                  local.get 1
                  i32.const 35
                  i32.add
                  i32.store offset=72
                  local.get 1
                  local.get 1
                  i32.const 40
                  i32.add
                  i32.store offset=68
                  local.get 1
                  local.get 1
                  i32.const 36
                  i32.add
                  i32.store offset=64
                  local.get 1
                  local.get 1
                  i32.const 52
                  i32.add
                  i32.store offset=60
                  block ;; label = @7
                    local.get 0
                    br_if 0 (;@7;)
                    local.get 1
                    i32.const 0
                    i32.store offset=76
                    br 5 (;@2;)
                  end
                  global.get $__memory_base
                  i32.const 4758
                  i32.add
                  i32.const 1
                  i32.store8
                  i32.const 0
                  call $#func24<std::io::stdio::OUTPUT_CAPTURE::__getit::h13738c54a5c306a3__.llvm.9582321433186723839_>
                  local.tee 3
                  i32.eqz
                  br_if 2 (;@4;)
                  local.get 3
                  i32.load
                  local.set 0
                  local.get 3
                  i32.const 0
                  i32.store
                  local.get 1
                  local.get 0
                  i32.store offset=76
                  local.get 0
                  i32.eqz
                  br_if 4 (;@2;)
                  local.get 0
                  i32.load8_u offset=8
                  local.set 3
                  local.get 0
                  i32.const 1
                  i32.store8 offset=8
                  local.get 1
                  local.get 3
                  i32.store8 offset=80
                  local.get 3
                  br_if 3 (;@3;)
                  block ;; label = @7
                    block ;; label = @8
                      block ;; label = @9
                        global.get $__memory_base
                        i32.const 4776
                        i32.add
                        i32.load
                        i32.const 2147483647
                        i32.and
                        br_if 0 (;@9;)
                        local.get 1
                        i32.const 60
                        i32.add
                        local.get 0
                        i32.const 12
                        i32.add
                        global.get $__memory_base
                        i32.const 4248
                        i32.add
                        call $std::panicking::default_hook::_$u7b$$u7b$closure$u7d$$u7d$::h99c42c0a8943aa06
                        local.get 0
                        i32.const 9
                        i32.add
                        local.set 3
                        br 1 (;@8;)
                      end
                      global.get $__memory_base
                      local.set 3
                      call $std::panicking::panic_count::is_zero_slow_path::h30fc47034d17437a
                      local.set 2
                      local.get 1
                      i32.const 60
                      i32.add
                      local.get 0
                      i32.const 12
                      i32.add
                      local.get 3
                      i32.const 4248
                      i32.add
                      call $std::panicking::default_hook::_$u7b$$u7b$closure$u7d$$u7d$::h99c42c0a8943aa06
                      local.get 2
                      i32.eqz
                      br_if 1 (;@7;)
                      local.get 0
                      i32.const 9
                      i32.add
                      local.set 3
                    end
                    global.get $__memory_base
                    i32.const 4776
                    i32.add
                    i32.load
                    i32.const 2147483647
                    i32.and
                    i32.eqz
                    br_if 0 (;@7;)
                    call $std::panicking::panic_count::is_zero_slow_path::h30fc47034d17437a
                    br_if 0 (;@7;)
                    local.get 3
                    i32.const 1
                    i32.store8
                  end
                  local.get 0
                  i32.const 0
                  i32.store8 offset=8
                  global.get $__memory_base
                  i32.const 4758
                  i32.add
                  i32.const 1
                  i32.store8
                  local.get 1
                  local.get 0
                  i32.store offset=84
                  block ;; label = @7
                    i32.const 0
                    call $#func24<std::io::stdio::OUTPUT_CAPTURE::__getit::h13738c54a5c306a3__.llvm.9582321433186723839_>
                    local.tee 2
                    br_if 0 (;@7;)
                    local.get 0
                    local.get 0
                    i32.load
                    local.tee 3
                    i32.const -1
                    i32.add
                    i32.store
                    block ;; label = @8
                      local.get 3
                      i32.const 1
                      i32.ne
                      br_if 0 (;@8;)
                      local.get 1
                      i32.const 84
                      i32.add
                      call $alloc::sync::Arc$LT$T$C$A$GT$::drop_slow::ha2345d2e0183cb9b
                    end
                    global.get $__memory_base
                    local.tee 0
                    i32.const 962
                    i32.add
                    i32.const 70
                    local.get 1
                    i32.const 111
                    i32.add
                    local.get 0
                    i32.const 3920
                    i32.add
                    local.get 0
                    i32.const 3936
                    i32.add
                    call $core::result::unwrap_failed::h0a2a95058939daaf
                    unreachable
                  end
                  local.get 2
                  i32.load
                  local.set 3
                  local.get 2
                  local.get 0
                  i32.store
                  local.get 1
                  local.get 3
                  i32.store offset=80
                  block ;; label = @7
                    local.get 3
                    i32.eqz
                    br_if 0 (;@7;)
                    local.get 3
                    local.get 3
                    i32.load
                    local.tee 0
                    i32.const -1
                    i32.add
                    i32.store
                    local.get 0
                    i32.const 1
                    i32.ne
                    br_if 0 (;@7;)
                    local.get 1
                    i32.const 80
                    i32.add
                    call $alloc::sync::Arc$LT$T$C$A$GT$::drop_slow::ha2345d2e0183cb9b
                  end
                  i32.const 1
                  local.set 3
                  br 5 (;@1;)
                end
                local.get 1
                i32.const 96
                i32.add
                i64.const 0
                i64.store align=4
                local.get 1
                i32.const 1
                i32.store offset=88
                local.get 1
                global.get $__memory_base
                local.tee 0
                i32.const 3760
                i32.add
                i32.store offset=84
                local.get 1
                local.get 0
                i32.const 324
                i32.add
                i32.store offset=92
                local.get 1
                i32.const 84
                i32.add
                local.get 0
                i32.const 3768
                i32.add
                call $core::panicking::panic_fmt::hf3388a9334a7d4c4
                unreachable
              end
              unreachable
              unreachable
            end
            global.get $__memory_base
            local.tee 0
            i32.const 962
            i32.add
            i32.const 70
            local.get 1
            i32.const 111
            i32.add
            local.get 0
            i32.const 3920
            i32.add
            local.get 0
            i32.const 3936
            i32.add
            call $core::result::unwrap_failed::h0a2a95058939daaf
            unreachable
          end
          local.get 1
          i64.const 0
          i64.store offset=96 align=4
          local.get 1
          i32.const 1
          i32.store offset=88
          local.get 1
          global.get $__memory_base
          local.tee 0
          i32.const 2072
          i32.add
          i32.store offset=92
          local.get 1
          local.get 0
          i32.const 4508
          i32.add
          i32.store offset=84
          i32.const 0
          local.get 1
          i32.const 80
          i32.add
          local.get 0
          i32.const 2578
          i32.add
          local.get 1
          i32.const 84
          i32.add
          local.get 0
          i32.const 4516
          i32.add
          call $core::panicking::assert_failed::h2389905535db148b
          unreachable
        end
        local.get 1
        i32.const 60
        i32.add
        local.get 1
        i32.const 111
        i32.add
        global.get $__memory_base
        i32.const 4288
        i32.add
        call $std::panicking::default_hook::_$u7b$$u7b$closure$u7d$$u7d$::h99c42c0a8943aa06
        i32.const 0
        local.set 3
      end
      block ;; label = @1
        local.get 1
        i32.load offset=48
        local.tee 0
        i32.eqz
        br_if 0 (;@1;)
        local.get 0
        local.get 0
        i32.load
        local.tee 2
        i32.const -1
        i32.add
        i32.store
        local.get 2
        i32.const 1
        i32.ne
        br_if 0 (;@1;)
        local.get 1
        i32.const 48
        i32.add
        call $alloc::sync::Arc$LT$T$C$A$GT$::drop_slow::hcedfc111557afb49
      end
      block ;; label = @1
        local.get 3
        i32.const -1
        i32.xor
        local.get 1
        i32.load offset=76
        local.tee 0
        i32.const 0
        i32.ne
        i32.and
        i32.eqz
        br_if 0 (;@1;)
        local.get 0
        local.get 0
        i32.load
        local.tee 3
        i32.const -1
        i32.add
        i32.store
        local.get 3
        i32.const 1
        i32.ne
        br_if 0 (;@1;)
        local.get 1
        i32.const 76
        i32.add
        call $alloc::sync::Arc$LT$T$C$A$GT$::drop_slow::ha2345d2e0183cb9b
      end
      local.get 1
      i32.const 112
      i32.add
      global.set $__stack_pointer
    )
    (func $core::ptr::drop_in_place$LT$i32$GT$::h7b5f74dcf93888ba (;81;) (type 0) (param i32))
    (func $core::ptr::drop_in_place$LT$$LP$$RP$$GT$::hc7983eb888352f97 (;82;) (type 0) (param i32))
    (func $core::ptr::drop_in_place$LT$alloc..string..String$GT$::h7e859c3d49663973 (;83;) (type 0) (param i32)
      (local i32)
      block ;; label = @1
        local.get 0
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@1;)
        local.get 0
        i32.load offset=4
        local.get 1
        i32.const 1
        call $__rust_dealloc
      end
    )
    (func $core::ptr::drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$::h10151d6d1c228eea (;84;) (type 2) (param i32 i32)
      (local i32 i32)
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i32.const 255
          i32.and
          local.tee 0
          i32.const 4
          i32.gt_u
          br_if 0 (;@2;)
          local.get 0
          i32.const 3
          i32.ne
          br_if 1 (;@1;)
        end
        local.get 1
        i32.load
        local.tee 2
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.tee 0
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 0
          i32.load offset=4
          local.tee 3
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          local.get 3
          local.get 0
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
    )
    (func $#func85<_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_char::hbccd3dd660ab8f35> (@name "_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_char::hbccd3dd660ab8f35") (;85;) (type 4) (param i32 i32) (result i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 1
              i32.const 128
              i32.lt_u
              br_if 0 (;@4;)
              local.get 2
              i32.const 0
              i32.store offset=12
              local.get 1
              i32.const 2048
              i32.lt_u
              br_if 1 (;@3;)
              block ;; label = @5
                local.get 1
                i32.const 65536
                i32.ge_u
                br_if 0 (;@5;)
                local.get 2
                local.get 1
                i32.const 63
                i32.and
                i32.const 128
                i32.or
                i32.store8 offset=14
                local.get 2
                local.get 1
                i32.const 12
                i32.shr_u
                i32.const 224
                i32.or
                i32.store8 offset=12
                local.get 2
                local.get 1
                i32.const 6
                i32.shr_u
                i32.const 63
                i32.and
                i32.const 128
                i32.or
                i32.store8 offset=13
                i32.const 3
                local.set 1
                br 3 (;@2;)
              end
              local.get 2
              local.get 1
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=15
              local.get 2
              local.get 1
              i32.const 6
              i32.shr_u
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=14
              local.get 2
              local.get 1
              i32.const 12
              i32.shr_u
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=13
              local.get 2
              local.get 1
              i32.const 18
              i32.shr_u
              i32.const 7
              i32.and
              i32.const 240
              i32.or
              i32.store8 offset=12
              i32.const 4
              local.set 1
              br 2 (;@2;)
            end
            block ;; label = @4
              local.get 0
              i32.load offset=8
              local.tee 3
              local.get 0
              i32.load
              i32.ne
              br_if 0 (;@4;)
              local.get 0
              local.get 3
              call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve_for_push::h6b611529b178451b
              local.get 0
              i32.load offset=8
              local.set 3
            end
            local.get 0
            i32.load offset=4
            local.get 3
            i32.add
            local.get 1
            i32.store8
            local.get 0
            local.get 0
            i32.load offset=8
            i32.const 1
            i32.add
            i32.store offset=8
            br 2 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 192
          i32.or
          i32.store8 offset=12
          i32.const 2
          local.set 1
        end
        block ;; label = @2
          local.get 0
          i32.load
          local.get 0
          i32.load offset=8
          local.tee 3
          i32.sub
          local.get 1
          i32.ge_u
          br_if 0 (;@2;)
          local.get 0
          local.get 3
          local.get 1
          call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
          local.get 0
          i32.load offset=8
          local.set 3
        end
        local.get 0
        i32.load offset=4
        local.get 3
        i32.add
        local.get 2
        i32.const 12
        i32.add
        local.get 1
        call $memcpy
        drop
        local.get 0
        local.get 3
        local.get 1
        i32.add
        i32.store offset=8
      end
      local.get 2
      i32.const 16
      i32.add
      global.set $__stack_pointer
      i32.const 0
    )
    (func $#func86<_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_str::ha232be781f4501bb> (@name "_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_str::ha232be781f4501bb") (;86;) (type 3) (param i32 i32 i32) (result i32)
      (local i32)
      block ;; label = @1
        local.get 0
        i32.load
        local.get 0
        i32.load offset=8
        local.tee 3
        i32.sub
        local.get 2
        i32.ge_u
        br_if 0 (;@1;)
        local.get 0
        local.get 3
        local.get 2
        call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
        local.get 0
        i32.load offset=8
        local.set 3
      end
      local.get 0
      i32.load offset=4
      local.get 3
      i32.add
      local.get 1
      local.get 2
      call $memcpy
      drop
      local.get 0
      local.get 3
      local.get 2
      i32.add
      i32.store offset=8
      i32.const 0
    )
    (func $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::write::h56227fb4b7b7274d (;87;) (type 10) (param i32 i32 i32 i32)
      (local i32)
      block ;; label = @1
        local.get 1
        i32.load
        local.get 1
        i32.load offset=8
        local.tee 4
        i32.sub
        local.get 3
        i32.ge_u
        br_if 0 (;@1;)
        local.get 1
        local.get 4
        local.get 3
        call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
        local.get 1
        i32.load offset=8
        local.set 4
      end
      local.get 1
      i32.load offset=4
      local.get 4
      i32.add
      local.get 2
      local.get 3
      call $memcpy
      drop
      local.get 0
      local.get 3
      i32.store offset=4
      local.get 1
      local.get 4
      local.get 3
      i32.add
      i32.store offset=8
      local.get 0
      i32.const 4
      i32.store8
    )
    (func $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::write_vectored::hb080017ea512e2c8 (;88;) (type 10) (param i32 i32 i32 i32)
      (local i32 i32 i32 i32 i32)
      block ;; label = @1
        block ;; label = @2
          local.get 3
          br_if 0 (;@2;)
          i32.const 0
          local.set 4
          br 1 (;@1;)
        end
        local.get 3
        i32.const 3
        i32.and
        local.set 5
        block ;; label = @2
          block ;; label = @3
            local.get 3
            i32.const 4
            i32.ge_u
            br_if 0 (;@3;)
            i32.const 0
            local.set 4
            i32.const 0
            local.set 6
            br 1 (;@2;)
          end
          local.get 2
          i32.const 28
          i32.add
          local.set 7
          local.get 3
          i32.const -4
          i32.and
          local.set 8
          i32.const 0
          local.set 4
          i32.const 0
          local.set 6
          loop ;; label = @3
            local.get 7
            i32.load
            local.get 7
            i32.const -8
            i32.add
            i32.load
            local.get 7
            i32.const -16
            i32.add
            i32.load
            local.get 7
            i32.const -24
            i32.add
            i32.load
            local.get 4
            i32.add
            i32.add
            i32.add
            i32.add
            local.set 4
            local.get 7
            i32.const 32
            i32.add
            local.set 7
            local.get 8
            local.get 6
            i32.const 4
            i32.add
            local.tee 6
            i32.ne
            br_if 0 (;@3;)
          end
        end
        block ;; label = @2
          local.get 5
          i32.eqz
          br_if 0 (;@2;)
          local.get 6
          i32.const 3
          i32.shl
          local.get 2
          i32.add
          i32.const 4
          i32.add
          local.set 7
          loop ;; label = @3
            local.get 7
            i32.load
            local.get 4
            i32.add
            local.set 4
            local.get 7
            i32.const 8
            i32.add
            local.set 7
            local.get 5
            i32.const -1
            i32.add
            local.tee 5
            br_if 0 (;@3;)
          end
        end
        local.get 3
        i32.const 3
        i32.shl
        local.set 5
        block ;; label = @2
          local.get 1
          i32.load
          local.get 1
          i32.load offset=8
          local.tee 7
          i32.sub
          local.get 4
          i32.ge_u
          br_if 0 (;@2;)
          local.get 1
          local.get 7
          local.get 4
          call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
          local.get 1
          i32.load offset=8
          local.set 7
        end
        local.get 2
        local.get 5
        i32.add
        local.set 8
        loop ;; label = @2
          local.get 2
          i32.load
          local.set 6
          block ;; label = @3
            local.get 1
            i32.load
            local.get 7
            i32.sub
            local.get 2
            i32.load offset=4
            local.tee 5
            i32.ge_u
            br_if 0 (;@3;)
            local.get 1
            local.get 7
            local.get 5
            call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
            local.get 1
            i32.load offset=8
            local.set 7
          end
          local.get 1
          i32.load offset=4
          local.get 7
          i32.add
          local.get 6
          local.get 5
          call $memcpy
          drop
          local.get 1
          local.get 7
          local.get 5
          i32.add
          local.tee 7
          i32.store offset=8
          local.get 2
          i32.const 8
          i32.add
          local.tee 2
          local.get 8
          i32.ne
          br_if 0 (;@2;)
        end
      end
      local.get 0
      i32.const 4
      i32.store8
      local.get 0
      local.get 4
      i32.store offset=4
    )
    (func $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::is_write_vectored::h47ca120c93aed961 (;89;) (type 5) (param i32) (result i32)
      i32.const 1
    )
    (func $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::write_all::h2828d09c0a196513 (;90;) (type 10) (param i32 i32 i32 i32)
      (local i32)
      block ;; label = @1
        local.get 1
        i32.load
        local.get 1
        i32.load offset=8
        local.tee 4
        i32.sub
        local.get 3
        i32.ge_u
        br_if 0 (;@1;)
        local.get 1
        local.get 4
        local.get 3
        call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4
        local.get 1
        i32.load offset=8
        local.set 4
      end
      local.get 1
      i32.load offset=4
      local.get 4
      i32.add
      local.get 2
      local.get 3
      call $memcpy
      drop
      local.get 0
      i32.const 4
      i32.store8
      local.get 1
      local.get 4
      local.get 3
      i32.add
      i32.store offset=8
    )
    (func $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::flush::h87a3932c46cb40cd (;91;) (type 2) (param i32 i32)
      local.get 0
      i32.const 4
      i32.store8
    )
    (func $std::panicking::default_hook::_$u7b$$u7b$closure$u7d$$u7d$::h99c42c0a8943aa06 (;92;) (type 1) (param i32 i32 i32)
      (local i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 64
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      local.get 3
      i32.const 16
      i32.add
      i32.const 12
      i32.add
      i64.const 3
      i64.store align=4
      local.get 3
      i32.const 60
      i32.add
      global.get $__table_base
      local.tee 4
      i32.const 27
      i32.add
      local.tee 5
      i32.store
      local.get 3
      i32.const 40
      i32.add
      i32.const 12
      i32.add
      local.get 4
      i32.const 28
      i32.add
      i32.store
      local.get 3
      global.get $__memory_base
      i32.const 4328
      i32.add
      i32.store offset=16
      local.get 3
      local.get 5
      i32.store offset=44
      local.get 3
      local.get 0
      i32.load offset=8
      i32.store offset=56
      local.get 3
      local.get 0
      i32.load offset=4
      i32.store offset=48
      local.get 3
      local.get 0
      i32.load
      i32.store offset=40
      local.get 3
      local.get 3
      i32.const 40
      i32.add
      i32.store offset=24
      local.get 3
      i32.const 4
      i32.store offset=20
      local.get 3
      i32.const 8
      i32.add
      local.get 1
      local.get 3
      i32.const 16
      i32.add
      local.get 2
      i32.load offset=36
      local.tee 6
      call_indirect (type 1)
      local.get 3
      i32.load offset=12
      local.set 5
      block ;; label = @1
        block ;; label = @2
          local.get 3
          i32.load8_u offset=8
          local.tee 4
          i32.const 4
          i32.gt_u
          br_if 0 (;@2;)
          local.get 4
          i32.const 3
          i32.ne
          br_if 1 (;@1;)
        end
        local.get 5
        i32.load
        local.tee 7
        local.get 5
        i32.const 4
        i32.add
        i32.load
        local.tee 4
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 4
          i32.load offset=4
          local.tee 8
          i32.eqz
          br_if 0 (;@2;)
          local.get 7
          local.get 8
          local.get 4
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 5
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      block ;; label = @1
        local.get 0
        i32.load offset=12
        i32.load8_u
        local.tee 0
        i32.const 3
        i32.eq
        br_if 0 (;@1;)
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 0
              br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 0 (;@4;)
            end
            local.get 3
            i32.const 40
            i32.add
            local.get 1
            local.get 2
            i32.const 0
            call $std::sys_common::backtrace::print::h54d309d45860f40c
            local.get 3
            i32.load offset=44
            local.set 1
            block ;; label = @4
              local.get 3
              i32.load8_u offset=40
              local.tee 0
              i32.const 4
              i32.gt_u
              br_if 0 (;@4;)
              local.get 0
              i32.const 3
              i32.ne
              br_if 3 (;@1;)
            end
            local.get 1
            i32.load
            local.tee 2
            local.get 1
            i32.const 4
            i32.add
            i32.load
            local.tee 0
            i32.load
            call_indirect (type 0)
            block ;; label = @4
              local.get 0
              i32.load offset=4
              local.tee 4
              i32.eqz
              br_if 0 (;@4;)
              local.get 2
              local.get 4
              local.get 0
              i32.load offset=8
              call $__rust_dealloc
            end
            local.get 1
            i32.const 12
            i32.const 4
            call $__rust_dealloc
            br 2 (;@1;)
          end
          local.get 3
          i32.const 40
          i32.add
          local.get 1
          local.get 2
          i32.const 1
          call $std::sys_common::backtrace::print::h54d309d45860f40c
          local.get 3
          i32.load offset=44
          local.set 1
          block ;; label = @3
            local.get 3
            i32.load8_u offset=40
            local.tee 0
            i32.const 4
            i32.gt_u
            br_if 0 (;@3;)
            local.get 0
            i32.const 3
            i32.ne
            br_if 2 (;@1;)
          end
          local.get 1
          i32.load
          local.tee 2
          local.get 1
          i32.const 4
          i32.add
          i32.load
          local.tee 0
          i32.load
          call_indirect (type 0)
          block ;; label = @3
            local.get 0
            i32.load offset=4
            local.tee 4
            i32.eqz
            br_if 0 (;@3;)
            local.get 2
            local.get 4
            local.get 0
            i32.load offset=8
            call $__rust_dealloc
          end
          local.get 1
          i32.const 12
          i32.const 4
          call $__rust_dealloc
          br 1 (;@1;)
        end
        global.get $__memory_base
        i32.const 4368
        i32.add
        local.tee 0
        i32.load8_u
        local.set 2
        local.get 0
        i32.const 0
        i32.store8
        local.get 2
        i32.eqz
        br_if 0 (;@1;)
        local.get 3
        i32.const 52
        i32.add
        i64.const 0
        i64.store align=4
        local.get 3
        i32.const 1
        i32.store offset=44
        local.get 3
        global.get $__memory_base
        local.tee 0
        i32.const 4360
        i32.add
        i32.store offset=40
        local.get 3
        local.get 0
        i32.const 2072
        i32.add
        i32.store offset=48
        local.get 3
        i32.const 16
        i32.add
        local.get 1
        local.get 3
        i32.const 40
        i32.add
        local.get 6
        call_indirect (type 1)
        local.get 3
        i32.load offset=20
        local.set 1
        block ;; label = @2
          local.get 3
          i32.load8_u offset=16
          local.tee 0
          i32.const 4
          i32.gt_u
          br_if 0 (;@2;)
          local.get 0
          i32.const 3
          i32.ne
          br_if 1 (;@1;)
        end
        local.get 1
        i32.load
        local.tee 2
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.tee 0
        i32.load
        call_indirect (type 0)
        block ;; label = @2
          local.get 0
          i32.load offset=4
          local.tee 4
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          local.get 4
          local.get 0
          i32.load offset=8
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      local.get 3
      i32.const 64
      i32.add
      global.set $__stack_pointer
    )
    (func $rust_begin_unwind (;93;) (type 0) (param i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 1
      global.set $__stack_pointer
      block ;; label = @1
        local.get 0
        i32.load offset=8
        local.tee 2
        br_if 0 (;@1;)
        global.get $__memory_base
        local.tee 0
        i32.const 2306
        i32.add
        i32.const 43
        local.get 0
        i32.const 4372
        i32.add
        call $core::panicking::panic::h6b5f2399fab9b82b
        unreachable
      end
      local.get 1
      local.get 0
      i32.load offset=12
      i32.store offset=12
      local.get 1
      local.get 0
      i32.store offset=8
      local.get 1
      local.get 2
      i32.store offset=4
      local.get 1
      i32.const 4
      i32.add
      call $std::sys_common::backtrace::__rust_end_short_backtrace::hcc5467269754a10b
      unreachable
    )
    (func $_$LT$std..panicking..begin_panic_handler..FormatStringPayload$u20$as$u20$core..panic..PanicPayload$GT$::take_box::h6075c9dae37cc887 (;94;) (type 2) (param i32 i32)
      (local i32 i32 i32 i64)
      global.get $__stack_pointer
      i32.const 48
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        local.get 1
        i32.load
        i32.const -2147483648
        i32.ne
        br_if 0 (;@1;)
        local.get 1
        i32.load offset=12
        local.set 3
        local.get 2
        i32.const 36
        i32.add
        i32.const 8
        i32.add
        local.tee 4
        i32.const 0
        i32.store
        local.get 2
        i64.const 4294967296
        i64.store offset=36 align=4
        local.get 2
        i32.const 36
        i32.add
        global.get $__memory_base
        i32.const 4388
        i32.add
        local.get 3
        call $core::fmt::write::h9a3c09758e76bf05
        drop
        local.get 2
        i32.const 24
        i32.add
        i32.const 8
        i32.add
        local.get 4
        i32.load
        local.tee 3
        i32.store
        local.get 2
        local.get 2
        i64.load offset=36 align=4
        local.tee 5
        i64.store offset=24
        local.get 1
        i32.const 8
        i32.add
        local.get 3
        i32.store
        local.get 1
        local.get 5
        i64.store align=4
      end
      local.get 1
      i64.load align=4
      local.set 5
      local.get 1
      i64.const 4294967296
      i64.store align=4
      local.get 2
      i32.const 8
      i32.add
      i32.const 8
      i32.add
      local.tee 3
      local.get 1
      i32.const 8
      i32.add
      local.tee 1
      i32.load
      i32.store
      local.get 1
      i32.const 0
      i32.store
      global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
      i32.load8_u
      drop
      local.get 2
      local.get 5
      i64.store offset=8
      block ;; label = @1
        i32.const 12
        i32.const 4
        call $__rust_alloc
        local.tee 1
        br_if 0 (;@1;)
        i32.const 4
        i32.const 12
        call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
        unreachable
      end
      local.get 1
      local.get 2
      i64.load offset=8
      i64.store align=4
      local.get 1
      i32.const 8
      i32.add
      local.get 3
      i32.load
      i32.store
      local.get 0
      global.get $__memory_base
      i32.const 4412
      i32.add
      i32.store offset=4
      local.get 0
      local.get 1
      i32.store
      local.get 2
      i32.const 48
      i32.add
      global.set $__stack_pointer
    )
    (func $_$LT$std..panicking..begin_panic_handler..FormatStringPayload$u20$as$u20$core..panic..PanicPayload$GT$::get::h8593919de4f10490 (;95;) (type 2) (param i32 i32)
      (local i32 i32 i32 i64)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        local.get 1
        i32.load
        i32.const -2147483648
        i32.ne
        br_if 0 (;@1;)
        local.get 1
        i32.load offset=12
        local.set 3
        local.get 2
        i32.const 20
        i32.add
        i32.const 8
        i32.add
        local.tee 4
        i32.const 0
        i32.store
        local.get 2
        i64.const 4294967296
        i64.store offset=20 align=4
        local.get 2
        i32.const 20
        i32.add
        global.get $__memory_base
        i32.const 4388
        i32.add
        local.get 3
        call $core::fmt::write::h9a3c09758e76bf05
        drop
        local.get 2
        i32.const 8
        i32.add
        i32.const 8
        i32.add
        local.get 4
        i32.load
        local.tee 3
        i32.store
        local.get 2
        local.get 2
        i64.load offset=20 align=4
        local.tee 5
        i64.store offset=8
        local.get 1
        i32.const 8
        i32.add
        local.get 3
        i32.store
        local.get 1
        local.get 5
        i64.store align=4
      end
      local.get 0
      local.get 1
      i32.store
      local.get 0
      global.get $__memory_base
      i32.const 4412
      i32.add
      i32.store offset=4
      local.get 2
      i32.const 32
      i32.add
      global.set $__stack_pointer
    )
    (func $_$LT$std..panicking..begin_panic_handler..StaticStrPayload$u20$as$u20$core..panic..PanicPayload$GT$::take_box::hb5f30bcde0dfc478 (;96;) (type 2) (param i32 i32)
      (local i32 i32)
      global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
      i32.load8_u
      drop
      local.get 1
      i32.load offset=4
      local.set 2
      local.get 1
      i32.load
      local.set 3
      block ;; label = @1
        i32.const 8
        i32.const 4
        call $__rust_alloc
        local.tee 1
        br_if 0 (;@1;)
        i32.const 4
        i32.const 8
        call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
        unreachable
      end
      local.get 1
      local.get 2
      i32.store offset=4
      local.get 1
      local.get 3
      i32.store
      local.get 0
      global.get $__memory_base
      i32.const 4428
      i32.add
      i32.store offset=4
      local.get 0
      local.get 1
      i32.store
    )
    (func $std::panicking::rust_panic_with_hook::hc6002f2b9824cb39 (;97;) (type 11) (param i32 i32 i32 i32 i32 i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 80
      i32.sub
      local.tee 6
      global.set $__stack_pointer
      global.get $__memory_base
      i32.const 4776
      i32.add
      local.tee 7
      local.get 7
      i32.load
      local.tee 7
      i32.const 1
      i32.add
      i32.store
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                block ;; label = @6
                  block ;; label = @7
                    local.get 7
                    i32.const 0
                    i32.lt_s
                    br_if 0 (;@7;)
                    i32.const 0
                    call $#func38<std::panicking::panic_count::LOCAL_PANIC_COUNT::__getit::hc13cf437609f1ede__.llvm.5565787320568768541_>
                    local.tee 7
                    i32.load8_u offset=4
                    br_if 1 (;@6;)
                    local.get 7
                    i32.const 1
                    i32.store8 offset=4
                    local.get 7
                    local.get 7
                    i32.load
                    i32.const 1
                    i32.add
                    i32.store
                    local.get 6
                    local.get 5
                    i32.store8 offset=33
                    local.get 6
                    local.get 4
                    i32.store8 offset=32
                    local.get 6
                    local.get 3
                    i32.store offset=28
                    local.get 6
                    local.get 2
                    i32.store offset=24
                    global.get $__memory_base
                    i32.const 4808
                    i32.add
                    i32.load
                    local.tee 5
                    i32.const -1
                    i32.le_s
                    br_if 5 (;@2;)
                    global.get $__memory_base
                    i32.const 4808
                    i32.add
                    local.tee 3
                    local.get 5
                    i32.const 1
                    i32.add
                    i32.store
                    local.get 3
                    i32.load offset=8
                    local.set 5
                    local.get 6
                    local.get 0
                    local.get 1
                    i32.load offset=16
                    call_indirect (type 2)
                    local.get 6
                    local.get 6
                    i64.load
                    i64.store offset=16 align=4
                    local.get 5
                    i32.eqz
                    br_if 3 (;@4;)
                    global.get $__memory_base
                    i32.const 4808
                    i32.add
                    local.tee 5
                    i32.load offset=8
                    local.get 6
                    i32.const 16
                    i32.add
                    local.get 5
                    i32.load offset=12
                    i32.load offset=20
                    call_indirect (type 2)
                    br 4 (;@3;)
                  end
                  local.get 6
                  local.get 5
                  i32.store8 offset=33
                  local.get 6
                  local.get 4
                  i32.store8 offset=32
                  local.get 6
                  local.get 3
                  i32.store offset=28
                  local.get 6
                  local.get 2
                  i32.store offset=24
                  local.get 6
                  global.get $__memory_base
                  local.tee 7
                  i32.const 4444
                  i32.add
                  i32.store offset=20
                  local.get 6
                  local.get 7
                  i32.const 2072
                  i32.add
                  i32.store offset=16
                  local.get 6
                  i32.const 52
                  i32.add
                  i64.const 1
                  i64.store align=4
                  local.get 6
                  i32.const 2
                  i32.store offset=44
                  local.get 6
                  local.get 7
                  i32.const 4460
                  i32.add
                  i32.store offset=40
                  local.get 6
                  global.get $GOT.func.internal._$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$::fmt::haaf6d012a733bd8d
                  i32.store offset=12
                  local.get 6
                  local.get 6
                  i32.const 8
                  i32.add
                  i32.store offset=48
                  local.get 6
                  local.get 6
                  i32.const 16
                  i32.add
                  i32.store offset=8
                  local.get 6
                  i32.const 4
                  i32.store8 offset=64
                  local.get 6
                  local.get 6
                  i32.const 8
                  i32.add
                  i32.store offset=72
                  local.get 6
                  i32.const 64
                  i32.add
                  local.get 7
                  i32.const 4036
                  i32.add
                  local.get 6
                  i32.const 40
                  i32.add
                  call $core::fmt::write::h9a3c09758e76bf05
                  local.set 4
                  local.get 6
                  i32.load8_u offset=64
                  local.set 7
                  block ;; label = @7
                    local.get 4
                    i32.eqz
                    br_if 0 (;@7;)
                    local.get 7
                    i32.const 4
                    i32.eq
                    br_if 2 (;@5;)
                    local.get 6
                    i32.load offset=68
                    local.set 7
                    block ;; label = @8
                      local.get 6
                      i32.load8_u offset=64
                      local.tee 6
                      i32.const 4
                      i32.gt_u
                      br_if 0 (;@8;)
                      local.get 6
                      i32.const 3
                      i32.ne
                      br_if 3 (;@5;)
                    end
                    local.get 7
                    i32.load
                    local.tee 4
                    local.get 7
                    i32.const 4
                    i32.add
                    i32.load
                    local.tee 6
                    i32.load
                    call_indirect (type 0)
                    block ;; label = @8
                      local.get 6
                      i32.load offset=4
                      local.tee 5
                      i32.eqz
                      br_if 0 (;@8;)
                      local.get 4
                      local.get 5
                      local.get 6
                      i32.load offset=8
                      call $__rust_dealloc
                    end
                    local.get 7
                    i32.const 12
                    i32.const 4
                    call $__rust_dealloc
                    call $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7
                    unreachable
                  end
                  local.get 6
                  i32.load offset=68
                  local.set 6
                  block ;; label = @7
                    local.get 7
                    i32.const 4
                    i32.gt_u
                    br_if 0 (;@7;)
                    local.get 7
                    i32.const 3
                    i32.ne
                    br_if 2 (;@5;)
                  end
                  local.get 6
                  i32.load
                  local.tee 4
                  local.get 6
                  i32.const 4
                  i32.add
                  i32.load
                  local.tee 7
                  i32.load
                  call_indirect (type 0)
                  block ;; label = @7
                    local.get 7
                    i32.load offset=4
                    local.tee 5
                    i32.eqz
                    br_if 0 (;@7;)
                    local.get 4
                    local.get 5
                    local.get 7
                    i32.load offset=8
                    call $__rust_dealloc
                  end
                  local.get 6
                  i32.const 12
                  i32.const 4
                  call $__rust_dealloc
                  call $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7
                  unreachable
                end
                local.get 6
                i32.const 52
                i32.add
                i64.const 0
                i64.store align=4
                local.get 6
                i32.const 1
                i32.store offset=44
                local.get 6
                global.get $__memory_base
                local.tee 7
                i32.const 4476
                i32.add
                i32.store offset=40
                local.get 6
                local.get 7
                i32.const 2072
                i32.add
                i32.store offset=48
                local.get 6
                i32.const 4
                i32.store8 offset=16
                local.get 6
                local.get 6
                i32.const 8
                i32.add
                i32.store offset=24
                local.get 6
                i32.const 16
                i32.add
                local.get 7
                i32.const 4036
                i32.add
                local.get 6
                i32.const 40
                i32.add
                call $core::fmt::write::h9a3c09758e76bf05
                local.set 4
                local.get 6
                i32.load8_u offset=16
                local.set 7
                block ;; label = @6
                  local.get 4
                  i32.eqz
                  br_if 0 (;@6;)
                  local.get 7
                  i32.const 4
                  i32.eq
                  br_if 1 (;@5;)
                  local.get 6
                  i32.load offset=20
                  local.set 7
                  block ;; label = @7
                    local.get 6
                    i32.load8_u offset=16
                    local.tee 6
                    i32.const 4
                    i32.gt_u
                    br_if 0 (;@7;)
                    local.get 6
                    i32.const 3
                    i32.ne
                    br_if 2 (;@5;)
                  end
                  local.get 7
                  i32.load
                  local.tee 4
                  local.get 7
                  i32.const 4
                  i32.add
                  i32.load
                  local.tee 6
                  i32.load
                  call_indirect (type 0)
                  block ;; label = @7
                    local.get 6
                    i32.load offset=4
                    local.tee 5
                    i32.eqz
                    br_if 0 (;@7;)
                    local.get 4
                    local.get 5
                    local.get 6
                    i32.load offset=8
                    call $__rust_dealloc
                  end
                  local.get 7
                  i32.const 12
                  i32.const 4
                  call $__rust_dealloc
                  call $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7
                  unreachable
                end
                local.get 6
                i32.load offset=20
                local.set 6
                block ;; label = @6
                  local.get 7
                  i32.const 4
                  i32.gt_u
                  br_if 0 (;@6;)
                  local.get 7
                  i32.const 3
                  i32.ne
                  br_if 1 (;@5;)
                end
                local.get 6
                i32.load
                local.tee 4
                local.get 6
                i32.const 4
                i32.add
                i32.load
                local.tee 7
                i32.load
                call_indirect (type 0)
                block ;; label = @6
                  local.get 7
                  i32.load offset=4
                  local.tee 5
                  i32.eqz
                  br_if 0 (;@6;)
                  local.get 4
                  local.get 5
                  local.get 7
                  i32.load offset=8
                  call $__rust_dealloc
                end
                local.get 6
                i32.const 12
                i32.const 4
                call $__rust_dealloc
              end
              call $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7
              unreachable
            end
            local.get 6
            i32.const 16
            i32.add
            call $std::panicking::default_hook::h63833b397d84e308
          end
          local.get 7
          i32.const 0
          i32.store8 offset=4
          global.get $__memory_base
          i32.const 4808
          i32.add
          local.tee 7
          local.get 7
          i32.load
          i32.const -1
          i32.add
          i32.store
          local.get 4
          i32.eqz
          br_if 1 (;@1;)
          local.get 0
          local.get 1
          call $rust_panic
          unreachable
        end
        local.get 6
        i32.const 52
        i32.add
        i64.const 0
        i64.store align=4
        local.get 6
        i32.const 1
        i32.store offset=44
        local.get 6
        global.get $__memory_base
        i32.const 4532
        i32.add
        i32.store offset=40
        local.get 6
        local.get 6
        i32.const 8
        i32.add
        i32.store offset=48
        local.get 6
        i32.const 64
        i32.add
        local.get 6
        i32.const 8
        i32.add
        local.get 6
        i32.const 40
        i32.add
        call $std::io::Write::write_fmt::h0b43c07e3174d5bc
        local.get 6
        i32.load8_u offset=64
        local.get 6
        i32.load offset=68
        call $core::ptr::drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$::h10151d6d1c228eea
        call $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7
        unreachable
      end
      local.get 6
      i32.const 52
      i32.add
      i64.const 0
      i64.store align=4
      local.get 6
      i32.const 1
      i32.store offset=44
      local.get 6
      global.get $__memory_base
      local.tee 7
      i32.const 4484
      i32.add
      i32.store offset=40
      local.get 6
      local.get 7
      i32.const 2072
      i32.add
      i32.store offset=48
      local.get 6
      i32.const 64
      i32.add
      local.get 6
      i32.const 8
      i32.add
      local.get 6
      i32.const 40
      i32.add
      call $std::io::Write::write_fmt::h0b43c07e3174d5bc
      local.get 6
      i32.load8_u offset=64
      local.get 6
      i32.load offset=68
      call $core::ptr::drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$::h10151d6d1c228eea
      call $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7
      unreachable
    )
    (func $rust_panic (;98;) (type 2) (param i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 48
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      local.get 2
      local.get 0
      local.get 1
      call $__rust_start_panic
      i32.store
      local.get 2
      i32.const 24
      i32.add
      i64.const 1
      i64.store align=4
      local.get 2
      i32.const 2
      i32.store offset=16
      local.get 2
      global.get $__memory_base
      i32.const 4492
      i32.add
      i32.store offset=12
      local.get 2
      global.get $GOT.func.internal.core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$::fmt::h98edb2ad46049cf7
      i32.store offset=40
      local.get 2
      local.get 2
      i32.const 36
      i32.add
      i32.store offset=20
      local.get 2
      local.get 2
      i32.store offset=36
      local.get 2
      i32.const 4
      i32.add
      local.get 2
      i32.const 47
      i32.add
      local.get 2
      i32.const 12
      i32.add
      call $std::io::Write::write_fmt::h0b43c07e3174d5bc
      local.get 2
      i32.load8_u offset=4
      local.get 2
      i32.load offset=8
      call $core::ptr::drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$::h10151d6d1c228eea
      call $std::sys::wasi::abort_internal::hf9e224bb89e3f2c7
      unreachable
    )
    (func $_$LT$std..sys..wasi..stdio..Stderr$u20$as$u20$std..io..Write$GT$::is_write_vectored::he4fe63897dfa8fc7 (;99;) (type 5) (param i32) (result i32)
      i32.const 1
    )
    (func $_$LT$std..sys..wasi..stdio..Stderr$u20$as$u20$std..io..Write$GT$::flush::h821a5942ed937229 (;100;) (type 2) (param i32 i32)
      local.get 0
      i32.const 4
      i32.store8
    )
    (func $#func101<alloc::raw_vec::finish_grow::h9d78a0ebeee4f730__.llvm.17953091742988396336_> (@name "alloc::raw_vec::finish_grow::h9d78a0ebeee4f730 (.llvm.17953091742988396336)") (;101;) (type 10) (param i32 i32 i32 i32)
      (local i32)
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            local.get 1
            i32.eqz
            br_if 0 (;@3;)
            local.get 2
            i32.const -1
            i32.le_s
            br_if 1 (;@2;)
            block ;; label = @4
              block ;; label = @5
                block ;; label = @6
                  local.get 3
                  i32.load offset=4
                  i32.eqz
                  br_if 0 (;@6;)
                  block ;; label = @7
                    local.get 3
                    i32.const 8
                    i32.add
                    i32.load
                    local.tee 4
                    br_if 0 (;@7;)
                    block ;; label = @8
                      local.get 2
                      br_if 0 (;@8;)
                      local.get 1
                      local.set 3
                      br 4 (;@4;)
                    end
                    global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
                    i32.load8_u
                    drop
                    br 2 (;@5;)
                  end
                  local.get 3
                  i32.load
                  local.get 4
                  local.get 1
                  local.get 2
                  call $__rust_realloc
                  local.set 3
                  br 2 (;@4;)
                end
                block ;; label = @6
                  local.get 2
                  br_if 0 (;@6;)
                  local.get 1
                  local.set 3
                  br 2 (;@4;)
                end
                global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
                i32.load8_u
                drop
              end
              local.get 2
              local.get 1
              call $__rust_alloc
              local.set 3
            end
            block ;; label = @4
              local.get 3
              i32.eqz
              br_if 0 (;@4;)
              local.get 0
              local.get 3
              i32.store offset=4
              local.get 0
              i32.const 8
              i32.add
              local.get 2
              i32.store
              local.get 0
              i32.const 0
              i32.store
              return
            end
            local.get 0
            local.get 1
            i32.store offset=4
            local.get 0
            i32.const 8
            i32.add
            local.get 2
            i32.store
            br 2 (;@1;)
          end
          local.get 0
          i32.const 0
          i32.store offset=4
          local.get 0
          i32.const 8
          i32.add
          local.get 2
          i32.store
          br 1 (;@1;)
        end
        local.get 0
        i32.const 0
        i32.store offset=4
      end
      local.get 0
      i32.const 1
      i32.store
    )
    (func $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve_for_push::h6b611529b178451b (;102;) (type 2) (param i32 i32)
      (local i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          local.get 1
          i32.const 1
          i32.add
          local.tee 1
          i32.eqz
          br_if 0 (;@2;)
          local.get 0
          i32.load
          local.tee 3
          i32.const 1
          i32.shl
          local.tee 4
          local.get 1
          local.get 4
          local.get 1
          i32.gt_u
          select
          local.tee 1
          i32.const 8
          local.get 1
          i32.const 8
          i32.gt_u
          select
          local.tee 1
          i32.const -1
          i32.xor
          i32.const 31
          i32.shr_u
          local.set 4
          block ;; label = @3
            block ;; label = @4
              local.get 3
              br_if 0 (;@4;)
              local.get 2
              i32.const 0
              i32.store offset=24
              br 1 (;@3;)
            end
            local.get 2
            local.get 3
            i32.store offset=28
            local.get 2
            i32.const 1
            i32.store offset=24
            local.get 2
            local.get 0
            i32.load offset=4
            i32.store offset=20
          end
          local.get 2
          i32.const 8
          i32.add
          local.get 4
          local.get 1
          local.get 2
          i32.const 20
          i32.add
          call $#func101<alloc::raw_vec::finish_grow::h9d78a0ebeee4f730__.llvm.17953091742988396336_>
          local.get 2
          i32.load offset=12
          local.set 3
          block ;; label = @3
            local.get 2
            i32.load offset=8
            br_if 0 (;@3;)
            local.get 0
            local.get 1
            i32.store
            local.get 0
            local.get 3
            i32.store offset=4
            br 2 (;@1;)
          end
          local.get 3
          i32.const -2147483647
          i32.eq
          br_if 1 (;@1;)
          local.get 3
          i32.eqz
          br_if 0 (;@2;)
          local.get 3
          local.get 2
          i32.const 16
          i32.add
          i32.load
          call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
          unreachable
        end
        call $alloc::raw_vec::capacity_overflow::h9441e1e81ed4591e
        unreachable
      end
      local.get 2
      i32.const 32
      i32.add
      global.set $__stack_pointer
    )
    (func $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve::do_reserve_and_handle::h676f2c087c7fa8d4 (;103;) (type 1) (param i32 i32 i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          local.get 1
          local.get 2
          i32.add
          local.tee 2
          local.get 1
          i32.lt_u
          br_if 0 (;@2;)
          local.get 0
          i32.load
          local.tee 1
          i32.const 1
          i32.shl
          local.tee 4
          local.get 2
          local.get 4
          local.get 2
          i32.gt_u
          select
          local.tee 2
          i32.const 8
          local.get 2
          i32.const 8
          i32.gt_u
          select
          local.tee 2
          i32.const -1
          i32.xor
          i32.const 31
          i32.shr_u
          local.set 4
          block ;; label = @3
            block ;; label = @4
              local.get 1
              br_if 0 (;@4;)
              local.get 3
              i32.const 0
              i32.store offset=24
              br 1 (;@3;)
            end
            local.get 3
            local.get 1
            i32.store offset=28
            local.get 3
            i32.const 1
            i32.store offset=24
            local.get 3
            local.get 0
            i32.load offset=4
            i32.store offset=20
          end
          local.get 3
          i32.const 8
          i32.add
          local.get 4
          local.get 2
          local.get 3
          i32.const 20
          i32.add
          call $#func101<alloc::raw_vec::finish_grow::h9d78a0ebeee4f730__.llvm.17953091742988396336_>
          local.get 3
          i32.load offset=12
          local.set 1
          block ;; label = @3
            local.get 3
            i32.load offset=8
            br_if 0 (;@3;)
            local.get 0
            local.get 2
            i32.store
            local.get 0
            local.get 1
            i32.store offset=4
            br 2 (;@1;)
          end
          local.get 1
          i32.const -2147483647
          i32.eq
          br_if 1 (;@1;)
          local.get 1
          i32.eqz
          br_if 0 (;@2;)
          local.get 1
          local.get 3
          i32.const 16
          i32.add
          i32.load
          call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
          unreachable
        end
        call $alloc::raw_vec::capacity_overflow::h9441e1e81ed4591e
        unreachable
      end
      local.get 3
      i32.const 32
      i32.add
      global.set $__stack_pointer
    )
    (func $__rust_start_panic (;104;) (type 4) (param i32 i32) (result i32)
      unreachable
      unreachable
    )
    (func $wasi::lib_generated::fd_write::h23a65eddaa0c426a (;105;) (type 10) (param i32 i32 i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 4
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          local.get 1
          local.get 2
          local.get 3
          local.get 4
          i32.const 12
          i32.add
          call $wasi::lib_generated::wasi_snapshot_preview1::fd_write::h1fa8da50c0f41235
          local.tee 3
          br_if 0 (;@2;)
          local.get 0
          local.get 4
          i32.load offset=12
          i32.store offset=4
          i32.const 0
          local.set 3
          br 1 (;@1;)
        end
        local.get 0
        local.get 3
        i32.store16 offset=2
        i32.const 1
        local.set 3
      end
      local.get 0
      local.get 3
      i32.store16
      local.get 4
      i32.const 16
      i32.add
      global.set $__stack_pointer
    )
    (func $_$LT$$RF$str$u20$as$u20$alloc..ffi..c_str..CString..new..SpecNewImpl$GT$::spec_new_impl::hd80596fd10e06640 (;106;) (type 1) (param i32 i32 i32)
      (local i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 48
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                block ;; label = @6
                  block ;; label = @7
                    block ;; label = @8
                      local.get 2
                      i32.const 1
                      i32.add
                      local.tee 4
                      i32.eqz
                      br_if 0 (;@8;)
                      local.get 4
                      i32.const -1
                      i32.le_s
                      br_if 4 (;@4;)
                      global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
                      i32.load8_u
                      drop
                      local.get 4
                      i32.const 1
                      call $__rust_alloc
                      local.tee 5
                      i32.eqz
                      br_if 1 (;@7;)
                      local.get 5
                      local.get 1
                      local.get 2
                      call $memcpy
                      local.set 6
                      block ;; label = @9
                        local.get 2
                        i32.const 8
                        i32.lt_u
                        br_if 0 (;@9;)
                        local.get 3
                        i32.const 0
                        local.get 1
                        local.get 2
                        call $core::slice::memchr::memchr_aligned::ha31a2be3ee5f1748
                        local.get 3
                        i32.load offset=4
                        local.set 7
                        local.get 3
                        i32.load
                        local.set 5
                        br 4 (;@5;)
                      end
                      block ;; label = @9
                        local.get 2
                        br_if 0 (;@9;)
                        i32.const 0
                        local.set 7
                        i32.const 0
                        local.set 5
                        br 4 (;@5;)
                      end
                      block ;; label = @9
                        local.get 1
                        i32.load8_u
                        br_if 0 (;@9;)
                        i32.const 1
                        local.set 5
                        i32.const 0
                        local.set 7
                        br 4 (;@5;)
                      end
                      i32.const 1
                      local.set 5
                      local.get 2
                      i32.const 1
                      i32.eq
                      br_if 2 (;@6;)
                      block ;; label = @9
                        local.get 1
                        i32.load8_u offset=1
                        br_if 0 (;@9;)
                        i32.const 1
                        local.set 7
                        br 4 (;@5;)
                      end
                      i32.const 2
                      local.set 7
                      local.get 2
                      i32.const 2
                      i32.eq
                      br_if 2 (;@6;)
                      local.get 1
                      i32.load8_u offset=2
                      i32.eqz
                      br_if 3 (;@5;)
                      i32.const 3
                      local.set 7
                      local.get 2
                      i32.const 3
                      i32.eq
                      br_if 2 (;@6;)
                      local.get 1
                      i32.load8_u offset=3
                      i32.eqz
                      br_if 3 (;@5;)
                      i32.const 4
                      local.set 7
                      local.get 2
                      i32.const 4
                      i32.eq
                      br_if 2 (;@6;)
                      local.get 1
                      i32.load8_u offset=4
                      i32.eqz
                      br_if 3 (;@5;)
                      i32.const 5
                      local.set 7
                      local.get 2
                      i32.const 5
                      i32.eq
                      br_if 2 (;@6;)
                      local.get 1
                      i32.load8_u offset=5
                      i32.eqz
                      br_if 3 (;@5;)
                      local.get 2
                      local.set 7
                      i32.const 0
                      local.set 5
                      local.get 2
                      i32.const 6
                      i32.eq
                      br_if 3 (;@5;)
                      local.get 2
                      i32.const 6
                      local.get 1
                      i32.load8_u offset=6
                      local.tee 1
                      select
                      local.set 7
                      local.get 1
                      i32.eqz
                      local.set 5
                      br 3 (;@5;)
                    end
                    global.get $__memory_base
                    local.tee 2
                    i32.const 2762
                    i32.add
                    i32.const 43
                    local.get 2
                    i32.const 4540
                    i32.add
                    call $core::panicking::panic::h6b5f2399fab9b82b
                    unreachable
                  end
                  i32.const 1
                  local.get 4
                  call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
                  unreachable
                end
                local.get 2
                local.set 7
                i32.const 0
                local.set 5
              end
              block ;; label = @5
                local.get 5
                br_if 0 (;@5;)
                local.get 3
                local.get 2
                i32.store offset=20
                local.get 3
                local.get 6
                i32.store offset=16
                local.get 3
                local.get 4
                i32.store offset=12
                local.get 4
                local.get 2
                i32.sub
                br_if 2 (;@3;)
                local.get 2
                i32.const 1
                i32.add
                local.tee 1
                i32.eqz
                br_if 1 (;@4;)
                local.get 1
                i32.const -1
                i32.xor
                i32.const 31
                i32.shr_u
                local.set 5
                block ;; label = @6
                  block ;; label = @7
                    local.get 4
                    br_if 0 (;@7;)
                    local.get 3
                    i32.const 0
                    i32.store offset=40
                    br 1 (;@6;)
                  end
                  local.get 3
                  local.get 4
                  i32.store offset=44
                  local.get 3
                  i32.const 1
                  i32.store offset=40
                  local.get 3
                  local.get 3
                  i32.load offset=16
                  i32.store offset=36
                end
                local.get 3
                i32.const 24
                i32.add
                local.get 5
                local.get 1
                local.get 3
                i32.const 36
                i32.add
                call $#func108<alloc::raw_vec::finish_grow::hae969e8ac10a6c7f__.llvm.2113455917521490986_>
                local.get 3
                i32.load offset=28
                local.set 5
                block ;; label = @6
                  local.get 3
                  i32.load offset=24
                  br_if 0 (;@6;)
                  local.get 3
                  local.get 1
                  i32.store offset=12
                  local.get 3
                  local.get 5
                  i32.store offset=16
                  local.get 1
                  local.set 4
                  br 3 (;@3;)
                end
                local.get 5
                i32.const -2147483647
                i32.eq
                br_if 2 (;@3;)
                local.get 5
                i32.eqz
                br_if 1 (;@4;)
                local.get 5
                local.get 3
                i32.const 32
                i32.add
                i32.load
                call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
                unreachable
              end
              local.get 0
              local.get 2
              i32.store offset=8
              local.get 0
              local.get 6
              i32.store offset=4
              local.get 0
              local.get 4
              i32.store
              local.get 0
              local.get 7
              i32.store offset=12
              br 2 (;@2;)
            end
            call $alloc::raw_vec::capacity_overflow::h9441e1e81ed4591e
            unreachable
          end
          block ;; label = @3
            local.get 2
            local.get 4
            i32.ne
            br_if 0 (;@3;)
            local.get 3
            i32.const 12
            i32.add
            local.get 2
            call $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve_for_push::h9be3efa62dcf8fea
            local.get 3
            i32.load offset=20
            local.set 2
          end
          local.get 3
          i32.load offset=16
          local.get 2
          i32.add
          i32.const 0
          i32.store8
          local.get 3
          local.get 3
          i32.load offset=20
          i32.const 1
          i32.add
          local.tee 2
          i32.store offset=20
          local.get 3
          i32.load offset=16
          local.set 4
          block ;; label = @3
            block ;; label = @4
              local.get 3
              i32.load offset=12
              local.tee 5
              local.get 2
              i32.gt_u
              br_if 0 (;@4;)
              local.get 4
              local.set 1
              br 1 (;@3;)
            end
            block ;; label = @4
              local.get 2
              br_if 0 (;@4;)
              i32.const 1
              local.set 1
              local.get 4
              local.get 5
              i32.const 1
              call $__rust_dealloc
              br 1 (;@3;)
            end
            local.get 4
            local.get 5
            i32.const 1
            local.get 2
            call $__rust_realloc
            local.tee 1
            i32.eqz
            br_if 2 (;@1;)
          end
          local.get 0
          local.get 1
          i32.store offset=4
          local.get 0
          i32.const -2147483648
          i32.store
          local.get 0
          i32.const 8
          i32.add
          local.get 2
          i32.store
        end
        local.get 3
        i32.const 48
        i32.add
        global.set $__stack_pointer
        return
      end
      i32.const 1
      local.get 2
      call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
      unreachable
    )
    (func $alloc::raw_vec::capacity_overflow::h9441e1e81ed4591e (;107;) (type 6)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 0
      global.set $__stack_pointer
      local.get 0
      i32.const 20
      i32.add
      i64.const 0
      i64.store align=4
      local.get 0
      i32.const 1
      i32.store offset=12
      local.get 0
      global.get $__memory_base
      local.tee 1
      i32.const 4556
      i32.add
      i32.store offset=8
      local.get 0
      local.get 1
      i32.const 2920
      i32.add
      i32.store offset=16
      local.get 0
      i32.const 8
      i32.add
      local.get 1
      i32.const 4564
      i32.add
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $#func108<alloc::raw_vec::finish_grow::hae969e8ac10a6c7f__.llvm.2113455917521490986_> (@name "alloc::raw_vec::finish_grow::hae969e8ac10a6c7f (.llvm.2113455917521490986)") (;108;) (type 10) (param i32 i32 i32 i32)
      block ;; label = @1
        block ;; label = @2
          local.get 1
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          i32.const -1
          i32.le_s
          br_if 1 (;@1;)
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                block ;; label = @6
                  local.get 3
                  i32.load offset=4
                  i32.eqz
                  br_if 0 (;@6;)
                  block ;; label = @7
                    local.get 3
                    i32.const 8
                    i32.add
                    i32.load
                    local.tee 1
                    br_if 0 (;@7;)
                    block ;; label = @8
                      local.get 2
                      br_if 0 (;@8;)
                      i32.const 1
                      local.set 1
                      br 4 (;@4;)
                    end
                    global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
                    i32.load8_u
                    drop
                    local.get 2
                    i32.const 1
                    call $__rust_alloc
                    local.set 1
                    br 2 (;@5;)
                  end
                  local.get 3
                  i32.load
                  local.get 1
                  i32.const 1
                  local.get 2
                  call $__rust_realloc
                  local.set 1
                  br 1 (;@5;)
                end
                block ;; label = @6
                  local.get 2
                  br_if 0 (;@6;)
                  i32.const 1
                  local.set 1
                  br 2 (;@4;)
                end
                global.get $GOT.data.internal.__rust_no_alloc_shim_is_unstable
                i32.load8_u
                drop
                local.get 2
                i32.const 1
                call $__rust_alloc
                local.set 1
              end
              local.get 1
              i32.eqz
              br_if 1 (;@3;)
            end
            local.get 0
            local.get 1
            i32.store offset=4
            local.get 0
            i32.const 8
            i32.add
            local.get 2
            i32.store
            local.get 0
            i32.const 0
            i32.store
            return
          end
          local.get 0
          i32.const 1
          i32.store offset=4
          local.get 0
          i32.const 8
          i32.add
          local.get 2
          i32.store
          local.get 0
          i32.const 1
          i32.store
          return
        end
        local.get 0
        i32.const 0
        i32.store offset=4
        local.get 0
        i32.const 8
        i32.add
        local.get 2
        i32.store
        local.get 0
        i32.const 1
        i32.store
        return
      end
      local.get 0
      i32.const 0
      i32.store offset=4
      local.get 0
      i32.const 1
      i32.store
    )
    (func $alloc::raw_vec::RawVec$LT$T$C$A$GT$::reserve_for_push::h9be3efa62dcf8fea (;109;) (type 2) (param i32 i32)
      (local i32 i32 i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      block ;; label = @1
        block ;; label = @2
          local.get 1
          i32.const 1
          i32.add
          local.tee 1
          i32.eqz
          br_if 0 (;@2;)
          local.get 0
          i32.load
          local.tee 3
          i32.const 1
          i32.shl
          local.tee 4
          local.get 1
          local.get 4
          local.get 1
          i32.gt_u
          select
          local.tee 1
          i32.const 8
          local.get 1
          i32.const 8
          i32.gt_u
          select
          local.tee 1
          i32.const -1
          i32.xor
          i32.const 31
          i32.shr_u
          local.set 4
          block ;; label = @3
            block ;; label = @4
              local.get 3
              br_if 0 (;@4;)
              local.get 2
              i32.const 0
              i32.store offset=24
              br 1 (;@3;)
            end
            local.get 2
            local.get 3
            i32.store offset=28
            local.get 2
            i32.const 1
            i32.store offset=24
            local.get 2
            local.get 0
            i32.load offset=4
            i32.store offset=20
          end
          local.get 2
          i32.const 8
          i32.add
          local.get 4
          local.get 1
          local.get 2
          i32.const 20
          i32.add
          call $#func108<alloc::raw_vec::finish_grow::hae969e8ac10a6c7f__.llvm.2113455917521490986_>
          local.get 2
          i32.load offset=12
          local.set 3
          block ;; label = @3
            local.get 2
            i32.load offset=8
            br_if 0 (;@3;)
            local.get 0
            local.get 1
            i32.store
            local.get 0
            local.get 3
            i32.store offset=4
            br 2 (;@1;)
          end
          local.get 3
          i32.const -2147483647
          i32.eq
          br_if 1 (;@1;)
          local.get 3
          i32.eqz
          br_if 0 (;@2;)
          local.get 3
          local.get 2
          i32.const 16
          i32.add
          i32.load
          call $alloc::alloc::handle_alloc_error::hb70e878d8319c339
          unreachable
        end
        call $alloc::raw_vec::capacity_overflow::h9441e1e81ed4591e
        unreachable
      end
      local.get 2
      i32.const 32
      i32.add
      global.set $__stack_pointer
    )
    (func $core::ptr::drop_in_place$LT$core..alloc..layout..LayoutError$GT$::h9b75eb298734e077 (;110;) (type 0) (param i32))
    (func $_$LT$core..alloc..layout..LayoutError$u20$as$u20$core..fmt..Debug$GT$::fmt::h64ca7dd1b0bd65bd (;111;) (type 4) (param i32 i32) (result i32)
      local.get 1
      global.get $__memory_base
      i32.const 3047
      i32.add
      i32.const 11
      call $core::fmt::Formatter::write_str::hbd1d482a114ac427
    )
    (func $alloc::alloc::handle_alloc_error::hb70e878d8319c339 (;112;) (type 2) (param i32 i32)
      local.get 1
      local.get 0
      call $__rust_alloc_error_handler
      unreachable
    )
    (func $alloc::sync::arcinner_layout_for_value_layout::he4ff1f6789a3f57f (;113;) (type 1) (param i32 i32 i32)
      (local i32 i32)
      global.get $__stack_pointer
      i32.const 16
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      block ;; label = @1
        local.get 1
        i32.const 7
        i32.add
        i32.const 0
        local.get 1
        i32.sub
        i32.and
        local.tee 4
        local.get 4
        i32.const -8
        i32.add
        i32.lt_u
        br_if 0 (;@1;)
        local.get 4
        local.get 2
        i32.add
        local.tee 2
        local.get 4
        i32.lt_u
        br_if 0 (;@1;)
        local.get 2
        i32.const -2147483648
        local.get 1
        i32.const 4
        local.get 1
        i32.const 4
        i32.gt_u
        select
        local.tee 1
        i32.sub
        i32.gt_u
        br_if 0 (;@1;)
        local.get 0
        local.get 1
        i32.store
        local.get 0
        local.get 1
        local.get 2
        i32.add
        i32.const -1
        i32.add
        i32.const 0
        local.get 1
        i32.sub
        i32.and
        i32.store offset=4
        local.get 3
        i32.const 16
        i32.add
        global.set $__stack_pointer
        return
      end
      global.get $__memory_base
      local.tee 1
      i32.const 3058
      i32.add
      i32.const 43
      local.get 3
      i32.const 15
      i32.add
      local.get 1
      i32.const 4580
      i32.add
      local.get 1
      i32.const 4596
      i32.add
      call $core::result::unwrap_failed::h0a2a95058939daaf
      unreachable
    )
    (func $core::fmt::write::h9a3c09758e76bf05 (;114;) (type 3) (param i32 i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 48
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      local.get 3
      i32.const 36
      i32.add
      local.get 1
      i32.store
      local.get 3
      i32.const 3
      i32.store8 offset=44
      local.get 3
      i32.const 32
      i32.store offset=28
      i32.const 0
      local.set 4
      local.get 3
      i32.const 0
      i32.store offset=40
      local.get 3
      local.get 0
      i32.store offset=32
      local.get 3
      i32.const 0
      i32.store offset=20
      local.get 3
      i32.const 0
      i32.store offset=12
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                local.get 2
                i32.load offset=16
                local.tee 5
                br_if 0 (;@5;)
                local.get 2
                i32.const 12
                i32.add
                i32.load
                local.tee 0
                i32.eqz
                br_if 1 (;@4;)
                local.get 2
                i32.load offset=8
                local.tee 1
                local.get 0
                i32.const 3
                i32.shl
                i32.add
                local.set 6
                local.get 0
                i32.const -1
                i32.add
                i32.const 536870911
                i32.and
                i32.const 1
                i32.add
                local.set 4
                local.get 2
                i32.load
                local.set 0
                i32.const 0
                local.set 7
                loop ;; label = @6
                  block ;; label = @7
                    local.get 0
                    i32.const 4
                    i32.add
                    i32.load
                    local.tee 8
                    i32.eqz
                    br_if 0 (;@7;)
                    local.get 3
                    i32.load offset=32
                    local.get 0
                    i32.load
                    local.get 8
                    local.get 3
                    i32.load offset=36
                    i32.load offset=12
                    call_indirect (type 3)
                    br_if 4 (;@3;)
                  end
                  local.get 1
                  i32.load
                  local.get 3
                  i32.const 12
                  i32.add
                  local.get 1
                  i32.const 4
                  i32.add
                  i32.load
                  call_indirect (type 4)
                  br_if 3 (;@3;)
                  local.get 7
                  i32.const 1
                  i32.add
                  local.set 7
                  local.get 0
                  i32.const 8
                  i32.add
                  local.set 0
                  local.get 1
                  i32.const 8
                  i32.add
                  local.tee 1
                  local.get 6
                  i32.ne
                  br_if 0 (;@6;)
                  br 2 (;@4;)
                end
              end
              local.get 2
              i32.const 20
              i32.add
              i32.load
              local.tee 1
              i32.eqz
              br_if 0 (;@4;)
              local.get 1
              i32.const 5
              i32.shl
              local.set 9
              local.get 1
              i32.const -1
              i32.add
              i32.const 134217727
              i32.and
              i32.const 1
              i32.add
              local.set 4
              local.get 2
              i32.load offset=8
              local.set 10
              local.get 2
              i32.load
              local.set 0
              i32.const 0
              local.set 7
              i32.const 0
              local.set 11
              loop ;; label = @5
                block ;; label = @6
                  local.get 0
                  i32.const 4
                  i32.add
                  i32.load
                  local.tee 1
                  i32.eqz
                  br_if 0 (;@6;)
                  local.get 3
                  i32.load offset=32
                  local.get 0
                  i32.load
                  local.get 1
                  local.get 3
                  i32.load offset=36
                  i32.load offset=12
                  call_indirect (type 3)
                  br_if 3 (;@3;)
                end
                local.get 3
                local.get 5
                local.get 7
                i32.add
                local.tee 1
                i32.const 16
                i32.add
                i32.load
                i32.store offset=28
                local.get 3
                local.get 1
                i32.const 28
                i32.add
                i32.load8_u
                i32.store8 offset=44
                local.get 3
                local.get 1
                i32.const 24
                i32.add
                i32.load
                i32.store offset=40
                local.get 1
                i32.const 12
                i32.add
                i32.load
                local.set 6
                i32.const 0
                local.set 12
                i32.const 0
                local.set 8
                block ;; label = @6
                  block ;; label = @7
                    block ;; label = @8
                      local.get 1
                      i32.const 8
                      i32.add
                      i32.load
                      br_table 1 (;@7;) 0 (;@8;) 2 (;@6;) 1 (;@7;)
                    end
                    global.get $__table_base
                    local.set 13
                    local.get 6
                    i32.const 3
                    i32.shl
                    local.set 14
                    i32.const 0
                    local.set 8
                    local.get 10
                    local.get 14
                    i32.add
                    local.tee 14
                    i32.load offset=4
                    local.get 13
                    i32.const 53
                    i32.add
                    i32.ne
                    br_if 1 (;@6;)
                    local.get 14
                    i32.load
                    i32.load
                    local.set 6
                  end
                  i32.const 1
                  local.set 8
                end
                local.get 3
                local.get 6
                i32.store offset=16
                local.get 3
                local.get 8
                i32.store offset=12
                local.get 1
                i32.const 4
                i32.add
                i32.load
                local.set 8
                block ;; label = @6
                  block ;; label = @7
                    block ;; label = @8
                      local.get 1
                      i32.load
                      br_table 1 (;@7;) 0 (;@8;) 2 (;@6;) 1 (;@7;)
                    end
                    global.get $__table_base
                    local.set 6
                    local.get 8
                    i32.const 3
                    i32.shl
                    local.set 13
                    local.get 10
                    local.get 13
                    i32.add
                    local.tee 13
                    i32.load offset=4
                    local.get 6
                    i32.const 53
                    i32.add
                    i32.ne
                    br_if 1 (;@6;)
                    local.get 13
                    i32.load
                    i32.load
                    local.set 8
                  end
                  i32.const 1
                  local.set 12
                end
                local.get 3
                local.get 8
                i32.store offset=24
                local.get 3
                local.get 12
                i32.store offset=20
                local.get 10
                local.get 1
                i32.const 20
                i32.add
                i32.load
                i32.const 3
                i32.shl
                i32.add
                local.tee 1
                i32.load
                local.get 3
                i32.const 12
                i32.add
                local.get 1
                i32.const 4
                i32.add
                i32.load
                call_indirect (type 4)
                br_if 2 (;@3;)
                local.get 11
                i32.const 1
                i32.add
                local.set 11
                local.get 0
                i32.const 8
                i32.add
                local.set 0
                local.get 9
                local.get 7
                i32.const 32
                i32.add
                local.tee 7
                i32.ne
                br_if 0 (;@5;)
              end
            end
            local.get 4
            local.get 2
            i32.load offset=4
            i32.ge_u
            br_if 1 (;@2;)
            local.get 3
            i32.load offset=32
            local.get 2
            i32.load
            local.get 4
            i32.const 3
            i32.shl
            i32.add
            local.tee 1
            i32.load
            local.get 1
            i32.load offset=4
            local.get 3
            i32.load offset=36
            i32.load offset=12
            call_indirect (type 3)
            i32.eqz
            br_if 1 (;@2;)
          end
          i32.const 1
          local.set 1
          br 1 (;@1;)
        end
        i32.const 0
        local.set 1
      end
      local.get 3
      i32.const 48
      i32.add
      global.set $__stack_pointer
      local.get 1
    )
    (func $_$LT$core..fmt..Arguments$u20$as$u20$core..fmt..Display$GT$::fmt::h0abdc867019bcb34 (;115;) (type 4) (param i32 i32) (result i32)
      local.get 1
      i32.load offset=20
      local.get 1
      i32.const 24
      i32.add
      i32.load
      local.get 0
      call $core::fmt::write::h9a3c09758e76bf05
    )
    (func $core::fmt::Formatter::pad_integral::h4c3505d4fc53f6b7 (;116;) (type 12) (param i32 i32 i32 i32 i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32)
      block ;; label = @1
        block ;; label = @2
          local.get 1
          br_if 0 (;@2;)
          local.get 5
          i32.const 1
          i32.add
          local.set 6
          local.get 0
          i32.load offset=28
          local.set 7
          i32.const 45
          local.set 8
          br 1 (;@1;)
        end
        i32.const 43
        i32.const 1114112
        local.get 0
        i32.load offset=28
        local.tee 7
        i32.const 1
        i32.and
        local.tee 1
        select
        local.set 8
        local.get 1
        local.get 5
        i32.add
        local.set 6
      end
      block ;; label = @1
        block ;; label = @2
          local.get 7
          i32.const 4
          i32.and
          br_if 0 (;@2;)
          i32.const 0
          local.set 2
          br 1 (;@1;)
        end
        block ;; label = @2
          block ;; label = @3
            local.get 3
            i32.const 16
            i32.lt_u
            br_if 0 (;@3;)
            local.get 2
            local.get 3
            call $core::str::count::do_count_chars::h971a1cc21c5012b1
            local.set 1
            br 1 (;@2;)
          end
          block ;; label = @3
            local.get 3
            br_if 0 (;@3;)
            i32.const 0
            local.set 1
            br 1 (;@2;)
          end
          local.get 3
          i32.const 3
          i32.and
          local.set 9
          block ;; label = @3
            block ;; label = @4
              local.get 3
              i32.const 4
              i32.ge_u
              br_if 0 (;@4;)
              i32.const 0
              local.set 1
              i32.const 0
              local.set 10
              br 1 (;@3;)
            end
            local.get 3
            i32.const -4
            i32.and
            local.set 11
            i32.const 0
            local.set 1
            i32.const 0
            local.set 10
            loop ;; label = @4
              local.get 1
              local.get 2
              local.get 10
              i32.add
              local.tee 12
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.get 12
              i32.const 1
              i32.add
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.get 12
              i32.const 2
              i32.add
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.get 12
              i32.const 3
              i32.add
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.set 1
              local.get 11
              local.get 10
              i32.const 4
              i32.add
              local.tee 10
              i32.ne
              br_if 0 (;@4;)
            end
          end
          local.get 9
          i32.eqz
          br_if 0 (;@2;)
          local.get 2
          local.get 10
          i32.add
          local.set 12
          loop ;; label = @3
            local.get 1
            local.get 12
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.set 1
            local.get 12
            i32.const 1
            i32.add
            local.set 12
            local.get 9
            i32.const -1
            i32.add
            local.tee 9
            br_if 0 (;@3;)
          end
        end
        local.get 1
        local.get 6
        i32.add
        local.set 6
      end
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i32.load
          br_if 0 (;@2;)
          i32.const 1
          local.set 1
          local.get 0
          i32.load offset=20
          local.tee 12
          local.get 0
          i32.load offset=24
          local.tee 10
          local.get 8
          local.get 2
          local.get 3
          call $core::fmt::Formatter::pad_integral::write_prefix::h63d0fde0afbfa83e
          br_if 1 (;@1;)
          local.get 12
          local.get 4
          local.get 5
          local.get 10
          i32.load offset=12
          call_indirect (type 3)
          return
        end
        block ;; label = @2
          local.get 0
          i32.load offset=4
          local.tee 9
          local.get 6
          i32.gt_u
          br_if 0 (;@2;)
          i32.const 1
          local.set 1
          local.get 0
          i32.load offset=20
          local.tee 12
          local.get 0
          i32.load offset=24
          local.tee 10
          local.get 8
          local.get 2
          local.get 3
          call $core::fmt::Formatter::pad_integral::write_prefix::h63d0fde0afbfa83e
          br_if 1 (;@1;)
          local.get 12
          local.get 4
          local.get 5
          local.get 10
          i32.load offset=12
          call_indirect (type 3)
          return
        end
        block ;; label = @2
          local.get 7
          i32.const 8
          i32.and
          i32.eqz
          br_if 0 (;@2;)
          local.get 0
          i32.load offset=16
          local.set 11
          local.get 0
          i32.const 48
          i32.store offset=16
          local.get 0
          i32.load8_u offset=32
          local.set 7
          i32.const 1
          local.set 1
          local.get 0
          i32.const 1
          i32.store8 offset=32
          local.get 0
          i32.load offset=20
          local.tee 12
          local.get 0
          i32.load offset=24
          local.tee 10
          local.get 8
          local.get 2
          local.get 3
          call $core::fmt::Formatter::pad_integral::write_prefix::h63d0fde0afbfa83e
          br_if 1 (;@1;)
          local.get 9
          local.get 6
          i32.sub
          i32.const 1
          i32.add
          local.set 1
          block ;; label = @3
            loop ;; label = @4
              local.get 1
              i32.const -1
              i32.add
              local.tee 1
              i32.eqz
              br_if 1 (;@3;)
              local.get 12
              i32.const 48
              local.get 10
              i32.load offset=16
              call_indirect (type 4)
              i32.eqz
              br_if 0 (;@4;)
            end
            i32.const 1
            return
          end
          i32.const 1
          local.set 1
          local.get 12
          local.get 4
          local.get 5
          local.get 10
          i32.load offset=12
          call_indirect (type 3)
          br_if 1 (;@1;)
          local.get 0
          local.get 7
          i32.store8 offset=32
          local.get 0
          local.get 11
          i32.store offset=16
          i32.const 0
          local.set 1
          br 1 (;@1;)
        end
        local.get 9
        local.get 6
        i32.sub
        local.set 6
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 0
              i32.load8_u offset=32
              local.tee 1
              br_table 2 (;@2;) 0 (;@4;) 1 (;@3;) 0 (;@4;) 2 (;@2;)
            end
            local.get 6
            local.set 1
            i32.const 0
            local.set 6
            br 1 (;@2;)
          end
          local.get 6
          i32.const 1
          i32.shr_u
          local.set 1
          local.get 6
          i32.const 1
          i32.add
          i32.const 1
          i32.shr_u
          local.set 6
        end
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 0
        i32.const 24
        i32.add
        i32.load
        local.set 12
        local.get 0
        i32.load offset=16
        local.set 9
        local.get 0
        i32.load offset=20
        local.set 10
        block ;; label = @2
          loop ;; label = @3
            local.get 1
            i32.const -1
            i32.add
            local.tee 1
            i32.eqz
            br_if 1 (;@2;)
            local.get 10
            local.get 9
            local.get 12
            i32.load offset=16
            call_indirect (type 4)
            i32.eqz
            br_if 0 (;@3;)
          end
          i32.const 1
          return
        end
        i32.const 1
        local.set 1
        local.get 10
        local.get 12
        local.get 8
        local.get 2
        local.get 3
        call $core::fmt::Formatter::pad_integral::write_prefix::h63d0fde0afbfa83e
        br_if 0 (;@1;)
        local.get 10
        local.get 4
        local.get 5
        local.get 12
        i32.load offset=12
        call_indirect (type 3)
        br_if 0 (;@1;)
        i32.const 0
        local.set 1
        loop ;; label = @2
          block ;; label = @3
            local.get 6
            local.get 1
            i32.ne
            br_if 0 (;@3;)
            local.get 6
            local.get 6
            i32.lt_u
            return
          end
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 10
          local.get 9
          local.get 12
          i32.load offset=16
          call_indirect (type 4)
          i32.eqz
          br_if 0 (;@2;)
        end
        local.get 1
        i32.const -1
        i32.add
        local.get 6
        i32.lt_u
        return
      end
      local.get 1
    )
    (func $core::fmt::Formatter::pad_integral::write_prefix::h63d0fde0afbfa83e (;117;) (type 13) (param i32 i32 i32 i32 i32) (result i32)
      (local i32)
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            local.get 2
            i32.const 1114112
            i32.eq
            br_if 0 (;@3;)
            i32.const 1
            local.set 5
            local.get 0
            local.get 2
            local.get 1
            i32.load offset=16
            call_indirect (type 4)
            br_if 1 (;@2;)
          end
          local.get 3
          br_if 1 (;@1;)
          i32.const 0
          local.set 5
        end
        local.get 5
        return
      end
      local.get 0
      local.get 3
      local.get 4
      local.get 1
      i32.load offset=12
      call_indirect (type 3)
    )
    (func $core::fmt::Formatter::pad::he5b059dce10eb550 (;118;) (type 3) (param i32 i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32 i32)
      block ;; label = @1
        local.get 0
        i32.load
        local.tee 3
        local.get 0
        i32.load offset=8
        local.tee 4
        i32.or
        i32.eqz
        br_if 0 (;@1;)
        block ;; label = @2
          local.get 4
          i32.eqz
          br_if 0 (;@2;)
          local.get 1
          local.get 2
          i32.add
          local.set 5
          local.get 0
          i32.const 12
          i32.add
          i32.load
          i32.const 1
          i32.add
          local.set 6
          i32.const 0
          local.set 7
          local.get 1
          local.set 8
          block ;; label = @3
            loop ;; label = @4
              local.get 8
              local.set 4
              local.get 6
              i32.const -1
              i32.add
              local.tee 6
              i32.eqz
              br_if 1 (;@3;)
              local.get 4
              local.get 5
              i32.eq
              br_if 2 (;@2;)
              block ;; label = @5
                block ;; label = @6
                  local.get 4
                  i32.load8_s
                  local.tee 9
                  i32.const -1
                  i32.le_s
                  br_if 0 (;@6;)
                  local.get 4
                  i32.const 1
                  i32.add
                  local.set 8
                  local.get 9
                  i32.const 255
                  i32.and
                  local.set 9
                  br 1 (;@5;)
                end
                local.get 4
                i32.load8_u offset=1
                i32.const 63
                i32.and
                local.set 10
                local.get 9
                i32.const 31
                i32.and
                local.set 8
                block ;; label = @6
                  local.get 9
                  i32.const -33
                  i32.gt_u
                  br_if 0 (;@6;)
                  local.get 8
                  i32.const 6
                  i32.shl
                  local.get 10
                  i32.or
                  local.set 9
                  local.get 4
                  i32.const 2
                  i32.add
                  local.set 8
                  br 1 (;@5;)
                end
                local.get 10
                i32.const 6
                i32.shl
                local.get 4
                i32.load8_u offset=2
                i32.const 63
                i32.and
                i32.or
                local.set 10
                block ;; label = @6
                  local.get 9
                  i32.const -16
                  i32.ge_u
                  br_if 0 (;@6;)
                  local.get 10
                  local.get 8
                  i32.const 12
                  i32.shl
                  i32.or
                  local.set 9
                  local.get 4
                  i32.const 3
                  i32.add
                  local.set 8
                  br 1 (;@5;)
                end
                local.get 10
                i32.const 6
                i32.shl
                local.get 4
                i32.load8_u offset=3
                i32.const 63
                i32.and
                i32.or
                local.get 8
                i32.const 18
                i32.shl
                i32.const 1835008
                i32.and
                i32.or
                local.tee 9
                i32.const 1114112
                i32.eq
                br_if 3 (;@2;)
                local.get 4
                i32.const 4
                i32.add
                local.set 8
              end
              local.get 7
              local.get 4
              i32.sub
              local.get 8
              i32.add
              local.set 7
              local.get 9
              i32.const 1114112
              i32.ne
              br_if 0 (;@4;)
              br 2 (;@2;)
            end
          end
          local.get 4
          local.get 5
          i32.eq
          br_if 0 (;@2;)
          block ;; label = @3
            local.get 4
            i32.load8_s
            local.tee 8
            i32.const -1
            i32.gt_s
            br_if 0 (;@3;)
            local.get 8
            i32.const -32
            i32.lt_u
            br_if 0 (;@3;)
            local.get 8
            i32.const -16
            i32.lt_u
            br_if 0 (;@3;)
            local.get 4
            i32.load8_u offset=2
            i32.const 63
            i32.and
            i32.const 6
            i32.shl
            local.get 4
            i32.load8_u offset=1
            i32.const 63
            i32.and
            i32.const 12
            i32.shl
            i32.or
            local.get 4
            i32.load8_u offset=3
            i32.const 63
            i32.and
            i32.or
            local.get 8
            i32.const 255
            i32.and
            i32.const 18
            i32.shl
            i32.const 1835008
            i32.and
            i32.or
            i32.const 1114112
            i32.eq
            br_if 1 (;@2;)
          end
          block ;; label = @3
            block ;; label = @4
              local.get 7
              i32.eqz
              br_if 0 (;@4;)
              block ;; label = @5
                local.get 7
                local.get 2
                i32.lt_u
                br_if 0 (;@5;)
                i32.const 0
                local.set 4
                local.get 7
                local.get 2
                i32.eq
                br_if 1 (;@4;)
                br 2 (;@3;)
              end
              i32.const 0
              local.set 4
              local.get 1
              local.get 7
              i32.add
              i32.load8_s
              i32.const -64
              i32.lt_s
              br_if 1 (;@3;)
            end
            local.get 1
            local.set 4
          end
          local.get 7
          local.get 2
          local.get 4
          select
          local.set 2
          local.get 4
          local.get 1
          local.get 4
          select
          local.set 1
        end
        block ;; label = @2
          local.get 3
          br_if 0 (;@2;)
          local.get 0
          i32.load offset=20
          local.get 1
          local.get 2
          local.get 0
          i32.const 24
          i32.add
          i32.load
          i32.load offset=12
          call_indirect (type 3)
          return
        end
        local.get 0
        i32.load offset=4
        local.set 5
        block ;; label = @2
          block ;; label = @3
            local.get 2
            i32.const 16
            i32.lt_u
            br_if 0 (;@3;)
            local.get 1
            local.get 2
            call $core::str::count::do_count_chars::h971a1cc21c5012b1
            local.set 4
            br 1 (;@2;)
          end
          block ;; label = @3
            local.get 2
            br_if 0 (;@3;)
            i32.const 0
            local.set 4
            br 1 (;@2;)
          end
          local.get 2
          i32.const 3
          i32.and
          local.set 6
          block ;; label = @3
            block ;; label = @4
              local.get 2
              i32.const 4
              i32.ge_u
              br_if 0 (;@4;)
              i32.const 0
              local.set 4
              i32.const 0
              local.set 9
              br 1 (;@3;)
            end
            local.get 2
            i32.const -4
            i32.and
            local.set 7
            i32.const 0
            local.set 4
            i32.const 0
            local.set 9
            loop ;; label = @4
              local.get 4
              local.get 1
              local.get 9
              i32.add
              local.tee 8
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.get 8
              i32.const 1
              i32.add
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.get 8
              i32.const 2
              i32.add
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.get 8
              i32.const 3
              i32.add
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.set 4
              local.get 7
              local.get 9
              i32.const 4
              i32.add
              local.tee 9
              i32.ne
              br_if 0 (;@4;)
            end
          end
          local.get 6
          i32.eqz
          br_if 0 (;@2;)
          local.get 1
          local.get 9
          i32.add
          local.set 8
          loop ;; label = @3
            local.get 4
            local.get 8
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.set 4
            local.get 8
            i32.const 1
            i32.add
            local.set 8
            local.get 6
            i32.const -1
            i32.add
            local.tee 6
            br_if 0 (;@3;)
          end
        end
        block ;; label = @2
          block ;; label = @3
            local.get 5
            local.get 4
            i32.le_u
            br_if 0 (;@3;)
            local.get 5
            local.get 4
            i32.sub
            local.set 7
            i32.const 0
            local.set 4
            block ;; label = @4
              block ;; label = @5
                block ;; label = @6
                  local.get 0
                  i32.load8_u offset=32
                  br_table 2 (;@4;) 0 (;@6;) 1 (;@5;) 2 (;@4;) 2 (;@4;)
                end
                local.get 7
                local.set 4
                i32.const 0
                local.set 7
                br 1 (;@4;)
              end
              local.get 7
              i32.const 1
              i32.shr_u
              local.set 4
              local.get 7
              i32.const 1
              i32.add
              i32.const 1
              i32.shr_u
              local.set 7
            end
            local.get 4
            i32.const 1
            i32.add
            local.set 4
            local.get 0
            i32.const 24
            i32.add
            i32.load
            local.set 8
            local.get 0
            i32.load offset=16
            local.set 6
            local.get 0
            i32.load offset=20
            local.set 9
            loop ;; label = @4
              local.get 4
              i32.const -1
              i32.add
              local.tee 4
              i32.eqz
              br_if 2 (;@2;)
              local.get 9
              local.get 6
              local.get 8
              i32.load offset=16
              call_indirect (type 4)
              i32.eqz
              br_if 0 (;@4;)
            end
            i32.const 1
            return
          end
          local.get 0
          i32.load offset=20
          local.get 1
          local.get 2
          local.get 0
          i32.const 24
          i32.add
          i32.load
          i32.load offset=12
          call_indirect (type 3)
          return
        end
        i32.const 1
        local.set 4
        block ;; label = @2
          local.get 9
          local.get 1
          local.get 2
          local.get 8
          i32.load offset=12
          call_indirect (type 3)
          br_if 0 (;@2;)
          i32.const 0
          local.set 4
          block ;; label = @3
            loop ;; label = @4
              block ;; label = @5
                local.get 7
                local.get 4
                i32.ne
                br_if 0 (;@5;)
                local.get 7
                local.set 4
                br 2 (;@3;)
              end
              local.get 4
              i32.const 1
              i32.add
              local.set 4
              local.get 9
              local.get 6
              local.get 8
              i32.load offset=16
              call_indirect (type 4)
              i32.eqz
              br_if 0 (;@4;)
            end
            local.get 4
            i32.const -1
            i32.add
            local.set 4
          end
          local.get 4
          local.get 7
          i32.lt_u
          local.set 4
        end
        local.get 4
        return
      end
      local.get 0
      i32.load offset=20
      local.get 1
      local.get 2
      local.get 0
      i32.const 24
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 3)
    )
    (func $core::fmt::Formatter::write_str::hbd1d482a114ac427 (;119;) (type 3) (param i32 i32 i32) (result i32)
      local.get 0
      i32.load offset=20
      local.get 1
      local.get 2
      local.get 0
      i32.const 24
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 3)
    )
    (func $core::fmt::Formatter::write_fmt::hbd0ea213d4da4bc3 (;120;) (type 4) (param i32 i32) (result i32)
      local.get 0
      i32.load offset=20
      local.get 0
      i32.const 24
      i32.add
      i32.load
      local.get 1
      call $core::fmt::write::h9a3c09758e76bf05
    )
    (func $core::fmt::Formatter::debug_struct::ha9d8a8b5657b52dc (;121;) (type 10) (param i32 i32 i32 i32)
      local.get 1
      i32.load offset=20
      local.get 2
      local.get 3
      local.get 1
      i32.const 24
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 3)
      local.set 3
      local.get 0
      i32.const 0
      i32.store8 offset=5
      local.get 0
      local.get 3
      i32.store8 offset=4
      local.get 0
      local.get 1
      i32.store
    )
    (func $_$LT$bool$u20$as$u20$core..fmt..Display$GT$::fmt::h5c6bb3c83a350620 (;122;) (type 4) (param i32 i32) (result i32)
      block ;; label = @1
        local.get 0
        i32.load8_u
        br_if 0 (;@1;)
        local.get 1
        global.get $__memory_base
        i32.const 3408
        i32.add
        i32.const 5
        call $core::fmt::Formatter::pad::he5b059dce10eb550
        return
      end
      local.get 1
      global.get $__memory_base
      i32.const 3413
      i32.add
      i32.const 4
      call $core::fmt::Formatter::pad::he5b059dce10eb550
    )
    (func $_$LT$str$u20$as$u20$core..fmt..Display$GT$::fmt::h7a939288e0f0136c (;123;) (type 3) (param i32 i32 i32) (result i32)
      local.get 2
      local.get 0
      local.get 1
      call $core::fmt::Formatter::pad::he5b059dce10eb550
    )
    (func $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::hefb7a28dbdd3d329 (;124;) (type 4) (param i32 i32) (result i32)
      local.get 0
      i32.load
      local.get 1
      local.get 0
      i32.load offset=4
      i32.load offset=12
      call_indirect (type 4)
    )
    (func $_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$::fmt::hfbbeeeb5a0b8c499 (;125;) (type 4) (param i32 i32) (result i32)
      local.get 1
      local.get 0
      i32.load
      local.get 0
      i32.load offset=4
      call $core::fmt::Formatter::pad::he5b059dce10eb550
    )
    (func $core::result::unwrap_failed::h0a2a95058939daaf (;126;) (type 9) (param i32 i32 i32 i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 64
      i32.sub
      local.tee 5
      global.set $__stack_pointer
      local.get 5
      local.get 1
      i32.store offset=12
      local.get 5
      local.get 0
      i32.store offset=8
      local.get 5
      local.get 3
      i32.store offset=20
      local.get 5
      local.get 2
      i32.store offset=16
      local.get 5
      i32.const 24
      i32.add
      i32.const 12
      i32.add
      i64.const 2
      i64.store align=4
      local.get 5
      i32.const 48
      i32.add
      i32.const 12
      i32.add
      global.get $__table_base
      local.tee 1
      i32.const 54
      i32.add
      i32.store
      local.get 5
      i32.const 2
      i32.store offset=28
      local.get 5
      global.get $__memory_base
      i32.const 4612
      i32.add
      i32.store offset=24
      local.get 5
      local.get 1
      i32.const 55
      i32.add
      i32.store offset=52
      local.get 5
      local.get 5
      i32.const 48
      i32.add
      i32.store offset=32
      local.get 5
      local.get 5
      i32.const 16
      i32.add
      i32.store offset=56
      local.get 5
      local.get 5
      i32.const 8
      i32.add
      i32.store offset=48
      local.get 5
      i32.const 24
      i32.add
      local.get 4
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $core::str::count::do_count_chars::h971a1cc21c5012b1 (;127;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32 i32)
      block ;; label = @1
        block ;; label = @2
          local.get 1
          local.get 0
          i32.const 3
          i32.add
          i32.const -4
          i32.and
          local.tee 2
          local.get 0
          i32.sub
          local.tee 3
          i32.lt_u
          br_if 0 (;@2;)
          local.get 1
          local.get 3
          i32.sub
          local.tee 4
          i32.const 4
          i32.lt_u
          br_if 0 (;@2;)
          local.get 4
          i32.const 3
          i32.and
          local.set 5
          i32.const 0
          local.set 6
          i32.const 0
          local.set 1
          block ;; label = @3
            local.get 2
            local.get 0
            i32.eq
            local.tee 7
            br_if 0 (;@3;)
            i32.const 0
            local.set 1
            block ;; label = @4
              block ;; label = @5
                local.get 2
                local.get 0
                i32.const -1
                i32.xor
                i32.add
                i32.const 3
                i32.ge_u
                br_if 0 (;@5;)
                i32.const 0
                local.set 8
                br 1 (;@4;)
              end
              i32.const 0
              local.set 8
              loop ;; label = @5
                local.get 1
                local.get 0
                local.get 8
                i32.add
                local.tee 9
                i32.load8_s
                i32.const -65
                i32.gt_s
                i32.add
                local.get 9
                i32.const 1
                i32.add
                i32.load8_s
                i32.const -65
                i32.gt_s
                i32.add
                local.get 9
                i32.const 2
                i32.add
                i32.load8_s
                i32.const -65
                i32.gt_s
                i32.add
                local.get 9
                i32.const 3
                i32.add
                i32.load8_s
                i32.const -65
                i32.gt_s
                i32.add
                local.set 1
                local.get 8
                i32.const 4
                i32.add
                local.tee 8
                br_if 0 (;@5;)
              end
            end
            local.get 7
            br_if 0 (;@3;)
            local.get 0
            local.get 2
            i32.sub
            local.set 2
            local.get 0
            local.get 8
            i32.add
            local.set 9
            loop ;; label = @4
              local.get 1
              local.get 9
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.set 1
              local.get 9
              i32.const 1
              i32.add
              local.set 9
              local.get 2
              i32.const 1
              i32.add
              local.tee 2
              br_if 0 (;@4;)
            end
          end
          local.get 0
          local.get 3
          i32.add
          local.set 8
          block ;; label = @3
            local.get 5
            i32.eqz
            br_if 0 (;@3;)
            local.get 8
            local.get 4
            i32.const -4
            i32.and
            i32.add
            local.tee 9
            i32.load8_s
            i32.const -65
            i32.gt_s
            local.set 6
            local.get 5
            i32.const 1
            i32.eq
            br_if 0 (;@3;)
            local.get 6
            local.get 9
            i32.load8_s offset=1
            i32.const -65
            i32.gt_s
            i32.add
            local.set 6
            local.get 5
            i32.const 2
            i32.eq
            br_if 0 (;@3;)
            local.get 6
            local.get 9
            i32.load8_s offset=2
            i32.const -65
            i32.gt_s
            i32.add
            local.set 6
          end
          local.get 4
          i32.const 2
          i32.shr_u
          local.set 3
          local.get 6
          local.get 1
          i32.add
          local.set 2
          loop ;; label = @3
            local.get 8
            local.set 6
            local.get 3
            i32.eqz
            br_if 2 (;@1;)
            local.get 3
            i32.const 192
            local.get 3
            i32.const 192
            i32.lt_u
            select
            local.tee 4
            i32.const 3
            i32.and
            local.set 7
            local.get 4
            i32.const 2
            i32.shl
            local.set 5
            i32.const 0
            local.set 9
            block ;; label = @4
              local.get 4
              i32.const 4
              i32.lt_u
              br_if 0 (;@4;)
              local.get 6
              local.get 5
              i32.const 1008
              i32.and
              i32.add
              local.set 0
              i32.const 0
              local.set 9
              local.get 6
              local.set 1
              loop ;; label = @5
                local.get 1
                i32.load
                local.tee 8
                i32.const -1
                i32.xor
                i32.const 7
                i32.shr_u
                local.get 8
                i32.const 6
                i32.shr_u
                i32.or
                i32.const 16843009
                i32.and
                local.get 9
                i32.add
                local.get 1
                i32.const 4
                i32.add
                i32.load
                local.tee 9
                i32.const -1
                i32.xor
                i32.const 7
                i32.shr_u
                local.get 9
                i32.const 6
                i32.shr_u
                i32.or
                i32.const 16843009
                i32.and
                i32.add
                local.get 1
                i32.const 8
                i32.add
                i32.load
                local.tee 9
                i32.const -1
                i32.xor
                i32.const 7
                i32.shr_u
                local.get 9
                i32.const 6
                i32.shr_u
                i32.or
                i32.const 16843009
                i32.and
                i32.add
                local.get 1
                i32.const 12
                i32.add
                i32.load
                local.tee 9
                i32.const -1
                i32.xor
                i32.const 7
                i32.shr_u
                local.get 9
                i32.const 6
                i32.shr_u
                i32.or
                i32.const 16843009
                i32.and
                i32.add
                local.set 9
                local.get 1
                i32.const 16
                i32.add
                local.tee 1
                local.get 0
                i32.ne
                br_if 0 (;@5;)
              end
            end
            local.get 3
            local.get 4
            i32.sub
            local.set 3
            local.get 6
            local.get 5
            i32.add
            local.set 8
            local.get 9
            i32.const 8
            i32.shr_u
            i32.const 16711935
            i32.and
            local.get 9
            i32.const 16711935
            i32.and
            i32.add
            i32.const 65537
            i32.mul
            i32.const 16
            i32.shr_u
            local.get 2
            i32.add
            local.set 2
            local.get 7
            i32.eqz
            br_if 0 (;@3;)
          end
          local.get 6
          local.get 4
          i32.const 252
          i32.and
          i32.const 2
          i32.shl
          i32.add
          local.tee 9
          i32.load
          local.tee 1
          i32.const -1
          i32.xor
          i32.const 7
          i32.shr_u
          local.get 1
          i32.const 6
          i32.shr_u
          i32.or
          i32.const 16843009
          i32.and
          local.set 1
          block ;; label = @3
            local.get 7
            i32.const 1
            i32.eq
            br_if 0 (;@3;)
            local.get 9
            i32.load offset=4
            local.tee 8
            i32.const -1
            i32.xor
            i32.const 7
            i32.shr_u
            local.get 8
            i32.const 6
            i32.shr_u
            i32.or
            i32.const 16843009
            i32.and
            local.get 1
            i32.add
            local.set 1
            local.get 7
            i32.const 2
            i32.eq
            br_if 0 (;@3;)
            local.get 9
            i32.load offset=8
            local.tee 9
            i32.const -1
            i32.xor
            i32.const 7
            i32.shr_u
            local.get 9
            i32.const 6
            i32.shr_u
            i32.or
            i32.const 16843009
            i32.and
            local.get 1
            i32.add
            local.set 1
          end
          local.get 1
          i32.const 8
          i32.shr_u
          i32.const 459007
          i32.and
          local.get 1
          i32.const 16711935
          i32.and
          i32.add
          i32.const 65537
          i32.mul
          i32.const 16
          i32.shr_u
          local.get 2
          i32.add
          return
        end
        block ;; label = @2
          local.get 1
          br_if 0 (;@2;)
          i32.const 0
          return
        end
        local.get 1
        i32.const 3
        i32.and
        local.set 8
        block ;; label = @2
          block ;; label = @3
            local.get 1
            i32.const 4
            i32.ge_u
            br_if 0 (;@3;)
            i32.const 0
            local.set 2
            i32.const 0
            local.set 9
            br 1 (;@2;)
          end
          local.get 1
          i32.const -4
          i32.and
          local.set 3
          i32.const 0
          local.set 2
          i32.const 0
          local.set 9
          loop ;; label = @3
            local.get 2
            local.get 0
            local.get 9
            i32.add
            local.tee 1
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.get 1
            i32.const 1
            i32.add
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.get 1
            i32.const 2
            i32.add
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.get 1
            i32.const 3
            i32.add
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.set 2
            local.get 3
            local.get 9
            i32.const 4
            i32.add
            local.tee 9
            i32.ne
            br_if 0 (;@3;)
          end
        end
        local.get 8
        i32.eqz
        br_if 0 (;@1;)
        local.get 0
        local.get 9
        i32.add
        local.set 1
        loop ;; label = @2
          local.get 2
          local.get 1
          i32.load8_s
          i32.const -65
          i32.gt_s
          i32.add
          local.set 2
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 8
          i32.const -1
          i32.add
          local.tee 8
          br_if 0 (;@2;)
        end
      end
      local.get 2
    )
    (func $#func128<core::fmt::num::imp::fmt_u64::h244ca1fb6ecd40bc__.llvm.2853079060249572303_> (@name "core::fmt::num::imp::fmt_u64::h244ca1fb6ecd40bc (.llvm.2853079060249572303)") (;128;) (type 14) (param i64 i32 i32) (result i32)
      (local i32 i32 i64 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 48
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      i32.const 39
      local.set 4
      block ;; label = @1
        block ;; label = @2
          local.get 0
          i64.const 10000
          i64.ge_u
          br_if 0 (;@2;)
          local.get 0
          local.set 5
          br 1 (;@1;)
        end
        i32.const 39
        local.set 4
        loop ;; label = @2
          local.get 3
          i32.const 9
          i32.add
          local.get 4
          i32.add
          local.tee 6
          i32.const -4
          i32.add
          global.get $__memory_base
          i32.const 3208
          i32.add
          local.tee 7
          local.get 0
          local.get 0
          i64.const 10000
          i64.div_u
          local.tee 5
          i64.const 10000
          i64.mul
          i64.sub
          i32.wrap_i64
          local.tee 8
          i32.const 65535
          i32.and
          i32.const 100
          i32.div_u
          local.tee 9
          i32.const 1
          i32.shl
          i32.add
          i32.load16_u align=1
          i32.store16 align=1
          local.get 6
          i32.const -2
          i32.add
          local.get 7
          local.get 8
          local.get 9
          i32.const 100
          i32.mul
          i32.sub
          i32.const 65535
          i32.and
          i32.const 1
          i32.shl
          i32.add
          i32.load16_u align=1
          i32.store16 align=1
          local.get 4
          i32.const -4
          i32.add
          local.set 4
          local.get 0
          i64.const 99999999
          i64.gt_u
          local.set 6
          local.get 5
          local.set 0
          local.get 6
          br_if 0 (;@2;)
        end
      end
      block ;; label = @1
        local.get 5
        i32.wrap_i64
        local.tee 6
        i32.const 99
        i32.le_u
        br_if 0 (;@1;)
        local.get 3
        i32.const 9
        i32.add
        local.get 4
        i32.const -2
        i32.add
        local.tee 4
        i32.add
        global.get $__memory_base
        i32.const 3208
        i32.add
        local.get 5
        i32.wrap_i64
        local.tee 6
        local.get 6
        i32.const 65535
        i32.and
        i32.const 100
        i32.div_u
        local.tee 6
        i32.const 100
        i32.mul
        i32.sub
        i32.const 65535
        i32.and
        i32.const 1
        i32.shl
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
      end
      block ;; label = @1
        block ;; label = @2
          local.get 6
          i32.const 10
          i32.lt_u
          br_if 0 (;@2;)
          local.get 3
          i32.const 9
          i32.add
          local.get 4
          i32.const -2
          i32.add
          local.tee 4
          i32.add
          global.get $__memory_base
          i32.const 3208
          i32.add
          local.get 6
          i32.const 1
          i32.shl
          i32.add
          i32.load16_u align=1
          i32.store16 align=1
          br 1 (;@1;)
        end
        local.get 3
        i32.const 9
        i32.add
        local.get 4
        i32.const -1
        i32.add
        local.tee 4
        i32.add
        local.get 6
        i32.const 48
        i32.add
        i32.store8
      end
      local.get 2
      local.get 1
      global.get $__memory_base
      i32.const 3424
      i32.add
      i32.const 0
      local.get 3
      i32.const 9
      i32.add
      local.get 4
      i32.add
      i32.const 39
      local.get 4
      i32.sub
      call $core::fmt::Formatter::pad_integral::h4c3505d4fc53f6b7
      local.set 4
      local.get 3
      i32.const 48
      i32.add
      global.set $__stack_pointer
      local.get 4
    )
    (func $core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$::fmt::h98edb2ad46049cf7 (;129;) (type 4) (param i32 i32) (result i32)
      local.get 0
      i64.load32_u
      i32.const 1
      local.get 1
      call $#func128<core::fmt::num::imp::fmt_u64::h244ca1fb6ecd40bc__.llvm.2853079060249572303_>
    )
    (func $#func130<core::ops::function::FnOnce::call_once::h1de21cb08b386aef__.llvm.2317637258618301942_> (@name "core::ops::function::FnOnce::call_once::h1de21cb08b386aef (.llvm.2317637258618301942)") (;130;) (type 4) (param i32 i32) (result i32)
      local.get 0
      i32.load
      drop
      loop (result i32) ;; label = @1
        br 0 (;@1;)
      end
    )
    (func $core::ptr::drop_in_place$LT$core..panic..panic_info..PanicInfo..internal_constructor..NoPayload$GT$::h6a104a878b33c3af (;131;) (type 0) (param i32))
    (func $core::panicking::panic_fmt::hf3388a9334a7d4c4 (;132;) (type 2) (param i32 i32)
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
      i32.const 4628
      i32.add
      i32.store offset=16
      local.get 2
      local.get 1
      i32.const 3424
      i32.add
      i32.store offset=12
      local.get 2
      i32.const 12
      i32.add
      call $rust_begin_unwind
      unreachable
    )
    (func $core::panicking::panic::h6b5f2399fab9b82b (;133;) (type 1) (param i32 i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 32
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      local.get 3
      i32.const 12
      i32.add
      i64.const 0
      i64.store align=4
      local.get 3
      i32.const 1
      i32.store offset=4
      local.get 3
      local.get 1
      i32.store offset=28
      local.get 3
      local.get 0
      i32.store offset=24
      local.get 3
      global.get $__memory_base
      i32.const 3424
      i32.add
      i32.store offset=8
      local.get 3
      local.get 3
      i32.const 24
      i32.add
      i32.store
      local.get 3
      local.get 2
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $core::panicking::assert_failed_inner::hda4f32f75bbd9c3b (;134;) (type 15) (param i32 i32 i32 i32 i32 i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 112
      i32.sub
      local.tee 7
      global.set $__stack_pointer
      local.get 7
      local.get 2
      i32.store offset=12
      local.get 7
      local.get 1
      i32.store offset=8
      local.get 7
      local.get 4
      i32.store offset=20
      local.get 7
      local.get 3
      i32.store offset=16
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 0
              i32.const 255
              i32.and
              br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 0 (;@4;)
            end
            local.get 7
            global.get $__memory_base
            i32.const 3424
            i32.add
            i32.store offset=24
            i32.const 2
            local.set 2
            br 2 (;@1;)
          end
          local.get 7
          global.get $__memory_base
          i32.const 3426
          i32.add
          i32.store offset=24
          i32.const 2
          local.set 2
          br 1 (;@1;)
        end
        local.get 7
        global.get $__memory_base
        i32.const 3428
        i32.add
        i32.store offset=24
        i32.const 7
        local.set 2
      end
      local.get 7
      local.get 2
      i32.store offset=28
      block ;; label = @1
        local.get 5
        i32.load
        br_if 0 (;@1;)
        local.get 7
        i32.const 76
        i32.add
        global.get $__table_base
        local.tee 5
        i32.const 54
        i32.add
        local.tee 2
        i32.store
        local.get 7
        i32.const 56
        i32.add
        i32.const 12
        i32.add
        local.get 2
        i32.store
        local.get 7
        i32.const 88
        i32.add
        i32.const 12
        i32.add
        i64.const 3
        i64.store align=4
        local.get 7
        i32.const 3
        i32.store offset=92
        local.get 7
        local.get 5
        i32.const 55
        i32.add
        i32.store offset=60
        local.get 7
        global.get $__memory_base
        i32.const 4644
        i32.add
        i32.store offset=88
        local.get 7
        local.get 7
        i32.const 56
        i32.add
        i32.store offset=96
        local.get 7
        local.get 7
        i32.const 16
        i32.add
        i32.store offset=72
        local.get 7
        local.get 7
        i32.const 8
        i32.add
        i32.store offset=64
        local.get 7
        local.get 7
        i32.const 24
        i32.add
        i32.store offset=56
        local.get 7
        i32.const 88
        i32.add
        local.get 6
        call $core::panicking::panic_fmt::hf3388a9334a7d4c4
        unreachable
      end
      local.get 7
      i32.const 32
      i32.add
      i32.const 16
      i32.add
      local.get 5
      i32.const 16
      i32.add
      i64.load align=4
      i64.store
      local.get 7
      i32.const 32
      i32.add
      i32.const 8
      i32.add
      local.get 5
      i32.const 8
      i32.add
      i64.load align=4
      i64.store
      local.get 7
      local.get 5
      i64.load align=4
      i64.store offset=32
      local.get 7
      i32.const 88
      i32.add
      i32.const 12
      i32.add
      i64.const 4
      i64.store align=4
      local.get 7
      i32.const 84
      i32.add
      global.get $__table_base
      local.tee 5
      i32.const 54
      i32.add
      local.tee 2
      i32.store
      local.get 7
      i32.const 76
      i32.add
      local.get 2
      i32.store
      local.get 7
      i32.const 56
      i32.add
      i32.const 12
      i32.add
      local.get 5
      i32.const 56
      i32.add
      i32.store
      local.get 7
      i32.const 4
      i32.store offset=92
      local.get 7
      global.get $__memory_base
      i32.const 4668
      i32.add
      i32.store offset=88
      local.get 7
      local.get 5
      i32.const 55
      i32.add
      i32.store offset=60
      local.get 7
      local.get 7
      i32.const 56
      i32.add
      i32.store offset=96
      local.get 7
      local.get 7
      i32.const 16
      i32.add
      i32.store offset=80
      local.get 7
      local.get 7
      i32.const 8
      i32.add
      i32.store offset=72
      local.get 7
      local.get 7
      i32.const 32
      i32.add
      i32.store offset=64
      local.get 7
      local.get 7
      i32.const 24
      i32.add
      i32.store offset=56
      local.get 7
      i32.const 88
      i32.add
      local.get 6
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $core::fmt::builders::DebugStruct::finish::hf1e218cbc9531c50 (;135;) (type 5) (param i32) (result i32)
      (local i32 i32)
      local.get 0
      i32.load8_u offset=4
      local.set 1
      block ;; label = @1
        local.get 0
        i32.load8_u offset=5
        br_if 0 (;@1;)
        local.get 1
        i32.const 255
        i32.and
        i32.const 0
        i32.ne
        return
      end
      i32.const 1
      local.set 2
      block ;; label = @1
        local.get 1
        i32.const 255
        i32.and
        br_if 0 (;@1;)
        block ;; label = @2
          local.get 0
          i32.load
          local.tee 1
          i32.load8_u offset=28
          i32.const 4
          i32.and
          br_if 0 (;@2;)
          global.get $__memory_base
          local.set 2
          local.get 0
          local.get 1
          i32.load offset=20
          local.get 2
          i32.const 3509
          i32.add
          i32.const 2
          local.get 1
          i32.load offset=24
          i32.load offset=12
          call_indirect (type 3)
          local.tee 1
          i32.store8 offset=4
          local.get 1
          return
        end
        global.get $__memory_base
        local.set 2
        local.get 1
        i32.load offset=20
        local.get 2
        i32.const 3508
        i32.add
        i32.const 1
        local.get 1
        i32.load offset=24
        i32.load offset=12
        call_indirect (type 3)
        local.set 2
      end
      local.get 0
      local.get 2
      i32.store8 offset=4
      local.get 2
    )
    (func $core::ffi::c_str::CStr::from_bytes_with_nul::he3131b60d7cc549c (;136;) (type 1) (param i32 i32 i32)
      (local i32 i32 i32 i32)
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              block ;; label = @5
                local.get 2
                i32.const 8
                i32.lt_u
                br_if 0 (;@5;)
                block ;; label = @6
                  block ;; label = @7
                    local.get 1
                    i32.const 3
                    i32.add
                    i32.const -4
                    i32.and
                    local.get 1
                    i32.sub
                    local.tee 3
                    i32.eqz
                    br_if 0 (;@7;)
                    i32.const 0
                    local.set 4
                    loop ;; label = @8
                      local.get 1
                      local.get 4
                      i32.add
                      i32.load8_u
                      i32.eqz
                      br_if 5 (;@3;)
                      local.get 3
                      local.get 4
                      i32.const 1
                      i32.add
                      local.tee 4
                      i32.ne
                      br_if 0 (;@8;)
                    end
                    local.get 3
                    local.get 2
                    i32.const -8
                    i32.add
                    local.tee 5
                    i32.le_u
                    br_if 1 (;@6;)
                    br 3 (;@4;)
                  end
                  local.get 2
                  i32.const -8
                  i32.add
                  local.set 5
                end
                loop ;; label = @6
                  local.get 1
                  local.get 3
                  i32.add
                  local.tee 4
                  i32.const 4
                  i32.add
                  i32.load
                  local.tee 6
                  i32.const -16843009
                  i32.add
                  local.get 6
                  i32.const -1
                  i32.xor
                  i32.and
                  local.get 4
                  i32.load
                  local.tee 4
                  i32.const -16843009
                  i32.add
                  local.get 4
                  i32.const -1
                  i32.xor
                  i32.and
                  i32.or
                  i32.const -2139062144
                  i32.and
                  br_if 2 (;@4;)
                  local.get 3
                  i32.const 8
                  i32.add
                  local.tee 3
                  local.get 5
                  i32.le_u
                  br_if 0 (;@6;)
                  br 2 (;@4;)
                end
              end
              local.get 2
              i32.eqz
              br_if 2 (;@2;)
              block ;; label = @5
                local.get 1
                i32.load8_u
                br_if 0 (;@5;)
                i32.const 0
                local.set 4
                br 2 (;@3;)
              end
              i32.const 1
              local.set 4
              local.get 2
              i32.const 1
              i32.eq
              br_if 2 (;@2;)
              local.get 1
              i32.load8_u offset=1
              i32.eqz
              br_if 1 (;@3;)
              i32.const 2
              local.set 4
              local.get 2
              i32.const 2
              i32.eq
              br_if 2 (;@2;)
              local.get 1
              i32.load8_u offset=2
              i32.eqz
              br_if 1 (;@3;)
              i32.const 3
              local.set 4
              local.get 2
              i32.const 3
              i32.eq
              br_if 2 (;@2;)
              local.get 1
              i32.load8_u offset=3
              i32.eqz
              br_if 1 (;@3;)
              i32.const 4
              local.set 4
              local.get 2
              i32.const 4
              i32.eq
              br_if 2 (;@2;)
              local.get 1
              i32.load8_u offset=4
              i32.eqz
              br_if 1 (;@3;)
              i32.const 5
              local.set 4
              local.get 2
              i32.const 5
              i32.eq
              br_if 2 (;@2;)
              local.get 1
              i32.load8_u offset=5
              i32.eqz
              br_if 1 (;@3;)
              i32.const 6
              local.set 4
              local.get 2
              i32.const 6
              i32.eq
              br_if 2 (;@2;)
              local.get 1
              i32.load8_u offset=6
              i32.eqz
              br_if 1 (;@3;)
              br 2 (;@2;)
            end
            local.get 3
            local.get 2
            i32.eq
            br_if 1 (;@2;)
            loop ;; label = @4
              block ;; label = @5
                local.get 1
                local.get 3
                i32.add
                i32.load8_u
                br_if 0 (;@5;)
                local.get 3
                local.set 4
                br 2 (;@3;)
              end
              local.get 2
              local.get 3
              i32.const 1
              i32.add
              local.tee 3
              i32.ne
              br_if 0 (;@4;)
              br 2 (;@2;)
            end
          end
          local.get 4
          i32.const 1
          i32.add
          local.get 2
          i32.eq
          br_if 1 (;@1;)
          local.get 0
          i32.const 0
          i32.store offset=4
          local.get 0
          i32.const 8
          i32.add
          local.get 4
          i32.store
          local.get 0
          i32.const 1
          i32.store
          return
        end
        local.get 0
        i32.const 1
        i32.store offset=4
        local.get 0
        i32.const 1
        i32.store
        return
      end
      local.get 0
      local.get 1
      i32.store offset=4
      local.get 0
      i32.const 8
      i32.add
      local.get 2
      i32.store
      local.get 0
      i32.const 0
      i32.store
    )
    (func $_$LT$core..panic..location..Location$u20$as$u20$core..fmt..Display$GT$::fmt::hcf775eb1a6630495 (;137;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32)
      global.get $__stack_pointer
      i32.const 48
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      local.get 2
      i32.const 44
      i32.add
      global.get $__table_base
      local.tee 3
      i32.const 18
      i32.add
      local.tee 4
      i32.store
      local.get 2
      i32.const 24
      i32.add
      i32.const 12
      i32.add
      local.get 4
      i32.store
      local.get 2
      i32.const 12
      i32.add
      i64.const 3
      i64.store align=4
      local.get 2
      i32.const 3
      i32.store offset=4
      local.get 2
      local.get 3
      i32.const 55
      i32.add
      i32.store offset=28
      local.get 2
      global.get $__memory_base
      i32.const 4700
      i32.add
      i32.store
      local.get 2
      local.get 0
      i32.store offset=24
      local.get 2
      local.get 0
      i32.const 12
      i32.add
      i32.store offset=40
      local.get 2
      local.get 0
      i32.const 8
      i32.add
      i32.store offset=32
      local.get 1
      i32.const 24
      i32.add
      i32.load
      local.set 0
      local.get 2
      local.get 2
      i32.const 24
      i32.add
      i32.store offset=8
      local.get 1
      i32.load offset=20
      local.get 0
      local.get 2
      call $core::fmt::write::h9a3c09758e76bf05
      local.set 0
      local.get 2
      i32.const 48
      i32.add
      global.set $__stack_pointer
      local.get 0
    )
    (func $core::slice::memchr::memchr_aligned::ha31a2be3ee5f1748 (;138;) (type 10) (param i32 i32 i32 i32)
      (local i32 i32 i32 i32 i32)
      block ;; label = @1
        block ;; label = @2
          block ;; label = @3
            block ;; label = @4
              local.get 2
              i32.const 3
              i32.add
              i32.const -4
              i32.and
              local.tee 4
              local.get 2
              i32.eq
              br_if 0 (;@4;)
              local.get 4
              local.get 2
              i32.sub
              local.tee 4
              local.get 3
              local.get 4
              local.get 3
              i32.lt_u
              select
              local.tee 4
              i32.eqz
              br_if 0 (;@4;)
              i32.const 0
              local.set 5
              local.get 1
              i32.const 255
              i32.and
              local.set 6
              i32.const 1
              local.set 7
              loop ;; label = @5
                local.get 2
                local.get 5
                i32.add
                i32.load8_u
                local.get 6
                i32.eq
                br_if 4 (;@1;)
                local.get 4
                local.get 5
                i32.const 1
                i32.add
                local.tee 5
                i32.ne
                br_if 0 (;@5;)
              end
              local.get 4
              local.get 3
              i32.const -8
              i32.add
              local.tee 8
              i32.gt_u
              br_if 2 (;@2;)
              br 1 (;@3;)
            end
            local.get 3
            i32.const -8
            i32.add
            local.set 8
            i32.const 0
            local.set 4
          end
          local.get 1
          i32.const 255
          i32.and
          i32.const 16843009
          i32.mul
          local.set 5
          loop ;; label = @3
            local.get 2
            local.get 4
            i32.add
            local.tee 6
            i32.const 4
            i32.add
            i32.load
            local.get 5
            i32.xor
            local.tee 7
            i32.const -16843009
            i32.add
            local.get 7
            i32.const -1
            i32.xor
            i32.and
            local.get 6
            i32.load
            local.get 5
            i32.xor
            local.tee 6
            i32.const -16843009
            i32.add
            local.get 6
            i32.const -1
            i32.xor
            i32.and
            i32.or
            i32.const -2139062144
            i32.and
            br_if 1 (;@2;)
            local.get 4
            i32.const 8
            i32.add
            local.tee 4
            local.get 8
            i32.le_u
            br_if 0 (;@3;)
          end
        end
        i32.const 0
        local.set 7
        block ;; label = @2
          local.get 4
          local.get 3
          i32.eq
          br_if 0 (;@2;)
          local.get 1
          i32.const 255
          i32.and
          local.set 5
          loop ;; label = @3
            block ;; label = @4
              local.get 2
              local.get 4
              i32.add
              i32.load8_u
              local.get 5
              i32.ne
              br_if 0 (;@4;)
              local.get 4
              local.set 5
              i32.const 1
              local.set 7
              br 3 (;@1;)
            end
            local.get 3
            local.get 4
            i32.const 1
            i32.add
            local.tee 4
            i32.ne
            br_if 0 (;@3;)
          end
        end
        local.get 3
        local.set 5
      end
      local.get 0
      local.get 5
      i32.store offset=4
      local.get 0
      local.get 7
      i32.store
    )
    (func $core::slice::index::slice_start_index_len_fail::hea1c763acd9dfde3 (;139;) (type 1) (param i32 i32 i32)
      (local i32)
      global.get $__stack_pointer
      i32.const 48
      i32.sub
      local.tee 3
      global.set $__stack_pointer
      local.get 3
      local.get 0
      i32.store
      local.get 3
      local.get 1
      i32.store offset=4
      local.get 3
      i32.const 8
      i32.add
      i32.const 12
      i32.add
      i64.const 2
      i64.store align=4
      local.get 3
      i32.const 32
      i32.add
      i32.const 12
      i32.add
      global.get $__table_base
      i32.const 18
      i32.add
      local.tee 0
      i32.store
      local.get 3
      i32.const 2
      i32.store offset=12
      local.get 3
      global.get $__memory_base
      i32.const 4724
      i32.add
      i32.store offset=8
      local.get 3
      local.get 0
      i32.store offset=36
      local.get 3
      local.get 3
      i32.const 32
      i32.add
      i32.store offset=16
      local.get 3
      local.get 3
      i32.const 4
      i32.add
      i32.store offset=40
      local.get 3
      local.get 3
      i32.store offset=32
      local.get 3
      i32.const 8
      i32.add
      local.get 2
      call $core::panicking::panic_fmt::hf3388a9334a7d4c4
      unreachable
    )
    (func $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h564ccd5ea9d298c8 (;140;) (type 2) (param i32 i32)
      local.get 0
      i64.const -6273870583586163964
      i64.store offset=8
      local.get 0
      i64.const -2479189555965818429
      i64.store
    )
    (func $_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$::fmt::haaf6d012a733bd8d (;141;) (type 4) (param i32 i32) (result i32)
      (local i32 i32 i32 i32 i32 i32 i32)
      global.get $__stack_pointer
      i32.const 64
      i32.sub
      local.tee 2
      global.set $__stack_pointer
      global.get $__memory_base
      local.set 3
      i32.const 1
      local.set 4
      block ;; label = @1
        local.get 1
        i32.load offset=20
        local.tee 5
        local.get 3
        i32.const 3565
        i32.add
        i32.const 12
        local.get 1
        i32.const 24
        i32.add
        i32.load
        local.tee 3
        i32.load offset=12
        local.tee 6
        call_indirect (type 3)
        br_if 0 (;@1;)
        local.get 0
        i32.load offset=12
        local.set 1
        local.get 2
        i32.const 16
        i32.add
        i32.const 12
        i32.add
        i64.const 3
        i64.store align=4
        local.get 2
        i32.const 60
        i32.add
        global.get $__table_base
        local.tee 7
        i32.const 18
        i32.add
        local.tee 8
        i32.store
        local.get 2
        i32.const 40
        i32.add
        i32.const 12
        i32.add
        local.get 8
        i32.store
        local.get 2
        i32.const 3
        i32.store offset=20
        local.get 2
        global.get $__memory_base
        i32.const 4700
        i32.add
        i32.store offset=16
        local.get 2
        local.get 1
        i32.const 12
        i32.add
        i32.store offset=56
        local.get 2
        local.get 1
        i32.const 8
        i32.add
        i32.store offset=48
        local.get 2
        local.get 7
        i32.const 55
        i32.add
        i32.store offset=44
        local.get 2
        local.get 1
        i32.store offset=40
        local.get 2
        local.get 2
        i32.const 40
        i32.add
        i32.store offset=24
        local.get 5
        local.get 3
        local.get 2
        i32.const 16
        i32.add
        call $core::fmt::write::h9a3c09758e76bf05
        br_if 0 (;@1;)
        block ;; label = @2
          block ;; label = @3
            local.get 0
            i32.load offset=8
            local.tee 1
            i32.eqz
            br_if 0 (;@3;)
            local.get 5
            global.get $__memory_base
            i32.const 3577
            i32.add
            i32.const 2
            local.get 6
            call_indirect (type 3)
            br_if 2 (;@1;)
            local.get 2
            i32.const 40
            i32.add
            i32.const 16
            i32.add
            local.get 1
            i32.const 16
            i32.add
            i64.load align=4
            i64.store
            local.get 2
            i32.const 40
            i32.add
            i32.const 8
            i32.add
            local.get 1
            i32.const 8
            i32.add
            i64.load align=4
            i64.store
            local.get 2
            local.get 1
            i64.load align=4
            i64.store offset=40
            local.get 5
            local.get 3
            local.get 2
            i32.const 40
            i32.add
            call $core::fmt::write::h9a3c09758e76bf05
            br_if 2 (;@1;)
            br 1 (;@2;)
          end
          local.get 2
          local.get 0
          i32.load
          local.tee 1
          local.get 0
          i32.load offset=4
          i32.const 12
          i32.add
          i32.load
          call_indirect (type 2)
          local.get 2
          i64.load
          i64.const -4493808902380553279
          i64.xor
          local.get 2
          i32.const 8
          i32.add
          i64.load
          i64.const -163230743173927068
          i64.xor
          i64.or
          i64.eqz
          i32.eqz
          br_if 0 (;@2;)
          local.get 5
          global.get $__memory_base
          i32.const 3577
          i32.add
          i32.const 2
          local.get 6
          call_indirect (type 3)
          br_if 1 (;@1;)
          local.get 5
          local.get 1
          i32.load
          local.get 1
          i32.load offset=4
          local.get 6
          call_indirect (type 3)
          br_if 1 (;@1;)
        end
        i32.const 0
        local.set 4
      end
      local.get 2
      i32.const 64
      i32.add
      global.set $__stack_pointer
      local.get 4
    )
    (global $GOT.data.internal.__rust_no_alloc_shim_is_unstable (;4;) (mut i32) i32.const 4757)
    (global $GOT.data.internal.__rust_alloc_error_handler_should_panic (;5;) (mut i32) i32.const 4756)
    (global $GOT.func.internal.core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$usize$GT$::fmt::hbfd83f0499f79957 (;6;) (mut i32) i32.const 18)
    (global $GOT.func.internal._$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$::fmt::haaf6d012a733bd8d (;7;) (mut i32) i32.const 29)
    (global $GOT.func.internal.core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$::fmt::h98edb2ad46049cf7 (;8;) (mut i32) i32.const 18)
    (export "__wasm_apply_data_relocs" (func $__wasm_apply_data_relocs))
    (export "_initialize" (func $_initialize))
    (export "dlclose" (func $dlclose))
    (export "dlerror" (func $dlerror))
    (export "dlopen" (func $dlopen))
    (export "dlsym" (func $dlsym))
    (export "__wasm_set_libraries" (func $__wasm_set_libraries))
    (start $__wasm_apply_global_relocs)
    (elem (;0;) (global.get $__table_base) func $core::ptr::drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$::hf998f532c50cb705 $_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$::write_str::h341120c3903b9bf2 $core::fmt::Write::write_char::h835ac4d37f70aa8f $core::fmt::Write::write_fmt::h0bb0a982cbee5fbf $_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$::write_str::h0cc9ffee36cd937f $core::fmt::Write::write_char::h9db6317e31914445 $core::fmt::Write::write_fmt::hf0f6500f70dd74b1 $#func42<core::ptr::drop_in_place$LT$$RF$u8$GT$::h1255938afe5b5108__.llvm.15332286852275299187_> $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::h38d29057232e3175 $core::ptr::drop_in_place$LT$$LT$alloc..boxed..Box$LT$dyn$u20$core..error..Error$u2b$core..marker..Sync$u2b$core..marker..Send$GT$$u20$as$u20$core..convert..From$LT$alloc..string..String$GT$$GT$..from..StringError$GT$::ha9469d5d85838b71 $_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_str::ha232be781f4501bb $_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_char::hbccd3dd660ab8f35 $core::fmt::Write::write_fmt::h31a2c20220c957e6 $#func47<core::ptr::drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$::hf998f532c50cb705__.llvm.16956913488488451105_> $#func56<core::ptr::drop_in_place$LT$std..thread..local..AccessError$GT$::h2b846ef7bba63faf__.llvm.11225111681125628113_> $_$LT$std..thread..local..AccessError$u20$as$u20$core..fmt..Debug$GT$::fmt::h894d1eb0ce9c3529 $#func61<core::ptr::drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$std..sys..wasi..stdio..Stderr$GT$$GT$::hdaf8233befe1c34a__.llvm.9557612308607314467_> $_$LT$std..sys_common..backtrace.._print..DisplayBacktrace$u20$as$u20$core..fmt..Display$GT$::fmt::h048cf0e1272391fd $core::fmt::num::imp::_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$::fmt::h98edb2ad46049cf7 $std::alloc::default_alloc_error_hook::h0be6e654dfab6bd4 $core::ptr::drop_in_place$LT$$RF$str$GT$::h6b1dc4f502fe29b8 $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h46e973aefffffd83 $_$LT$std..panicking..begin_panic_handler..StaticStrPayload$u20$as$u20$core..panic..PanicPayload$GT$::take_box::hb5f30bcde0dfc478 $_$LT$std..panicking..begin_panic_handler..StaticStrPayload$u20$as$u20$core..panic..PanicPayload$GT$::get::h501156f013b749a6 $core::ptr::drop_in_place$LT$std..panicking..begin_panic_handler..FormatStringPayload$GT$::h5dc476b836162c7e $_$LT$std..panicking..begin_panic_handler..FormatStringPayload$u20$as$u20$core..panic..PanicPayload$GT$::take_box::h6075c9dae37cc887 $_$LT$std..panicking..begin_panic_handler..FormatStringPayload$u20$as$u20$core..panic..PanicPayload$GT$::get::h8593919de4f10490 $_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$::fmt::h94e43c46ea1ae9fc $_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$::fmt::h5885aa12204b115c $_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$::fmt::haaf6d012a733bd8d $core::ptr::drop_in_place$LT$alloc..string..String$GT$::h7e859c3d49663973 $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::write::h56227fb4b7b7274d $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::write_vectored::hb080017ea512e2c8 $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::is_write_vectored::h47ca120c93aed961 $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::flush::h87a3932c46cb40cd $std::io::impls::_$LT$impl$u20$std..io..Write$u20$for$u20$alloc..vec..Vec$LT$u8$C$A$GT$$GT$::write_all::h2828d09c0a196513 $std::io::Write::write_all_vectored::ha7c695d0a1fd5e82 $std::io::Write::write_fmt::h1462c39bcd355635 $core::ptr::drop_in_place$LT$$LP$$RP$$GT$::hc7983eb888352f97 $_$LT$std..sys..wasi..stdio..Stderr$u20$as$u20$std..io..Write$GT$::write::had566fb5db304fce $_$LT$std..sys..wasi..stdio..Stderr$u20$as$u20$std..io..Write$GT$::write_vectored::ha0d7307e4f52640c $_$LT$std..sys..wasi..stdio..Stderr$u20$as$u20$std..io..Write$GT$::is_write_vectored::he4fe63897dfa8fc7 $_$LT$std..sys..wasi..stdio..Stderr$u20$as$u20$std..io..Write$GT$::flush::h821a5942ed937229 $std::io::Write::write_all::he1386585803e5b4d $std::io::Write::write_all_vectored::h7c465c96bfe57a1f $std::io::Write::write_fmt::h0b43c07e3174d5bc $#func86<_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_str::ha232be781f4501bb> $#func85<_$LT$alloc..string..String$u20$as$u20$core..fmt..Write$GT$::write_char::hbccd3dd660ab8f35> $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h6af49548f242c026 $core::ptr::drop_in_place$LT$i32$GT$::h7b5f74dcf93888ba $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h45e6f7a5d52f9492 $core::ptr::drop_in_place$LT$core..alloc..layout..LayoutError$GT$::h9b75eb298734e077 $_$LT$core..alloc..layout..LayoutError$u20$as$u20$core..fmt..Debug$GT$::fmt::h64ca7dd1b0bd65bd $#func130<core::ops::function::FnOnce::call_once::h1de21cb08b386aef__.llvm.2317637258618301942_> $_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$::fmt::hefb7a28dbdd3d329 $_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$::fmt::hfbbeeeb5a0b8c499 $_$LT$core..fmt..Arguments$u20$as$u20$core..fmt..Display$GT$::fmt::h0abdc867019bcb34 $core::ptr::drop_in_place$LT$core..panic..panic_info..PanicInfo..internal_constructor..NoPayload$GT$::h6a104a878b33c3af $_$LT$T$u20$as$u20$core..any..Any$GT$::type_id::h564ccd5ea9d298c8)
    (data $.data (;0;) (global.get $__memory_base) "`__wasm_set_libraries` should have been called during instantiation with a non-NULL valuesrc/lib.rsinvalid library handle\00library not found\00dlopen flags not yet supported\00symbol not found\00dlsym RTLD_NEXT and RTLD_DEFAULT not yet supported\00file name contained an unexpected NUL bytefile name contained an unexpected NUL byte\00reentrant init/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/core/src/cell/once.rs/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/thread/mod.rsfailed to generate unique thread ID: bitspace exhaustedadvancing io slices beyond their length/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/io/mod.rsfailed to write whole bufferformatter erroradvancing IoSlice beyond its length/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/sys/wasi/io.rsAccessErrorcannot access a Thread Local Storage value during or after destruction/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/thread/local.rsRUST_BACKTRACEinternal error: entered unreachable code/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/panic.rs\00\00\00advancing io slices beyond their length/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/io/mod.rsfailed to write whole bufferformatter erroradvancing IoSlice beyond its length/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/sys/wasi/io.rsstack backtrace:\0anote: Some details are omitted, run with `RUST_BACKTRACE=full` for a verbose backtrace.\0amemory allocation of  bytes failed\0a bytes failed/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/alloc.rscannot recursively acquire mutex\00/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/sys/wasi/../unsupported/locks/mutex.rs/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/panicking.rsBox<dyn Any><unnamed>thread '' panicked at :\0a\0anote: run with `RUST_BACKTRACE=1` environment variable to display a backtrace\0acalled `Option::unwrap()` on a `None` value\0apanicked after panic::always_abort(), aborting.\0athread panicked while processing panic. aborting.\0athread caused non-unwinding panic. aborting.\0afatal runtime error: failed to initiate panic, error cannot recursively acquire mutex\00/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/std/src/sys/wasi/../unsupported/locks/mutex.rsfatal runtime error: rwlock locked for writing\0acalled `Option::unwrap()` on a `None` value/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/ffi/c_str.rs\00\00\00/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/raw_vec.rscapacity overflowLayoutErrorcalled `Result::unwrap()` on an `Err` value/Users/dicej/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/library/alloc/src/sync.rs00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899falsetrue\00\00\00: \00\00==!=matchesassertion `left  right` failed\0a  left: \0a right:  right` failed: \0a  left: } }\00:range start index  out of range for slice of length panicked at :\0a\00\00\00\00\00\00\00\00\00Y\00\00\00Y\00\00\00\0a\00\00\008\00\00\00\09\00\00\00Y\00\00\00\0a\00\00\00h\00\00\00\09\00\00\00\ef\00\00\00*\00\00\00\14\00\00\00\00\00\00\00\02\00\00\00(\0e\00\00\19\01\00\00*\00\00\00\14\00\00\00\00\00\00\00\02\00\00\00@\0e\00\00\00\00\00\00\0c\00\00\00\04\00\00\00\01\00\00\00\02\00\00\00\03\00\00\00\00\00\00\00\0c\00\00\00\04\00\00\00\04\00\00\00\05\00\00\00\06\00\00\00\07\00\00\00\04\00\00\00\04\00\00\00\08\00\00\00\09\00\00\00\0c\00\00\00\04\00\00\00\0a\00\00\00\0b\00\00\00\0c\00\00\00D\01\00\00\0e\00\00\00R\01\00\00o\00\00\00\d9\00\00\00B\00\00\000\02\00\007\00\00\00\c1\01\00\00o\00\00\00\98\04\00\00\0d\00\00\00g\02\00\00'\00\00\00\8e\02\00\00k\00\00\00\a5\05\00\00\0d\00\00\00\8e\02\00\00k\00\00\00\a3\05\00\00 \00\00\00\f9\02\00\00\1c\00\00\00\17\00\00\00\0d\00\00\00\0c\00\00\00\04\00\00\00\01\00\00\00\02\00\00\00\03\00\00\00\15\03\00\00\0f\00\00\00(\00\00\00$\03\00\00#\00\00\00G\03\00\00p\00\00\00\17\00\00\00\0d\00\00\00\0e\00\00\00\00\00\00\00\01\00\00\00\0f\00\00\00\08\04\00\00q\00\00\00\04\01\00\00\1a\00\00\00\af\04\00\00j\00\00\00\f5\00\00\00\12\00\00\00\1c\05\00\00'\00\00\00C\05\00\00k\00\00\00\a5\05\00\00\0d\00\00\00C\05\00\00k\00\00\00\a3\05\00\00 \00\00\00\ae\05\00\00\1c\00\00\00\17\00\00\00C\05\00\00k\00\00\00\8d\06\00\00$\00\00\00\10\00\00\00\0c\00\00\00\04\00\00\00\04\00\00\00\05\00\00\00\06\00\00\00\ca\05\00\00\0f\00\00\00(\00\00\00\d9\05\00\00#\00\00\00\fc\05\00\00p\00\00\00\17\00\00\00\0d\00\00\00l\06\00\00\00\00\00\00l\06\00\00\11\00\00\00}\06\00\00X\00\00\00\d5\06\00\00\15\00\00\00\ea\06\00\00\0e\00\00\00\d5\06\00\00\15\00\00\00\f8\06\00\00\0d\00\00\00\05\07\00\00j\00\00\00b\01\00\00\09\00\00\00\14\00\00\00\08\00\00\00\04\00\00\00\15\00\00\00\14\00\00\00\08\00\00\00\04\00\00\00\16\00\00\00\17\00\00\00\18\00\00\00\10\00\00\00\04\00\00\00\19\00\00\00\1a\00\00\00o\07\00\00 \00\00\00\90\07\00\00\88\00\00\00\14\00\00\00\09\00\00\00\1e\00\00\00\0c\00\00\00\04\00\00\00\1f\00\00\00 \00\00\00!\00\00\00\22\00\00\00#\00\00\00$\00\00\00%\00\00\00&\00\00\00\00\00\00\00\01\00\00\00'\00\00\00(\00\00\00)\00\00\00*\00\00\00+\00\00\00,\00\00\00-\00\00\00\9b\08\00\00\08\00\00\00\a3\08\00\00\0e\00\00\00\b1\08\00\00\02\00\00\00\b3\08\00\00\01\00\00\00\b4\08\00\00N\00\00\00\01\00\00\00\18\08\00\00n\00\00\00\84\02\00\00\1e\00\00\00\1e\00\00\00\0c\00\00\00\04\00\00\00.\00\00\00/\00\00\00\0c\00\00\00\1e\00\00\00\0c\00\00\00\04\00\00\000\00\00\001\00\00\00\08\00\00\00\04\00\00\00\15\00\00\00&\00\00\00\00\00\00\00\01\00\00\002\00\00\00\18\08\00\00\00\00\00\00-\09\00\001\00\00\00^\09\00\002\00\00\00\90\09\00\00-\00\00\00\bd\09\00\005\00\00\00\b3\08\00\00\01\00\00\00\f2\09\00\00 \00\00\00\13\0a\00\00\88\00\00\00\14\00\00\00\09\00\00\00\9b\0a\00\00/\00\00\00\f5\0a\00\00p\00\00\00\1b\01\00\007\00\00\00\d6\0b\00\00\11\00\00\00h\0b\00\00n\00\00\00;\02\00\00\05\00\00\003\00\00\00\00\00\00\00\01\00\00\004\00\00\00\1d\0c\00\00k\00\00\00o\01\00\002\00\00\00\5c\0d\00\00\00\00\00\00\5c\0d\00\00\02\00\00\009\00\00\00\00\00\00\00\01\00\00\00:\00\00\00k\0d\00\00\10\00\00\00{\0d\00\00\17\00\00\00\92\0d\00\00\09\00\00\00k\0d\00\00\10\00\00\00\9b\0d\00\00\10\00\00\00\ab\0d\00\00\09\00\00\00\92\0d\00\00\09\00\00\00\b8\0d\00\00\00\00\00\00\b8\0d\00\00\01\00\00\00\b8\0d\00\00\01\00\00\00\b9\0d\00\00\12\00\00\00\cb\0d\00\00\22\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
    (@producers
      (processed-by "clang" "17.0.6 (https://github.com/llvm/llvm-project 6009708b4367171ccdbf4b5905cb6a803753fe18)")
      (processed-by "rustc" "1.77.0-nightly (75c68cfd2 2024-01-07)")
    )
    (@custom "target_features" (after data) "\02+\0fmutable-globals+\08sign-ext")
  )
  (core module (;5;)
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
  (core module (;6;)
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
    (import "env" "libc.so:memory_base" (global (;2;) i32))
    (import "libc.so" "errno" (global (;3;) i32))
    (import "env" "libdl.so:errno" (global (;4;) (mut i32)))
    (import "foo" "bar" (func (;3;) (type 2)))
    (import "foo" "baz" (func (;4;) (type 3)))
    (import "foo" "test:test/test#foo" (func (;5;) (type 4)))
    (func (;6;) (type 0)
      i32.const 1048656
      global.get 0
      global.get 1
      i32.add
      i32.store
      global.get 2
      global.get 3
      i32.add
      global.set 4
      call 0
      call 1
      i32.const 1048676
      call 2
    )
    (start 6)
    (elem (;0;) (i32.const 1) func 3 4 5)
    (elem (;1;) (i32.const 64) func)
    (data (;0;) (i32.const 1048576) "foo\00bar\00baz\00test:test/test#foo\00\00what\03\00\00\00\04\00\10\00\01\00\00\00\03\00\00\00\08\00\10\00\02\00\00\00\12\00\00\00\0c\00\10\00\03\00\00\00\04\00\00\00 \00\10\00\00\00\00\00\03\00\00\00\00\00\10\00\04\00\00\00$\00\10\00\01\00\00\00T\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;7;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (func $adapt-wasi_snapshot_preview1-fd_write (;0;) (type 0) (param i32 i32 i32 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 0
      call_indirect (type 0)
    )
    (table (;0;) 1 1 funcref)
    (export "0" (func $adapt-wasi_snapshot_preview1-fd_write))
    (export "$imports" (table 0))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;8;)
    (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
    (import "" "0" (func (;0;) (type 0)))
    (import "" "$imports" (table (;0;) 1 1 funcref))
    (elem (;0;) (i32.const 0) func 0)
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance (;0;) (instantiate 7))
  (alias core export 0 "0" (core func (;0;)))
  (core instance (;1;)
    (export "fd_write" (func 0))
  )
  (core instance (;2;) (instantiate 0
      (with "wasi_snapshot_preview1" (instance 1))
    )
  )
  (alias core export 2 "memory" (core memory (;0;)))
  (core instance (;3;) (instantiate 1))
  (alias core export 2 "__heap_base" (core global (;0;)))
  (alias core export 2 "__heap_end" (core global (;1;)))
  (core instance (;4;)
    (export "__heap_base" (global 0))
    (export "__heap_end" (global 1))
  )
  (core instance (;5;))
  (alias core export 2 "memory" (core memory (;1;)))
  (alias core export 2 "__indirect_function_table" (core table (;0;)))
  (alias core export 2 "__stack_pointer" (core global (;2;)))
  (alias core export 2 "libc.so:memory_base" (core global (;3;)))
  (alias core export 2 "libc.so:table_base" (core global (;4;)))
  (core instance (;6;)
    (export "memory" (memory 1))
    (export "__indirect_function_table" (table 0))
    (export "__stack_pointer" (global 2))
    (export "__memory_base" (global 3))
    (export "__table_base" (global 4))
  )
  (core instance (;7;) (instantiate 2
      (with "GOT.mem" (instance 4))
      (with "GOT.func" (instance 5))
      (with "env" (instance 6))
    )
  )
  (alias core export 2 "__heap_base" (core global (;5;)))
  (alias core export 2 "__heap_end" (core global (;6;)))
  (core instance (;8;)
    (export "__heap_base" (global 5))
    (export "__heap_end" (global 6))
  )
  (core instance (;9;))
  (alias core export 2 "memory" (core memory (;2;)))
  (alias core export 2 "__indirect_function_table" (core table (;1;)))
  (alias core export 2 "__stack_pointer" (core global (;7;)))
  (alias core export 2 "wit-component:stubs:memory_base" (core global (;8;)))
  (alias core export 2 "wit-component:stubs:table_base" (core global (;9;)))
  (core instance (;10;)
    (export "memory" (memory 2))
    (export "__indirect_function_table" (table 1))
    (export "__stack_pointer" (global 7))
    (export "__memory_base" (global 8))
    (export "__table_base" (global 9))
  )
  (core instance (;11;) (instantiate 3
      (with "GOT.mem" (instance 8))
      (with "GOT.func" (instance 9))
      (with "env" (instance 10))
    )
  )
  (alias core export 2 "libdl.so:errno" (core global (;10;)))
  (alias core export 2 "__heap_base" (core global (;11;)))
  (alias core export 2 "__heap_end" (core global (;12;)))
  (core instance (;12;)
    (export "errno" (global 10))
    (export "__heap_base" (global 11))
    (export "__heap_end" (global 12))
  )
  (core instance (;13;))
  (alias core export 2 "memory" (core memory (;3;)))
  (alias core export 2 "__indirect_function_table" (core table (;2;)))
  (alias core export 2 "__stack_pointer" (core global (;13;)))
  (alias core export 2 "libdl.so:memory_base" (core global (;14;)))
  (alias core export 2 "libdl.so:table_base" (core global (;15;)))
  (alias core export 7 "abort" (core func (;1;)))
  (alias core export 11 "aligned_alloc" (core func (;2;)))
  (alias core export 11 "free" (core func (;3;)))
  (alias core export 11 "getcwd" (core func (;4;)))
  (alias core export 11 "getenv" (core func (;5;)))
  (alias core export 7 "malloc" (core func (;6;)))
  (alias core export 11 "memcmp" (core func (;7;)))
  (alias core export 11 "memcpy" (core func (;8;)))
  (alias core export 11 "realloc" (core func (;9;)))
  (alias core export 11 "strlen" (core func (;10;)))
  (core instance (;14;)
    (export "memory" (memory 3))
    (export "__indirect_function_table" (table 2))
    (export "__stack_pointer" (global 13))
    (export "__memory_base" (global 14))
    (export "__table_base" (global 15))
    (export "abort" (func 1))
    (export "aligned_alloc" (func 2))
    (export "free" (func 3))
    (export "getcwd" (func 4))
    (export "getenv" (func 5))
    (export "malloc" (func 6))
    (export "memcmp" (func 7))
    (export "memcpy" (func 8))
    (export "realloc" (func 9))
    (export "strlen" (func 10))
  )
  (alias core export 2 "wasi_snapshot_preview1:fd_write" (core func (;11;)))
  (core instance (;15;)
    (export "fd_write" (func 11))
  )
  (core instance (;16;) (instantiate 4
      (with "GOT.mem" (instance 12))
      (with "GOT.func" (instance 13))
      (with "env" (instance 14))
      (with "wasi_snapshot_preview1" (instance 15))
    )
  )
  (alias core export 2 "__heap_base" (core global (;16;)))
  (alias core export 2 "__heap_end" (core global (;17;)))
  (core instance (;17;)
    (export "__heap_base" (global 16))
    (export "__heap_end" (global 17))
  )
  (core instance (;18;))
  (alias core export 2 "memory" (core memory (;4;)))
  (alias core export 2 "__indirect_function_table" (core table (;3;)))
  (alias core export 2 "__stack_pointer" (core global (;18;)))
  (alias core export 2 "foo:memory_base" (core global (;19;)))
  (alias core export 2 "foo:table_base" (core global (;20;)))
  (alias core export 16 "dlopen" (core func (;12;)))
  (core instance (;19;)
    (export "memory" (memory 4))
    (export "__indirect_function_table" (table 3))
    (export "__stack_pointer" (global 18))
    (export "__memory_base" (global 19))
    (export "__table_base" (global 20))
    (export "dlopen" (func 12))
  )
  (alias export 0 "foo" (func (;0;)))
  (core func (;13;) (canon lower (func 0)))
  (core instance (;20;)
    (export "foo" (func 13))
  )
  (core instance (;21;) (instantiate 5
      (with "GOT.mem" (instance 17))
      (with "GOT.func" (instance 18))
      (with "env" (instance 19))
      (with "test:test/test" (instance 20))
    )
  )
  (alias core export 0 "$imports" (core table (;4;)))
  (alias core export 3 "fd_write" (core func (;14;)))
  (core instance (;22;)
    (export "$imports" (table 4))
    (export "0" (func 14))
  )
  (core instance (;23;) (instantiate 8
      (with "" (instance 22))
    )
  )
  (core instance (;24;) (instantiate 6
      (with "env" (instance 2))
      (with "foo" (instance 21))
      (with "libc.so" (instance 7))
      (with "libdl.so" (instance 16))
      (with "wit-component:stubs" (instance 11))
    )
  )
  (type (;1;) (func (param "v" s32) (result s32)))
  (alias core export 21 "test:test/test#foo" (core func (;15;)))
  (func (;1;) (type 1) (canon lift (core func 15)))
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
