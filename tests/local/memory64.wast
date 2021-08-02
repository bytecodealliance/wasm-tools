(module
  (memory i64 1)

  (func (result i64) memory.size)
  (func (param i64) (result i64) local.get 0 memory.grow)

  (func
    i64.const 0 i64.const 0 i64.const 0 memory.copy
    i64.const 0 i32.const 0 i64.const 0 memory.fill
    i64.const 0 i32.const 0 i32.const 0 memory.init $seg

    memory.size i32.load drop
    i64.const 0 memory.grow i64.load drop

    i64.const 0 i32.load offset=0xffffffffffff drop

    i64.const 0 i32.load drop
    i64.const 0 i64.load drop
    i64.const 0 f32.load drop
    i64.const 0 f64.load drop
    i64.const 0 i32.load8_s drop
    i64.const 0 i32.load8_u drop
    i64.const 0 i32.load16_s drop
    i64.const 0 i32.load16_u drop
    i64.const 0 i64.load8_s drop
    i64.const 0 i64.load8_u drop
    i64.const 0 i64.load16_s drop
    i64.const 0 i64.load16_u drop
    i64.const 0 i64.load32_s drop
    i64.const 0 i64.load32_u drop
    i64.const 0 i32.const 0 i32.store
    i64.const 0 i64.const 0 i64.store
    i64.const 0 f32.const 0 f32.store
    i64.const 0 f64.const 0 f64.store
    i64.const 0 i32.const 0 i32.store8
    i64.const 0 i32.const 0 i32.store16
    i64.const 0 i64.const 0 i64.store8
    i64.const 0 i64.const 0 i64.store16
    i64.const 0 i64.const 0 i64.store32

    i64.const 0 i32.const 0 memory.atomic.notify drop
    i64.const 0 i32.const 0 i64.const 0 memory.atomic.wait32 drop
    i64.const 0 i64.const 0 i64.const 0 memory.atomic.wait64 drop

    i64.const 0 i32.atomic.load drop
    i64.const 0 i64.atomic.load drop
    i64.const 0 i32.atomic.load8_u drop
    i64.const 0 i32.atomic.load16_u drop
    i64.const 0 i64.atomic.load8_u drop
    i64.const 0 i64.atomic.load16_u drop
    i64.const 0 i64.atomic.load32_u drop
    i64.const 0 i32.const 0 i32.atomic.store
    i64.const 0 i64.const 0 i64.atomic.store
    i64.const 0 i32.const 0 i32.atomic.store8
    i64.const 0 i32.const 0 i32.atomic.store16
    i64.const 0 i64.const 0 i64.atomic.store8
    i64.const 0 i64.const 0 i64.atomic.store16
    i64.const 0 i64.const 0 i64.atomic.store32
    i64.const 0 i32.const 0 i32.atomic.rmw.add drop
    i64.const 0 i64.const 0 i64.atomic.rmw.add drop
    i64.const 0 i32.const 0 i32.atomic.rmw8.add_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw16.add_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw8.add_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw16.add_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw32.add_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw.sub drop
    i64.const 0 i64.const 0 i64.atomic.rmw.sub drop
    i64.const 0 i32.const 0 i32.atomic.rmw8.sub_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw16.sub_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw8.sub_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw16.sub_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw32.sub_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw.and drop
    i64.const 0 i64.const 0 i64.atomic.rmw.and drop
    i64.const 0 i32.const 0 i32.atomic.rmw8.and_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw16.and_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw8.and_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw16.and_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw32.and_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw.or drop
    i64.const 0 i64.const 0 i64.atomic.rmw.or drop
    i64.const 0 i32.const 0 i32.atomic.rmw8.or_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw16.or_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw8.or_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw16.or_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw32.or_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw.xor drop
    i64.const 0 i64.const 0 i64.atomic.rmw.xor drop
    i64.const 0 i32.const 0 i32.atomic.rmw8.xor_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw16.xor_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw8.xor_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw16.xor_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw32.xor_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw.xchg drop
    i64.const 0 i64.const 0 i64.atomic.rmw.xchg drop
    i64.const 0 i32.const 0 i32.atomic.rmw8.xchg_u drop
    i64.const 0 i32.const 0 i32.atomic.rmw16.xchg_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw8.xchg_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw16.xchg_u drop
    i64.const 0 i64.const 0 i64.atomic.rmw32.xchg_u drop
    i64.const 0 i32.const 0 i32.const 0 i32.atomic.rmw.cmpxchg drop
    i64.const 0 i64.const 0 i64.const 0 i64.atomic.rmw.cmpxchg drop
    i64.const 0 i32.const 0 i32.const 0 i32.atomic.rmw8.cmpxchg_u drop
    i64.const 0 i32.const 0 i32.const 0 i32.atomic.rmw16.cmpxchg_u drop
    i64.const 0 i64.const 0 i64.const 0 i64.atomic.rmw8.cmpxchg_u drop
    i64.const 0 i64.const 0 i64.const 0 i64.atomic.rmw16.cmpxchg_u drop
    i64.const 0 i64.const 0 i64.const 0 i64.atomic.rmw32.cmpxchg_u drop

    i64.const 0 v128.load drop
    i64.const 0 v128.load8x8_s drop
    i64.const 0 v128.load8x8_u drop
    i64.const 0 v128.load16x4_s drop
    i64.const 0 v128.load16x4_u drop
    i64.const 0 v128.load32x2_s drop
    i64.const 0 v128.load32x2_u drop
    i64.const 0 v128.load8_splat drop
    i64.const 0 v128.load16_splat drop
    i64.const 0 v128.load32_splat drop
    i64.const 0 v128.load64_splat drop
    i64.const 0 i32.const 0 i8x16.splat v128.store
  )

  (data (i64.const 0) "..")
  (data $seg "..")
)

(assert_invalid
  (module (memory i64 1) (data (i32.const 0) ".."))
  "type mismatch")
(assert_invalid
  (module (memory 1) (data (i64.const 0) ".."))
  "type mismatch")
