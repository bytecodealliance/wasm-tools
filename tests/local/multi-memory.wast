(module
  (memory $a 0)
  (memory $b 0)

  (data $seg "")

  (func $foo
    i32.const 0
    i32.load
    drop

    i32.const 0
    i32.load (memory 0)
    drop

    i32.const 0
    i32.load (memory 1)
    drop

    i32.const 0
    i32.load (memory $a)
    drop

    i32.const 0
    i32.load (memory $b)
    drop

    i32.const 0
    i32.const 0
    i32.store (memory 0)

    i32.const 0
    i32.const 0
    i32.store (memory 1)

    i32.const 0
    i32.const 0
    i32.store (memory $a)

    i32.const 0
    i32.const 0
    i32.store (memory $b)

    i32.const 0
    i32.const 0
    i32.const 0
    memory.copy $a $b

    i32.const 0
    i32.const 0
    i32.const 0
    memory.copy $b $a

    i32.const 0
    i32.const 0
    i32.const 0
    memory.fill $a

    i32.const 0
    i32.const 0
    i32.const 0
    memory.fill $b

    i32.const 0
    i32.const 0
    i32.const 0
    memory.init $b $seg

    memory.size $a drop
    memory.size $b drop
    i32.const 0 memory.grow $a drop
    i32.const 0 memory.grow $b drop

    i32.const 0 i32.load drop
    i32.const 0 i64.load (memory $b) drop
    i32.const 0 f32.load (memory $b) drop
    i32.const 0 f64.load (memory $b) drop
    i32.const 0 i32.load8_s (memory $b) drop
    i32.const 0 i32.load8_u (memory $b) drop
    i32.const 0 i32.load16_s (memory $b) drop
    i32.const 0 i32.load16_u (memory $b) drop
    i32.const 0 i64.load8_s (memory $b) drop
    i32.const 0 i64.load8_u (memory $b) drop
    i32.const 0 i64.load16_s (memory $b) drop
    i32.const 0 i64.load16_u (memory $b) drop
    i32.const 0 i64.load32_s (memory $b) drop
    i32.const 0 i64.load32_u (memory $b) drop
    i32.const 0 i32.const 0 i32.store (memory $b)
    i32.const 0 i64.const 0 i64.store (memory $b)
    i32.const 0 f32.const 0 f32.store (memory $b)
    i32.const 0 f64.const 0 f64.store (memory $b)
    i32.const 0 i32.const 0 i32.store8 (memory $b)
    i32.const 0 i32.const 0 i32.store16 (memory $b)
    i32.const 0 i64.const 0 i64.store8 (memory $b)
    i32.const 0 i64.const 0 i64.store16 (memory $b)
    i32.const 0 i64.const 0 i64.store32 (memory $b)

    i32.const 0
    i32.const 0
    memory.atomic.notify (memory $b)
    drop

    i32.const 0
    i32.const 0
    i64.const 0
    memory.atomic.wait32 (memory $b)
    drop

    i32.const 0
    i64.const 0
    i64.const 0
    memory.atomic.wait64 (memory $b)
    drop

    i32.const 0 i32.atomic.load (memory $b) drop
    i32.const 0 i64.atomic.load (memory $b) drop
    i32.const 0 i32.atomic.load8_u (memory $b) drop
    i32.const 0 i32.atomic.load16_u (memory $b) drop
    i32.const 0 i64.atomic.load8_u (memory $b) drop
    i32.const 0 i64.atomic.load16_u (memory $b) drop
    i32.const 0 i64.atomic.load32_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.store (memory $b)
    i32.const 0 i64.const 0 i64.atomic.store (memory $b)
    i32.const 0 i32.const 0 i32.atomic.store8 (memory $b)
    i32.const 0 i32.const 0 i32.atomic.store16 (memory $b)
    i32.const 0 i64.const 0 i64.atomic.store8 (memory $b)
    i32.const 0 i64.const 0 i64.atomic.store16 (memory $b)
    i32.const 0 i64.const 0 i64.atomic.store32 (memory $b)
    i32.const 0 i32.const 0 i32.atomic.rmw.add (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw.add (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.add_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.add_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.add_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.add_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.add_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw.sub (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw.sub (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.sub_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.sub_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.sub_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.sub_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.sub_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw.and (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw.and (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.and_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.and_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.and_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.and_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.and_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw.or (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw.or (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.or_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.or_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.or_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.or_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.or_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw.xor (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw.xor (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.xor_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.xor_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.xor_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.xor_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.xor_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw.xchg (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw.xchg (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.xchg_u (memory $b) drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.xchg_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.xchg_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.xchg_u (memory $b) drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.xchg_u (memory $b) drop
    i32.const 0 i32.const 0 i32.const 0 i32.atomic.rmw.cmpxchg (memory $b) drop
    i32.const 0 i64.const 0 i64.const 0 i64.atomic.rmw.cmpxchg (memory $b) drop
    i32.const 0 i32.const 0 i32.const 0 i32.atomic.rmw8.cmpxchg_u (memory $b) drop
    i32.const 0 i32.const 0 i32.const 0 i32.atomic.rmw16.cmpxchg_u (memory $b) drop
    i32.const 0 i64.const 0 i64.const 0 i64.atomic.rmw8.cmpxchg_u (memory $b) drop
    i32.const 0 i64.const 0 i64.const 0 i64.atomic.rmw16.cmpxchg_u (memory $b) drop
    i32.const 0 i64.const 0 i64.const 0 i64.atomic.rmw32.cmpxchg_u (memory $b) drop

    i32.const 0 v128.load (memory $b) drop
    i32.const 0 v128.load8x8_s (memory $b) drop
    i32.const 0 v128.load8x8_u (memory $b) drop
    i32.const 0 v128.load16x4_s (memory $b) drop
    i32.const 0 v128.load16x4_u (memory $b) drop
    i32.const 0 v128.load32x2_s (memory $b) drop
    i32.const 0 v128.load32x2_u (memory $b) drop
    i32.const 0 v128.load8_splat (memory $b) drop
    i32.const 0 v128.load16_splat (memory $b) drop
    i32.const 0 v128.load32_splat (memory $b) drop
    i32.const 0 v128.load64_splat (memory $b) drop
    i32.const 0 v128.load32_zero (memory $b) drop
    i32.const 0 v128.load64_zero (memory $b) drop
    i32.const 0 v128.const i64x2 0 0 v128.load8_lane (memory $b) 0 drop
    i32.const 0 v128.const i64x2 0 0 v128.load16_lane (memory $b) 0 drop
    i32.const 0 v128.const i64x2 0 0 v128.load32_lane (memory $b) 0 drop
    i32.const 0 v128.const i64x2 0 0 v128.load64_lane (memory $b) 0 drop
    i32.const 0 v128.const i64x2 0 0 v128.store8_lane (memory $b) 0
    i32.const 0 v128.const i64x2 0 0 v128.store16_lane (memory $b) 0
    i32.const 0 v128.const i64x2 0 0 v128.store32_lane (memory $b) 0
    i32.const 0 v128.const i64x2 0 0 v128.store64_lane (memory $b) 0
    i32.const 0 i32.const 0 i8x16.splat v128.store (memory $b)
  )
)

(assert_invalid
  (module
    (memory 0)
    (func
      i32.const 0
      i32.load (memory 1)
      drop
    )
  )
  "unknown memory 1")

(assert_malformed
  (module quote
    "(func i32.load (memory $a))"
  )
  "failed to find memory")

(module
  (memory 1)
  (memory $m 1)
  (data (memory $m) (i32.const 0) "...")
)

