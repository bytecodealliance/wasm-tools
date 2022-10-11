(module
  (memory $a 0)
  (memory $b 0)

  (data $seg "")

  (func $foo
    i32.const 0
    i32.load
    drop

    i32.const 0
    i32.load 0
    drop

    i32.const 0
    i32.load 1
    drop

    i32.const 0
    i32.load $a
    drop

    i32.const 0
    i32.load $b
    drop

    i32.const 0
    i32.const 0
    i32.store 0

    i32.const 0
    i32.const 0
    i32.store 1

    i32.const 0
    i32.const 0
    i32.store $a

    i32.const 0
    i32.const 0
    i32.store $b

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
    i32.const 0 i64.load $b drop
    i32.const 0 f32.load $b drop
    i32.const 0 f64.load $b drop
    i32.const 0 i32.load8_s $b drop
    i32.const 0 i32.load8_u $b drop
    i32.const 0 i32.load16_s $b drop
    i32.const 0 i32.load16_u $b drop
    i32.const 0 i64.load8_s $b drop
    i32.const 0 i64.load8_u $b drop
    i32.const 0 i64.load16_s $b drop
    i32.const 0 i64.load16_u $b drop
    i32.const 0 i64.load32_s $b drop
    i32.const 0 i64.load32_u $b drop
    i32.const 0 i32.const 0 i32.store $b
    i32.const 0 i64.const 0 i64.store $b
    i32.const 0 f32.const 0 f32.store $b
    i32.const 0 f64.const 0 f64.store $b
    i32.const 0 i32.const 0 i32.store8 $b
    i32.const 0 i32.const 0 i32.store16 $b
    i32.const 0 i64.const 0 i64.store8 $b
    i32.const 0 i64.const 0 i64.store16 $b
    i32.const 0 i64.const 0 i64.store32 $b

    i32.const 0
    i32.const 0
    memory.atomic.notify $b
    drop

    i32.const 0
    i32.const 0
    i64.const 0
    memory.atomic.wait32 $b
    drop

    i32.const 0
    i64.const 0
    i64.const 0
    memory.atomic.wait64 $b
    drop

    i32.const 0 i32.atomic.load $b drop
    i32.const 0 i64.atomic.load $b drop
    i32.const 0 i32.atomic.load8_u $b drop
    i32.const 0 i32.atomic.load16_u $b drop
    i32.const 0 i64.atomic.load8_u $b drop
    i32.const 0 i64.atomic.load16_u $b drop
    i32.const 0 i64.atomic.load32_u $b drop
    i32.const 0 i32.const 0 i32.atomic.store $b
    i32.const 0 i64.const 0 i64.atomic.store $b
    i32.const 0 i32.const 0 i32.atomic.store8 $b
    i32.const 0 i32.const 0 i32.atomic.store16 $b
    i32.const 0 i64.const 0 i64.atomic.store8 $b
    i32.const 0 i64.const 0 i64.atomic.store16 $b
    i32.const 0 i64.const 0 i64.atomic.store32 $b
    i32.const 0 i32.const 0 i32.atomic.rmw.add $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw.add $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.add_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.add_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.add_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.add_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.add_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw.sub $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw.sub $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.sub_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.sub_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.sub_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.sub_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.sub_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw.and $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw.and $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.and_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.and_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.and_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.and_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.and_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw.or $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw.or $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.or_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.or_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.or_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.or_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.or_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw.xor $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw.xor $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.xor_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.xor_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.xor_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.xor_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.xor_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw.xchg $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw.xchg $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw8.xchg_u $b drop
    i32.const 0 i32.const 0 i32.atomic.rmw16.xchg_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw8.xchg_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw16.xchg_u $b drop
    i32.const 0 i64.const 0 i64.atomic.rmw32.xchg_u $b drop
    i32.const 0 i32.const 0 i32.const 0 i32.atomic.rmw.cmpxchg $b drop
    i32.const 0 i64.const 0 i64.const 0 i64.atomic.rmw.cmpxchg $b drop
    i32.const 0 i32.const 0 i32.const 0 i32.atomic.rmw8.cmpxchg_u $b drop
    i32.const 0 i32.const 0 i32.const 0 i32.atomic.rmw16.cmpxchg_u $b drop
    i32.const 0 i64.const 0 i64.const 0 i64.atomic.rmw8.cmpxchg_u $b drop
    i32.const 0 i64.const 0 i64.const 0 i64.atomic.rmw16.cmpxchg_u $b drop
    i32.const 0 i64.const 0 i64.const 0 i64.atomic.rmw32.cmpxchg_u $b drop

    i32.const 0 v128.load $b drop
    i32.const 0 v128.load8x8_s $b drop
    i32.const 0 v128.load8x8_u $b drop
    i32.const 0 v128.load16x4_s $b drop
    i32.const 0 v128.load16x4_u $b drop
    i32.const 0 v128.load32x2_s $b drop
    i32.const 0 v128.load32x2_u $b drop
    i32.const 0 v128.load8_splat $b drop
    i32.const 0 v128.load16_splat $b drop
    i32.const 0 v128.load32_splat $b drop
    i32.const 0 v128.load64_splat $b drop
    i32.const 0 v128.load32_zero $b drop
    i32.const 0 v128.load64_zero $b drop
    i32.const 0 v128.const i64x2 0 0 v128.load8_lane $b 0 drop
    i32.const 0 v128.const i64x2 0 0 v128.load16_lane $b 0 drop
    i32.const 0 v128.const i64x2 0 0 v128.load32_lane $b 0 drop
    i32.const 0 v128.const i64x2 0 0 v128.load64_lane $b 0 drop
    i32.const 0 v128.const i64x2 0 0 v128.store8_lane $b 0
    i32.const 0 v128.const i64x2 0 0 v128.store16_lane $b 0
    i32.const 0 v128.const i64x2 0 0 v128.store32_lane $b 0
    i32.const 0 v128.const i64x2 0 0 v128.store64_lane $b 0
    i32.const 0 i32.const 0 i8x16.splat v128.store $b
  )
)

(assert_invalid
  (module
    (memory 0)
    (func
      i32.const 0
      i32.load 1
      drop
    )
  )
  "unknown memory 1")

(assert_malformed
  (module quote
    "(func i32.load $a)"
  )
  "unknown memory")

(module
  (memory 1)
  (memory $m 1)
  (data (memory $m) (i32.const 0) "...")
)

