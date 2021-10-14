;; --enable-simd --enable-relaxed-simd
(module
  (func $swizzle (param v128 v128) (result v128)
    local.get 0
    local.get 1
    i8x16.swizzle_relaxed)

  (func $i32x4_trunc_f32x4_s (param v128) (result v128)
    local.get 0
    i32x4.trunc_f32x4_s_relaxed)

  (func $i32x4_trunc_f32x4_u (param v128) (result v128)
    local.get 0
    i32x4.trunc_f32x4_u_relaxed)

  (func $i32x4.trunc_f64x2_s_zero (param v128) (result v128)
    local.get 0
    i32x4.trunc_f64x2_s_zero_relaxed)

  (func $i32x4.trunc_f64x2_u_zero (param v128) (result v128)
    local.get 0
    i32x4.trunc_f64x2_u_zero_relaxed)

  (func $f32x4_fma (param v128 v128) (result v128)
    local.get 0
    local.get 1
    f32x4.fma_relaxed)

  (func $f32x4_fms (param v128 v128) (result v128)
    local.get 0
    local.get 1
    f32x4.fms_relaxed)

  (func $f64x2_fma (param v128 v128) (result v128)
    local.get 0
    local.get 1
    f64x2.fma_relaxed)

  (func $f64x2_fms (param v128 v128) (result v128)
    local.get 0
    local.get 1
    f64x2.fms_relaxed)

  (func $i8x16_laneselect (param v128 v128 v128) (result v128)
    local.get 0
    local.get 1
    local.get 2
    i8x16.laneselect)

  (func $i16x8_laneselect (param v128 v128 v128) (result v128)
    local.get 0
    local.get 1
    local.get 2
    i16x8.laneselect)

  (func $i32x4_laneselect (param v128 v128 v128) (result v128)
    local.get 0
    local.get 1
    local.get 2
    i32x4.laneselect)

  (func $i64x2_laneselect (param v128 v128 v128) (result v128)
    local.get 0
    local.get 1
    local.get 2
    i64x2.laneselect)

  (func $f32x4_min (param v128 v128) (result v128)
    local.get 0
    local.get 1
    f32x4.min_relaxed)

  (func $f32x4_max (param v128 v128) (result v128)
    local.get 0
    local.get 1
    f32x4.max_relaxed)

  (func $f64x2_min (param v128 v128) (result v128)
    local.get 0
    local.get 1
    f64x2.min_relaxed)

  (func $f64x2_max (param v128 v128) (result v128)
    local.get 0
    local.get 1
    f64x2.max_relaxed)
)
