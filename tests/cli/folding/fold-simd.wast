;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-simd.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (memory 1)
  (func
    (local v128)
    i32.const 0 v128.load drop
    i32.const 0 v128.load8x8_s drop
    i32.const 0 v128.load8x8_u drop
    i32.const 0 v128.load16x4_s drop
    i32.const 0 v128.load16x4_u drop
    i32.const 0 v128.load32x2_s drop
    i32.const 0 v128.load32x2_u drop
    i32.const 0 v128.load8_splat drop
    i32.const 0 v128.load16_splat drop
    i32.const 0 v128.load32_splat drop
    i32.const 0 v128.load64_splat drop
    i32.const 0 local.get 0 v128.store
    v128.const i32x4 0 0 0 0 drop

    i32.const 0 local.get 0 v128.load8_lane 0 drop
    i32.const 0 local.get 0 v128.load16_lane 0 drop
    i32.const 0 local.get 0 v128.load32_lane 0 drop
    i32.const 0 local.get 0 v128.load64_lane 0 drop
    i32.const 0 local.get 0 v128.store8_lane 0
    i32.const 0 local.get 0 v128.store16_lane 0
    i32.const 0 local.get 0 v128.store32_lane 0
    i32.const 0 local.get 0 v128.store64_lane 0

    local.get 0 local.get 0 i8x16.shuffle 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 drop
    local.get 0 local.get 0 i8x16.swizzle drop

    i32.const 0 i8x16.splat drop
    i32.const 0 i16x8.splat drop
    i32.const 0 i32x4.splat drop
    i64.const 0 i64x2.splat drop
    f32.const 0 f32x4.splat drop
    f64.const 0 f64x2.splat drop
    local.get 0 i8x16.extract_lane_s 0 drop
    local.get 0 i8x16.extract_lane_u 0 drop
    local.get 0 i32.const 0 i8x16.replace_lane 0 drop
    local.get 0 i16x8.extract_lane_s 0 drop
    local.get 0 i16x8.extract_lane_u 0 drop
    local.get 0 i32.const 0 i16x8.replace_lane 0 drop
    local.get 0 i32x4.extract_lane 0 drop
    local.get 0 i32.const 0 i32x4.replace_lane 0 drop
    local.get 0 i64x2.extract_lane 0 drop
    local.get 0 i64.const 0 i64x2.replace_lane 0 drop
    local.get 0 f32x4.extract_lane 0 drop
    local.get 0 f32.const 0 f32x4.replace_lane 0 drop
    local.get 0 f64x2.extract_lane 0 drop
    local.get 0 f64.const 0 f64x2.replace_lane 0 drop

    local.get 0 local.get 0 i8x16.eq drop
    local.get 0 local.get 0 i8x16.ne drop
    local.get 0 local.get 0 i8x16.lt_s drop
    local.get 0 local.get 0 i8x16.lt_u drop
    local.get 0 local.get 0 i8x16.gt_s drop
    local.get 0 local.get 0 i8x16.gt_u drop
    local.get 0 local.get 0 i8x16.le_s drop
    local.get 0 local.get 0 i8x16.le_u drop
    local.get 0 local.get 0 i8x16.ge_s drop
    local.get 0 local.get 0 i8x16.ge_u drop
    local.get 0 local.get 0 i16x8.eq drop
    local.get 0 local.get 0 i16x8.ne drop
    local.get 0 local.get 0 i16x8.lt_s drop
    local.get 0 local.get 0 i16x8.lt_u drop
    local.get 0 local.get 0 i16x8.gt_s drop
    local.get 0 local.get 0 i16x8.gt_u drop
    local.get 0 local.get 0 i16x8.le_s drop
    local.get 0 local.get 0 i16x8.le_u drop
    local.get 0 local.get 0 i16x8.ge_s drop
    local.get 0 local.get 0 i16x8.ge_u drop
    local.get 0 local.get 0 i32x4.eq drop
    local.get 0 local.get 0 i32x4.ne drop
    local.get 0 local.get 0 i32x4.lt_s drop
    local.get 0 local.get 0 i32x4.lt_u drop
    local.get 0 local.get 0 i32x4.gt_s drop
    local.get 0 local.get 0 i32x4.gt_u drop
    local.get 0 local.get 0 i32x4.le_s drop
    local.get 0 local.get 0 i32x4.le_u drop
    local.get 0 local.get 0 i32x4.ge_s drop
    local.get 0 local.get 0 i32x4.ge_u drop
    local.get 0 local.get 0 f32x4.eq drop
    local.get 0 local.get 0 f32x4.ne drop
    local.get 0 local.get 0 f32x4.lt drop
    local.get 0 local.get 0 f32x4.gt drop
    local.get 0 local.get 0 f32x4.le drop
    local.get 0 local.get 0 f32x4.ge drop
    local.get 0 local.get 0 f64x2.eq drop
    local.get 0 local.get 0 f64x2.ne drop
    local.get 0 local.get 0 f64x2.lt drop
    local.get 0 local.get 0 f64x2.gt drop
    local.get 0 local.get 0 f64x2.le drop
    local.get 0 local.get 0 f64x2.ge drop

    local.get 0 v128.not drop
    local.get 0 local.get 0 v128.and drop
    local.get 0 local.get 0 v128.andnot drop
    local.get 0 local.get 0 v128.or drop
    local.get 0 local.get 0 v128.xor drop
    local.get 0 local.get 0 local.get 0 v128.bitselect drop
    local.get 0 v128.any_true drop

    local.get 0 i8x16.abs drop
    local.get 0 i8x16.neg drop
    local.get 0 i8x16.all_true drop
    local.get 0 local.get 0 i8x16.narrow_i16x8_s drop
    local.get 0 local.get 0 i8x16.narrow_i16x8_u drop
    local.get 0 i32.const 0 i8x16.shl drop
    local.get 0 i32.const 0 i8x16.shr_s drop
    local.get 0 i32.const 0 i8x16.shr_u drop
    local.get 0 local.get 0 i8x16.add drop
    local.get 0 local.get 0 i8x16.add_sat_s drop
    local.get 0 local.get 0 i8x16.add_sat_u drop
    local.get 0 local.get 0 i8x16.sub drop
    local.get 0 local.get 0 i8x16.sub_sat_s drop
    local.get 0 local.get 0 i8x16.sub_sat_u drop
    local.get 0 local.get 0 i8x16.min_s drop
    local.get 0 local.get 0 i8x16.min_u drop
    local.get 0 local.get 0 i8x16.max_s drop
    local.get 0 local.get 0 i8x16.max_u drop
    local.get 0 local.get 0 i8x16.avgr_u drop

    local.get 0 i16x8.abs drop
    local.get 0 i16x8.neg drop
    local.get 0 i16x8.all_true drop
    local.get 0 local.get 0 i16x8.narrow_i32x4_s drop
    local.get 0 local.get 0 i16x8.narrow_i32x4_u drop
    local.get 0 i16x8.extend_low_i8x16_s drop
    local.get 0 i16x8.extend_high_i8x16_s drop
    local.get 0 i16x8.extend_low_i8x16_u drop
    local.get 0 i16x8.extend_high_i8x16_u drop
    local.get 0 i32.const 0 i16x8.shl drop
    local.get 0 i32.const 0 i16x8.shr_s drop
    local.get 0 i32.const 0 i16x8.shr_u drop
    local.get 0 local.get 0 i16x8.add drop
    local.get 0 local.get 0 i16x8.add_sat_s drop
    local.get 0 local.get 0 i16x8.add_sat_u drop
    local.get 0 local.get 0 i16x8.sub drop
    local.get 0 local.get 0 i16x8.sub_sat_s drop
    local.get 0 local.get 0 i16x8.sub_sat_u drop
    local.get 0 local.get 0 i16x8.mul drop
    local.get 0 local.get 0 i16x8.min_s drop
    local.get 0 local.get 0 i16x8.min_u drop
    local.get 0 local.get 0 i16x8.max_s drop
    local.get 0 local.get 0 i16x8.max_u drop
    local.get 0 local.get 0 i16x8.avgr_u drop

    local.get 0 i32x4.abs drop
    local.get 0 i32x4.neg drop
    local.get 0 i32x4.all_true drop
    local.get 0 i32x4.extend_low_i16x8_s drop
    local.get 0 i32x4.extend_high_i16x8_s drop
    local.get 0 i32x4.extend_low_i16x8_u drop
    local.get 0 i32x4.extend_high_i16x8_u drop
    local.get 0 i32.const 0 i32x4.shl drop
    local.get 0 i32.const 0 i32x4.shr_s drop
    local.get 0 i32.const 0 i32x4.shr_u drop
    local.get 0 local.get 0 i32x4.add drop
    local.get 0 local.get 0 i32x4.sub drop
    local.get 0 local.get 0 i32x4.mul drop
    local.get 0 local.get 0 i32x4.min_s drop
    local.get 0 local.get 0 i32x4.min_u drop
    local.get 0 local.get 0 i32x4.max_s drop
    local.get 0 local.get 0 i32x4.max_u drop

    local.get 0 i64x2.neg drop
    local.get 0 i32.const 0 i64x2.shl drop
    local.get 0 i32.const 0 i64x2.shr_s drop
    local.get 0 i32.const 0 i64x2.shr_u drop
    local.get 0 local.get 0 i64x2.add drop
    local.get 0 local.get 0 i64x2.sub drop
    local.get 0 local.get 0 i64x2.mul drop

    local.get 0 f32x4.abs drop
    local.get 0 f32x4.neg drop
    local.get 0 f32x4.sqrt drop
    local.get 0 local.get 0 f32x4.add drop
    local.get 0 local.get 0 f32x4.sub drop
    local.get 0 local.get 0 f32x4.mul drop
    local.get 0 local.get 0 f32x4.div drop
    local.get 0 local.get 0 f32x4.min drop
    local.get 0 local.get 0 f32x4.max drop

    local.get 0 f64x2.abs drop
    local.get 0 f64x2.neg drop
    local.get 0 f64x2.sqrt drop
    local.get 0 local.get 0 f64x2.add drop
    local.get 0 local.get 0 f64x2.sub drop
    local.get 0 local.get 0 f64x2.mul drop
    local.get 0 local.get 0 f64x2.div drop
    local.get 0 local.get 0 f64x2.min drop
    local.get 0 local.get 0 f64x2.max drop

    local.get 0 i32x4.trunc_sat_f32x4_s drop
    local.get 0 i32x4.trunc_sat_f32x4_u drop
    local.get 0 f32x4.convert_i32x4_s drop
    local.get 0 f32x4.convert_i32x4_u drop
  )
)
