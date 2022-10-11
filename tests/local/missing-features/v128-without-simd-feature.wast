(assert_invalid
  (module (func (param v128)))
  "SIMD support is not enabled")
(assert_invalid
  (module (type (func (param v128))))
  "SIMD support is not enabled")
(assert_invalid
  (module (type (func (result v128))))
  "SIMD support is not enabled")
(assert_invalid
  (module (import "" "" (global v128)))
  "SIMD support is not enabled")
(assert_invalid
  (module (import "" "" (global (mut v128))))
  "SIMD support is not enabled")
(assert_invalid
  (module (global v128 v128.const i64x2 0 0))
  "SIMD support is not enabled")
(assert_invalid
  (module (func block (result v128) end))
  "SIMD support is not enabled")
