(assert_invalid
  (module (func select (result v128)))
  "SIMD support is not enabled")
