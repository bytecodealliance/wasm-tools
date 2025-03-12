;; RUN: wast --assert default --snapshot tests/snapshots % -f mvp,reference-types

(assert_invalid
  (module (func select (result v128)))
  "SIMD support is not enabled")
