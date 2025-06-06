;; RUN: wast --assert default --snapshot tests/snapshots %

(module
  (table 1 funcref)
  (elem (offset i32.const 0) funcref)
  (elem (i32.const 0) funcref)
  (elem (offset i32.const 0 i32.const 0 i32.add) funcref)
  (elem (offset (i32.add (i32.const 0) (i32.const 0))) funcref)
  (elem (i32.add (i32.const 0) (i32.const 0)) funcref)
)

(assert_invalid
  (module
    (table 1 funcref)
    (elem (i32.const 0) funcref (ref.null extern))
  )
  "type mismatch"
)
(assert_malformed
  (module
    (table 1 funcref)
    (elem (i32.const 0) funcref (if))
  )
  "control frames remain"
)
(assert_malformed
  (module quote
    "(table 1 funcref)"
    "(elem (i32.const 0 i32.const 0 i32.add) funcref)"
  )
  "folded instruction"
)
