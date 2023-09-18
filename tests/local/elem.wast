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
(assert_invalid
  (module
    (table 1 funcref)
    (elem (i32.const 0) funcref (if))
  )
  "constant expression required"
)
(assert_malformed
  (module quote
    "(table 1 funcref)"
    "(elem (i32.const 0 i32.const 0 i32.add) funcref)"
  )
  "folded instruction"
)
